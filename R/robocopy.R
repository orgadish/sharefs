#' Check whether `robocopy` is available
#'
#' @description
#' Checks whether `robocopy` is on `PATH`. Unlike
#' [sfs_powershell_available()], does not test that it can be run.
#'
#' @return A single logical value.
#' @export
#'
#' @examples
#' sfs_robocopy_available()
sfs_robocopy_available <- function() {
   nzchar(find_robocopy())
}

find_robocopy <- function() {
   Sys.which("robocopy")
}

#' Run `robocopy` to copy files from one directory to another
#'
#' @description
#' Maps R arguments to `robocopy` flags, runs it, and interprets the
#' exit code (0-7 = success, 8+ = failure, even after retries). Exposes
#' the flags people reach for most often as named arguments; anything
#' else can be passed via `...`, e.g. `sfs_robocopy(src, dst, "/SEC")`. See
#' `robocopy /?` for the full flag reference.
#'
#' @param source,destination Source and destination directory paths.
#' @param ... Additional raw `robocopy` flags (e.g. `"/SEC"`,
#'   `"/MAXAGE:30"`), appended after the named ones below. Placed right
#'   after `destination` so flags can be passed positionally; everything
#'   from `files` onward must be named as a result.
#' @param files File names (not full paths) to copy from `source`. If
#'   `NULL` (default), copies every file directly in `source`. An empty
#'   vector copies nothing.
#' @param recurse Copy subdirectories, including empty ones (`/E`).
#'   Ignored if `mirror = TRUE`.
#' @param mirror Mirror `source` to `destination` (`/MIR`): also deletes
#'   files in `destination` that no longer exist in `source`.
#'   **Destructive.**
#' @param move Move rather than copy (`/MOVE`): deletes files from
#'   `source` after copying. **Destructive.**
#' @param exclude_files,exclude_dirs File/directory name patterns to
#'   exclude (`/XF`, `/XD`), wildcards allowed.
#' @param dry_run List what would happen without doing it (`/L`).
#' @param log_file Path to write `robocopy`'s log to (`/LOG:`). Unlike
#'   the default (fully suppressed) console output, a requested log
#'   includes the file list, directory list, and job header/summary --
#'   only the per-file progress percentage is always left out.
#' @param threads Thread count for `/MT`. Default `8`.
#' @param retries,wait_seconds `/R` and `/W`: retry count and wait
#'   between retries. Defaults (`5`, `2`) are more patient than
#'   `robocopy`'s own (a million retries at 30s apart).
#' @param error_on_failure If `TRUE` (default), aborts on failure. If
#'   `FALSE`, returns the result either way.
#' @param timeout Maximum time in seconds to let `robocopy` run before
#'   killing it. Default `Inf` (no limit), since a copy can legitimately
#'   take a long time.
#'
#' @return Invisibly, a list with `status` (the exit code) and `success`
#'   (`status < 8`).
#' @seealso [sfs_robocopy_available()], which checks if robocopy is available
#'   on this system.
#'
#' @export
#'
#' @examples
#' if (sfs_robocopy_available() && sfs_powershell_available()) {
#'   src <- tempfile()
#'   dir.create(src)
#'
#'   write.csv(data.frame(x = 1:2), fs::path(src, "a.csv"), row.names = FALSE)
#'
#'   dest <- tempfile()
#'   sfs_robocopy(src, dest)
#'   sfs_dir_info(dest)
#'
#'   fs::dir_delete(c(src, dest))
#' }
sfs_robocopy <- function(source, destination, ...,
                         files = NULL,
                         recurse = FALSE, mirror = FALSE, move = FALSE,
                         exclude_files = NULL, exclude_dirs = NULL,
                         dry_run = FALSE, log_file = NULL,
                         threads = 8, retries = 5, wait_seconds = 2,
                         error_on_failure = TRUE, timeout = Inf) {
   if (length(source) != 1 || length(destination) != 1) {
      cli::cli_abort(
         "{.arg source} and {.arg destination} must each be a single path.",
         class = "sharefs_error_robocopy_bad_args"
      )
   }
   
   exe <- find_robocopy()
   if (!nzchar(exe)) {
      cli::cli_abort(
         c(
            "{.code robocopy} was not found on the {.envvar PATH}.",
            "i" = "It ships with Windows by default; if it's genuinely
					missing, this is likely a minimal or locked-down install."
         ),
         class = "sharefs_error_robocopy_unavailable"
      )
   }
   
   # An explicit, empty file list means "copy nothing" -- robocopy itself
   # has no such concept (an absent file filter just means "everything").
   if (!is.null(files) && length(files) == 0) {
      return(invisible(list(status = 0L, success = TRUE)))
   }
   
   # /NFL /NDL /NJH /NJS suppress exactly the content a log is meant to
   # capture. They're only skipped when log_file is NULL, since robocopy's
   # own console output is discarded either way in that case. /NP
   # (per-file progress percentage) is pure noise even in a saved log, so
   # it's always suppressed.
   quiet_flags <- if (is.null(log_file)) {
      c("/NFL", "/NDL", "/NJH", "/NJS", "/NP")
   } else {
      "/NP"
   }
   
   args <- c(
      source,
      destination,
      files,
      if (mirror) "/MIR" else if (recurse) "/E",
      if (move) "/MOVE",
      # length(x) > 0, not !is.null(x): an empty (but non-NULL) vector
      # would otherwise still add a bare /XF or /XD with no pattern after
      # it, which robocopy could misparse as consuming whatever flag
      # happens to follow as an exclude pattern instead.
      if (length(exclude_files) > 0) c("/XF", exclude_files),
      if (length(exclude_dirs) > 0) c("/XD", exclude_dirs),
      if (dry_run) "/L",
      if (!is.null(log_file)) paste0("/LOG:", log_file),
      paste0("/MT:", threads),
      paste0("/R:", retries),
      paste0("/W:", wait_seconds),
      quiet_flags,
      ...
   )
   
   res <- tryCatch(
      run_process(
         command = exe,
         args = args,
         timeout = timeout
      ),
      error = function(e) {
         spawn_error <- e$message
         cli::cli_abort(
            c(
               "The background robocopy process failed to start or timed out.",
               "x" = "{spawn_error}"
            ),
            class = "sharefs_error_robocopy_execution_failed"
         )
      }
   )
   
   status <- as.integer(res$status)
   if (is.na(status)) {
      status <- 16L # missing exit status treated as failure
   }
   
   result <- list(status = status, success = status < 8)
   
   if (error_on_failure && !result$success) {
      cli::cli_abort(
         c(
            "{.code robocopy} failed copying from {.path {source}} to
			{.path {destination}}, even after retrying.",
            "i" = "Exit code: {status}"
         ),
         class = "sharefs_error_robocopy_failed"
      )
   }
   
   invisible(result)
}
