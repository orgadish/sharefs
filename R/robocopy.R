#' Run `robocopy` to copy files from one directory to another
#'
#' @description
#' Maps R arguments to `robocopy` flags, runs it, and interprets the
#' exit code (0-7 = success, 8+ = failure, even after retries). Exposes
#' the flags people reach for most often as named arguments; anything
#' else can be passed via `...`, e.g. `robocopy(src, dst, "/SEC")`. See
#' `robocopy /?` for the full flag reference.
#'
#' @param source,destination Source and destination directory paths.
#' @param ... Additional raw `robocopy` flags (e.g. `"/SEC"`,
#'   `"/MAXAGE:30"`), appended after the named ones below. Placed right
#'   after `destination` so flags can be passed positionally; everything
#'   from `files` onward must be named as a result.
#' @param files File names (not full paths) to copy from `source`. If
#'   `NULL` (default), copies every file directly in `source`.
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
#' @param log_file Path to write `robocopy`'s log to (`/LOG:`).
#' @param threads Thread count for `/MT`. Default `8`.
#' @param retries,wait_seconds `/R` and `/W`: retry count and wait
#'   between retries. Defaults (`5`, `2`) are more patient than
#'   `robocopy`'s own (a million retries at 30s apart).
#' @param error_on_failure If `TRUE` (default), aborts on failure. If
#'   `FALSE`, returns the result either way.
#'
#' @return Invisibly, a list with `status` (the exit code) and `success`
#'   (`status < 8`).
#' @export
robocopy <- function(source, destination, ...,
                      files = NULL,
                      recurse = FALSE, mirror = FALSE, move = FALSE,
                      exclude_files = NULL, exclude_dirs = NULL,
                      dry_run = FALSE, log_file = NULL,
                      threads = 8, retries = 5, wait_seconds = 2,
                      error_on_failure = TRUE) {
  if (!robocopy_available()) {
    cli::cli_abort(
      c(
        "{.code robocopy} was not found on the {.envvar PATH}.",
        "i" = "It ships with Windows by default; if it's genuinely
               missing, this is likely a minimal or locked-down install."
      ),
      class = "sharefs_error_robocopy_unavailable"
    )
  }

  args <- c(
    shQuote(source),
    shQuote(destination),
    if (!is.null(files)) shQuote(files),
    if (mirror) "/MIR" else if (recurse) "/E",
    if (move) "/MOVE",
    if (!is.null(exclude_files)) c("/XF", shQuote(exclude_files)),
    if (!is.null(exclude_dirs)) c("/XD", shQuote(exclude_dirs)),
    if (dry_run) "/L",
    if (!is.null(log_file)) paste0("/LOG:", shQuote(log_file)),
    paste0("/MT:", threads),
    paste0("/R:", retries),
    paste0("/W:", wait_seconds),
    "/NFL", "/NDL", "/NJH", "/NJS", "/NP",
    ...
  )

  status <- suppressWarnings(
    system2("robocopy", args, stdout = FALSE, stderr = FALSE)
  )
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
