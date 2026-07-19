#' List files with minimal network calls
#'
#' @description
#' Lists files and metadata in a single network round trip, using
#' PowerShell's `Get-ChildItem` instead of one `stat()` per file.
#'
#' This is similar to [fs::dir_info()], but not identical.
#'
#' The output includes only the subset of the columns that are available via
#' `Get-ChildItem`: `path`, `type`, `size`, `modification_time`, `access_time`,
#' `birth_time`. Unlike [fs::dir_info()], it will return partial results with
#' a `sharefs_warning_powershell_partial` warning if some items had to be
#' skipped (e.g. a permission-denied subfolder).
#'
#' Automatically includes retry logic if an intermittent error is encountered.
#'
#' @param path A character vector of one or more paths.
#' @param all If `TRUE` hidden files are also returned.
#' @param recurse Whether to recurse into subdirectories. Depth-limited
#'    recursion is not supported.
#' @param type File type(s) to return, one or more of "any", "file",
#'   "directory", "symlink", "FIFO", "socket", "character_device" or
#'   "block_device".
#' @param regexp A regular expression (e.g. `[.]csv$`) passed on to
#'   [grep()] to filter paths.
#' @param glob A wildcard aka globbing pattern (e.g. `*.csv`) passed on
#'   to [grep()] to filter paths.
#' @param invert If `TRUE` return files which do *not* match
#' @param fail Should the call fail (the default) or warn if a file
#'   cannot be accessed.
#' @param ... Passed to [grepl()] (e.g. `ignore.case = TRUE`).
#'
#' @return A tibble with columns `path`, `type`, `size`,
#'   `modification_time`, `access_time`, `birth_time`.
#'
#' @seealso [sfs_powershell_available()] which checks if Powershell is
#'    available and accessible.
#'
#' @export
#'
#' @examples
#' if (sfs_powershell_available()) {
#'   sfs_dir_info(system.file(package = "sharefs"))
#' }
sfs_dir_info <- function(path = ".", all = FALSE, recurse = FALSE, type = "any",
                         regexp = NULL, glob = NULL, invert = FALSE,
                         fail = TRUE, ...) {
  # Argument validation
  # unique() first, fs::as_fs_path() after: unique() doesn't preserve
  # fs_path's class, so converting first would lose it.
  path <- fs::as_fs_path(unique(path))
  type <- validate_dir_info_type(type)
  validate_regexp_glob_exclusive(regexp, glob)

  exists_mask <- fs::dir_exists(path)
  missing_paths <- path[!exists_mask]

  if (length(missing_paths) > 0) {
    if (fail) {
      cli::cli_abort(
        c(
          "{.arg path} must contain only existing directories.",
          "x" = "Not found: {.path {missing_paths}}"
        ),
        class = "sharefs_error_path_not_found"
      )
    }
    cli::cli_warn(
      c(
        "Skipping {.arg path} entries that don't exist.",
        "x" = "Not found: {.path {missing_paths}}"
      ),
      class = "sharefs_warning_path_not_found"
    )
    # Logical indexing, not setdiff(path, missing_paths): setdiff()
    # strips fs_path's class (and its slash formatting/printing).
    path <- path[exists_mask]
  }

  if (length(path) == 0) {
    return(empty_dir_info())
  }

  if (!sfs_powershell_available()) {
    cli::cli_abort(
      c(
        "PowerShell is not available on this device.",
        "i" = "Use {.fn fs::dir_info} instead -- it accepts the same
					{.arg type}/{.arg regexp}/{.arg glob}/{.arg invert}
					filtering natively, with a superset of this function's
					columns.",
        "i" = "If you've just made PowerShell available (e.g. an
					AppLocker/WDAC exception), just try again --
					{.fn sfs_powershell_available} always rechecks after a
					{.val FALSE} result."
      ),
      class = "sharefs_error_powershell_unavailable"
    )
  }

  info <- retry_on_error(
    function() dir_info_powershell(path, all = all, recurse = recurse),
    retries = 5,
    retryable = function(e) !is_permission_error(e)
  )

  filter_dir_info(info, type = type, regexp = regexp, glob = glob, invert = invert, ...)
}


#' List file paths with minimal network calls
#'
#' @description
#' `sfs_dir_info(...)$path`. No extra cost to fetching full metadata even
#' when only paths are needed, since `Get-ChildItem` returns it either
#' way.
#'
#' @inheritParams sfs_dir_info
#'
#' @return A named `fs::fs_path` character vector.
#' @export
#'
#' @examples
#' if (sfs_powershell_available()) {
#'   sfs_dir_ls(system.file(package = "sharefs"))
#' }
sfs_dir_ls <- function(path = ".", all = FALSE, recurse = FALSE, type = "any",
                       regexp = NULL, glob = NULL, invert = FALSE,
                       fail = TRUE, ...) {
  paths <- sfs_dir_info(
    path = path, all = all, recurse = recurse, type = type,
    regexp = regexp, glob = glob, invert = invert, fail = fail, ...
  )$path

  stats::setNames(paths, paths)
}
