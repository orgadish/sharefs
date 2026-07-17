#' List files with minimal network calls
#'
#' @description
#' Lists files and metadata in a single network round trip, using
#' PowerShell's `Get-ChildItem` instead of one `stat()` per file.
#'
#' Arguments mirror [fs::dir_info()]. The result has fewer columns,
#' though: `path`, `type`, `size`, `modification_time`, `access_time`,
#' `birth_time`. The rest of `fs::dir_info()`'s 18 columns aren't
#' available via `Get-ChildItem` the same way, so they're left out
#' rather than filled in with placeholders -- call [fs::dir_info()]
#' directly if you need them.
#'
#' Errors if PowerShell isn't available (see
#' [sfs_powershell_available()]), or if a listing attempt still fails
#' after retrying -- there's no fallback to [fs::dir_info()]. Call that
#' directly if you want its behavior instead; it accepts the same
#' `type`/`regexp`/`glob`/`invert` filtering natively.
#'
#' @param path A directory path, or a character vector of several.
#' @param all Include hidden files. Default `FALSE`.
#' @param recurse Recurse into subdirectories. Default `FALSE`.
#' @param type One or more of `"any"` (default), `"file"`, `"directory"`,
#'   `"symlink"`. Other `fs` types are accepted but never match anything
#'   on NTFS/SMB.
#' @param regexp Regular expression to filter by, matched against the
#'   full path. Only one of `regexp`/`glob` may be supplied.
#' @param glob Wildcard pattern (e.g. `"*.csv"`) to filter by.
#' @param invert If `TRUE`, return entries that don't match.
#' @param fail If `TRUE` (default), error on a non-existent path;
#'   otherwise drop it with a warning.
#' @param ... Passed to [grepl()] (e.g. `ignore.case = TRUE`).
#'
#' @return A tibble with columns `path`, `type`, `size`,
#'   `modification_time`, `access_time`, `birth_time`. A
#'   `sharefs_warning_powershell_partial` warning is raised if some
#'   items (e.g. a permission-denied subfolder) had to be skipped -- the
#'   returned tibble still contains everything else that was retrieved.
#' @seealso [sfs_powershell_available()] for how PowerShell's own
#'   availability is determined and cached.
#' @export
#'
#' @examples
#' sfs_dir_info(system.file(package = "sharefs"))
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
#' @return A character vector of paths (an [fs::fs_path()]).
#' @export
#'
#' @examples
#' sfs_dir_ls(system.file(package = "sharefs"))
sfs_dir_ls <- function(path = ".", all = FALSE, recurse = FALSE, type = "any",
                    regexp = NULL, glob = NULL, invert = FALSE,
                    fail = TRUE, ...) {
  sfs_dir_info(
    path = path, all = all, recurse = recurse, type = type,
    regexp = regexp, glob = glob, invert = invert, fail = fail, ...
  )$path
}
