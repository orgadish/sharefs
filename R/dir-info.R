#' List files with minimal network calls
#'
#' @description
#' Lists files and metadata in a single network round trip, using
#' PowerShell's `Get-ChildItem` instead of one `stat()` per file. Retries
#' a few times before falling back to [fs::dir_info()] if PowerShell
#' isn't available or fails.
#'
#' Arguments mirror [fs::dir_info()]. The result has fewer columns,
#' though: `path`, `type`, `size`, `modification_time`, `access_time`,
#' `birth_time`. The rest of `fs::dir_info()`'s 18 columns aren't
#' available via `Get-ChildItem` the same way, so they're left out
#' rather than filled in with placeholders -- call [fs::dir_info()]
#' directly if you need them.
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
#'   `modification_time`, `access_time`, `birth_time`.
#' @export
#'
#' @examples
#' sfs_dir_info(system.file(package = "sharefs"))
sfs_dir_info <- function(
   path = ".",
   all = FALSE,
   recurse = FALSE,
   type = "any",
   regexp = NULL,
   glob = NULL,
   invert = FALSE,
   fail = TRUE,
   ...
) {
   # Argument validation
   type <- validate_dir_info_type(type)
   regexp <- resolve_dir_info_pattern(regexp, glob)

   missing_paths <- path[!dir.exists(path)]
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
      path <- setdiff(path, missing_paths)
   }

   if (length(path) == 0) {
      return(empty_dir_info())
   }

   # Try PowerShell first, retrying a transient failure (e.g. a brief
   # network blip) before giving up on it -- falling back to fs doesn't
   # route around that kind of failure, since both backends need to
   # reach the same network path.
   info <- NULL

   if (sfs_powershell_available()) {
      info <- tryCatch(
         retry_on_error(
            function() dir_info_powershell(path, all = all, recurse = recurse),
            retries = 5
         ),
         error = function(e) {
            cli::cli_warn(
               c(
                  "The {.val powershell} listing failed after retrying; falling back to {.pkg fs}.",
                  "i" = conditionMessage(e)
               ),
               class = "sharefs_warning_powershell_fallback"
            )
            NULL
         }
      )
   }

   # Otherwise fall back to fs, with the same light retry -- this is the
   # last resort, so a transient failure here has nothing further to
   # fall back to.
   if (is.null(info)) {
      info <- retry_on_error(
         function() dir_info_fs(path, all = all, recurse = recurse),
         retries = 5
      )
   }

   filter_dir_info(info, type = type, regexp = regexp, invert = invert, ...)
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
sfs_dir_ls <- function(
   path = ".",
   all = FALSE,
   recurse = FALSE,
   type = "any",
   regexp = NULL,
   glob = NULL,
   invert = FALSE,
   fail = TRUE,
   ...
) {
   sfs_dir_info(
      path = path,
      all = all,
      recurse = recurse,
      type = type,
      regexp = regexp,
      glob = glob,
      invert = invert,
      fail = fail,
      ...
   )$path
}
