#' Copy files to local disk before reading them
#'
#' @description
#' Copies `files` to a local directory using `robocopy` (multi-threaded,
#' grouped by source directory so each directory is one call). Reading
#' many small files locally is typically much faster than reading them
#' over a network share. Errors if `robocopy` is unavailable or fails --
#' no fallback, since `fs::file_copy()` doesn't preserve timestamps the
#' way `robocopy` does.
#'
#' `files` can be a plain path vector or the output of [sfs_dir_info()]
#' -- only `path` is used as the copy source. The result's
#' `size`/`modification_time` are re-derived from the staged copies
#' themselves, not carried through from any prior listing.
#'
#' @param files A character vector of existing file paths to stage, or
#'   the output of [sfs_dir_info()].
#' @param dir The local directory to copy files into. If `NULL` (default),
#'   a new temporary directory is created. If an existing directory is
#'   given, [sfs_stage_cleanup()] will only remove the files it staged
#'   there, not the directory itself or anything else already in it.
#'
#' @return A tibble with columns `path`, `local_path`, `size`, and
#'   `modification_time`. The staging directory is attached as the
#'   `"sharefs_stage_dir"` attribute -- pass the result to
#'   [sfs_stage_cleanup()] when done.
#' @export
#'
#' @examples
#' if (sfs_robocopy_available()) {
#'   src <- tempfile()
#'   dir.create(src)
#'   files <- file.path(src, c("a.txt", "b.txt"))
#'   file.create(files)
#'
#'   staged <- sfs_stage_local(files)
#'   staged
#'
#'   sfs_stage_cleanup(staged)
#'   unlink(src, recursive = TRUE)
#' }
sfs_stage_local <- function(files, dir = NULL) {
   # Argument validation
   file_paths <- if (is_dir_info_table(files)) files$path else files
   
   if (length(file_paths) == 0) {
      cli::cli_abort(
         "{.arg files} must contain at least one path.",
         class = "sharefs_error_no_files"
      )
   }
   
   missing <- file_paths[!fs::file_exists(file_paths)]
   if (length(missing) > 0) {
      cli::cli_abort(
         c(
            "Some {.arg files} do not exist.",
            "x" = "Missing: {.path {missing}}"
         ),
         class = "sharefs_error_missing_files"
      )
   }
   
   check_no_duplicate_basenames(file_paths)
   
   if (!sfs_robocopy_available()) {
      cli::cli_abort(
         c(
            "{.code robocopy} was not found on the {.envvar PATH}.",
            "i" = "It ships with Windows by default; if it's genuinely
					missing, this is likely a minimal or locked-down install."
         ),
         class = "sharefs_error_robocopy_unavailable"
      )
   }
   
   # Stage the files. Whether sharefs created `dir` itself is tracked so
   # sfs_stage_cleanup() knows whether it's safe to remove the directory
   # itself, not just the files placed in it -- an existing directory the
   # caller supplied may have had other content in it already.
   dir_created_by_us <- is.null(dir)
   if (is.null(dir)) {
      dir <- tempfile("sharefs_stage_")
   }
   if (!fs::dir_exists(dir)) {
      fs::dir_create(dir, recurse = TRUE)
      dir_created_by_us <- TRUE
   }
   
   local_paths <- stage_local_robocopy(file_paths, dir)
   
   # Build the result
   local_info <- fs::file_info(local_paths)
   
   out <- tibble::tibble(
      path = file_paths,
      local_path = local_paths,
      size = as.double(local_info$size),
      modification_time = local_info$modification_time
   )
   
   attr(out, "sharefs_stage_dir") <- dir
   attr(out, "sharefs_stage_dir_created") <- dir_created_by_us
   out
}

#' Remove the files staged by `sfs_stage_local()`
#'
#' @description
#' Removes exactly the files `sfs_stage_local()` staged. If it also
#' created the staging directory itself (rather than being given an
#' existing one via `dir`), that directory is removed too.
#'
#' @param staged The tibble returned by [sfs_stage_local()].
#'
#' @return `TRUE` (invisibly).
#' @export
#'
#' @examples
#' if (sfs_robocopy_available()) {
#'   src <- tempfile()
#'   dir.create(src)
#'   files <- file.path(src, c("a.txt", "b.txt"))
#'   file.create(files)
#'
#'   staged <- sfs_stage_local(files)
#'   sfs_stage_cleanup(staged)
#'   unlink(src, recursive = TRUE)
#' }
sfs_stage_cleanup <- function(staged) {
   dir <- attr(staged, "sharefs_stage_dir")
   if (is.null(dir)) {
      cli::cli_abort(
         c(
            "{.arg staged} doesn't have a {.field sharefs_stage_dir} attribute.",
            "i" = "Was it created by {.fn sfs_stage_local}?"
         ),
         class = "sharefs_error_not_staged"
      )
   }
   
   # unlink(), not fs::file_delete()/dir_delete(): those error if the
   # path is already gone, which would make cleanup less tolerant of
   # being called twice or after a partial failure.
   unlink(staged$local_path)
   
   if (isTRUE(attr(staged, "sharefs_stage_dir_created"))) {
      unlink(dir, recursive = TRUE)
   }
   
   invisible(TRUE)
}

check_no_duplicate_basenames <- function(files) {
   local_paths <- fs::path_file(files)
   if (anyDuplicated(local_paths)) {
      cli::cli_abort(
         c(
            "Staging would overwrite files: two or more {.arg files} share
			the same base name.",
            "i" = "Staging flattens files into one directory; stage files
			with the same name separately if you need to keep them apart."
         ),
         class = "sharefs_error_duplicate_basenames"
      )
   }
}

# One sfs_robocopy() call per source directory, rather than one per file.
# Windows paths are case-insensitive, so grouping uses a lowercased key
# -- fs's path functions are purely lexical and don't correct case, so
# two references to the same directory in different case would
# otherwise split into separate calls.
stage_local_robocopy <- function(files, dir) {
   files <- to_windows_path(files)
   local_paths <- file.path(dir, fs::path_file(files))
   
   file_dirs <- fs::path_dir(files)
   group_key <- tolower(file_dirs)
   
   for (key in unique(group_key)) {
      in_group <- group_key == key
      sfs_robocopy(
         as.character(file_dirs[in_group][1]), dir,
         files = as.character(fs::path_file(files[in_group]))
      )
   }
   
   local_paths
}
