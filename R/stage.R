#' Copy files to local disk before reading them
#'
#' @description
#' Copies `files` to a local directory using `robocopy`, since reading many
#' small files locally is typically much faster than reading them
#' over a network share. The entire source directory structure is preserved,
#' and `robocopy` is run multi-threaded, grouped by source directory so 
#' each directory is one call. 
#'
#' @param files A character vector of file paths to stage, or the
#'   output of [sfs_dir_info()].
#' @param dir The local directory to copy files into. If `NULL` (default),
#'   a new temporary directory is created. If an existing directory is
#'   given, [sfs_stage_cleanup()] will only remove the files/subdirectories 
#'   it staged there.
#'
#' @return A `data.frame` with columns `path`, `local_path`, `size`, and
#'   `modification_time` (or `tibble` if installed). The staging directory
#'   is attached as the `"sharefs_stage_dir"` attribute, so that the whole data
#'   frame can be passed to [sfs_stage_cleanup()] for cleanup.
#'
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

  if (!sfs_robocopy_available()) {
    cli::cli_abort(
      c(
        "{.code robocopy} was not found on the {.envvar PATH}.",
        "i" = "It ships with Windows by default; if it's genuinely missing, this is likely a minimal or locked-down install."
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

  # A missing/inaccessible source file just means robocopy never
  # created its local copy -- checking that locally afterward, rather
  # than checking every source file exists beforehand, gets the same
  # answer without a separate network round trip per file.
  copy_failed <- !fs::file_exists(local_paths)
  if (any(copy_failed)) {
    cli::cli_abort(
      c(
        "Some {.arg files} could not be staged.",
        "x" = "Missing or inaccessible: {.path {file_paths[copy_failed]}}"
      ),
      class = "sharefs_error_missing_files"
    )
  }

  # Build the result
  local_info <- fs::file_info(local_paths)

  out <- tibble_or_df(
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
#' Removes exactly the files `sfs_stage_local()` staged, and any
#' subdirectories it created to mirror their source structure that are
#' now empty. If it also created the staging directory itself (rather
#' than being given an existing one via `dir`), that directory is
#' removed too.
#'
#' @param staged The `data.frame` (or `tibble`) returned by [sfs_stage_local()].
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
  } else {
    # Files may have been staged into subdirectories mirroring source
    # structure (see stage_local_robocopy()); remove any that are now
    # empty, without touching `dir` itself or anything the caller (not
    # sharefs) put there.
    for (d in unique(as.character(fs::path_dir(staged$local_path)))) {
      remove_empty_dirs_up_to(d, dir)
    }
  }

  invisible(TRUE)
}

# Removes `dir` and each empty ancestor above it, stopping at (and
# never removing) `stop_at`. Bails out entirely, doing nothing, if
# `dir` isn't actually inside `stop_at`'s tree -- should always be true
# by construction (every staged local_path is built under the staging
# dir), but this is a deletion loop, so that isn't trusted blindly.
remove_empty_dirs_up_to <- function(dir, stop_at) {
  stop_at_norm <- fs::path_norm(stop_at)
  dir_norm <- fs::path_norm(dir)

  same_dir <- identical(tolower(as.character(dir_norm)), tolower(as.character(stop_at_norm)))
  if (!same_dir && !isTRUE(fs::path_has_parent(dir_norm, stop_at_norm))) {
    return(invisible())
  }

  stop_at_lower <- tolower(as.character(stop_at_norm))
  dir <- as.character(dir_norm)

  while (!identical(tolower(dir), stop_at_lower) &&
         fs::dir_exists(dir) &&
         length(fs::dir_ls(dir, all = TRUE)) == 0) {
    parent <- as.character(fs::path_dir(dir))
    fs::dir_delete(dir)
    dir <- parent
  }
}

# Mirrors each file's full source directory structure under `dir`,
# sanitized into something file.path() can nest as a relative subpath
# (strip the leading UNC slashes or drive-letter colon, which aren't
# valid inside a nested relative path).
stage_relative_dirs <- function(file_dirs) {
  sanitized <- gsub("^/+", "", as.character(file_dirs))
  gsub(":", "", sanitized, fixed = TRUE)
}

# One sfs_robocopy() call per source directory, rather than one per
# file. Windows paths are case-insensitive, so grouping uses a
# lowercased key -- fs's path functions are purely lexical and don't
# correct case, so two references to the same directory in different
# case would otherwise split into separate calls.
#
# Local copies land at dir/<source directory's full path>/<basename>,
# mirroring the source structure exactly rather than flattening
# everything into `dir` -- two files with the same name in different
# source directories, which is common in real directory trees, would
# otherwise collide.
stage_local_robocopy <- function(files, dir) {
  abs_files <- fs::path_norm(fs::path_abs(files))
  file_dirs <- fs::path_dir(abs_files)

  rel_dirs <- stage_relative_dirs(file_dirs)
  # file.path(dir, "") would leave a trailing slash (double-slashing
  # once the basename is joined on) -- guards against the (now rare)
  # case where a sanitized path is empty, e.g. staging a file directly
  # at a bare drive root.
  local_dirs <- ifelse(nzchar(rel_dirs), file.path(dir, rel_dirs), dir)
  local_paths <- file.path(local_dirs, as.character(fs::path_file(abs_files)))

  group_key <- tolower(as.character(file_dirs))

  for (key in unique(group_key)) {
    in_group <- group_key == key
    sfs_robocopy(
      to_windows_path(as.character(file_dirs[in_group][1])),
      to_windows_path(local_dirs[in_group][1]),
      files = as.character(fs::path_file(abs_files[in_group]))
    )
  }

  local_paths
}
