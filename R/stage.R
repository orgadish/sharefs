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
#'   a new temporary directory is created.
#'
#' @return A tibble with columns `path`, `local_path`, `size`, and
#'   `modification_time`. The staging directory is attached as the
#'   `"sharefs_stage_dir"` attribute -- pass the result to
#'   [sfs_stage_cleanup()] when done.
#' @export
#'
#' @examples
#' src <- tempfile()
#' dir.create(src)
#' files <- file.path(src, c("a.txt", "b.txt"))
#' file.create(files)
#'
#' staged <- sfs_stage_local(files)
#' staged
#'
#' sfs_stage_cleanup(staged)
#' unlink(src, recursive = TRUE)
sfs_stage_local <- function(files, dir = NULL) {
  # Argument validation
  file_paths <- if (is_dir_info_table(files)) files$path else files

  if (length(file_paths) == 0) {
    cli::cli_abort(
      "{.arg files} must contain at least one path.",
      class = "sharefs_error_no_files"
    )
  }

  missing <- file_paths[!file.exists(file_paths)]
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

  # Stage the files
  if (is.null(dir)) {
    dir <- tempfile("sharefs_stage_")
  }
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  local_paths <- stage_local_robocopy(file_paths, dir)

  # Build the result
  local_info <- file.info(local_paths, extra_cols = FALSE)

  out <- tibble::tibble(
    path = file_paths,
    local_path = local_paths,
    size = as.double(local_info$size),
    modification_time = local_info$mtime
  )

  attr(out, "sharefs_stage_dir") <- dir
  out
}

#' Remove a staging directory created by `sfs_stage_local()`
#'
#' @param staged The tibble returned by [sfs_stage_local()].
#'
#' @return `TRUE` (invisibly).
#' @export
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
  unlink(dir, recursive = TRUE)
  invisible(TRUE)
}

check_no_duplicate_basenames <- function(files) {
  local_paths <- basename(files)
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

# One robocopy() call per source directory, rather than one per file.
# Paths are normalized before grouping (not just for the basenames, which
# are unaffected) so that mixed case or slash style for files actually in
# the same directory -- Windows paths are case-insensitive -- still group
# into a single call instead of spuriously splitting into more than
# necessary.
stage_local_robocopy <- function(files, dir) {
  files <- normalizePath(files, mustWork = FALSE)
  local_paths <- file.path(dir, basename(files))
  groups <- split(files, dirname(files))

  for (source_dir in names(groups)) {
    group_files <- groups[[source_dir]]
    robocopy(source_dir, dir, files = basename(group_files))
  }

  local_paths
}
