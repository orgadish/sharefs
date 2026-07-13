# Helpers shared by both dir_info backends (fs and powershell): the
# column set they must both produce, input validation, and the filtering
# applied uniformly to whichever one's output.

dir_info_columns <- function() {
  c("path", "type", "size", "modification_time", "access_time", "birth_time")
}

dir_info_type_levels <- function() {
  c(
    "file", "directory", "symlink",
    "FIFO", "socket", "character_device", "block_device"
  )
}

validate_dir_info_type <- function(type) {
  bad <- setdiff(type, c("any", dir_info_type_levels()))
  if (length(bad) > 0) {
    cli::cli_abort(
      c(
        "{.arg type} must be {.val any} or one of {.val {dir_info_type_levels()}}.",
        "x" = "Unrecognized: {.val {bad}}"
      ),
      class = "sharefs_error_bad_type"
    )
  }
  type
}

# glob and regexp are the same filter in two forms; only one may be given.
resolve_dir_info_pattern <- function(regexp, glob) {
  if (!is.null(regexp) && !is.null(glob)) {
    cli::cli_abort(
      "Only one of {.arg regexp} or {.arg glob} may be supplied.",
      class = "sharefs_error_regexp_and_glob"
    )
  }
  if (!is.null(glob)) {
    return(utils::glob2rx(glob))
  }
  regexp
}

empty_dir_info <- function() {
  tibble::tibble(
    path = character(0),
    type = factor(character(0), levels = dir_info_type_levels()),
    size = double(0),
    modification_time = as.POSIXct(character(0)),
    access_time = as.POSIXct(character(0)),
    birth_time = as.POSIXct(character(0))
  )
}

# type/regexp/invert are applied here, after listing, so both backends
# filter identically. size is forced to 0 for directories, matching
# fs::dir_info()'s own behavior on Windows.
filter_dir_info <- function(info, type, regexp, invert, ...) {
  info$size[as.character(info$type) == "directory"] <- 0

  keep <- rep(TRUE, nrow(info))

  if (!identical(type, "any")) {
    keep <- keep & (as.character(info$type) %in% type)
  }

  if (!is.null(regexp)) {
    matches <- grepl(regexp, info$path, ...)
    keep <- keep & (if (invert) !matches else matches)
  }

  info[keep, , drop = FALSE]
}

# Whether `x` looks like sfs_dir_info()'s output: a data frame with the
# 3 columns stage_local() actually needs, out of dir_info_columns()'s 6.
# Not exported -- filecacher (or anyone else) that wants this check
# should define its own copy, since its requirements may differ (e.g.
# accepting "mtime" as well as "modification_time").
is_dir_info_table <- function(x) {
  required <- c("path", "size", "modification_time")
  is.data.frame(x) && all(required %in% names(x))
}
