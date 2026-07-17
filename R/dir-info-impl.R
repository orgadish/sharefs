# Backend implementation for sfs_dir_info() (see dir-info.R for the
# front-end: argument validation, dispatch, retry). Everything here is
# either the PowerShell backend itself, or logic shared with the
# front-end (column/type definitions, filtering).
#
# There's no fs backend: fs::dir_info(recurse = TRUE) aborts the entire
# call if any one subdirectory is inaccessible, discarding results for
# every sibling the caller did have permission to see. Get-ChildItem
# -Recurse instead skips the inaccessible item and returns everything
# else (see run_powershell()). Since fs is strictly less robust here,
# falling back to it on PowerShell failure would trade a real problem
# for a worse, silent one -- sfs_dir_info() errors instead and points
# the caller at fs::dir_info() directly if they want it.

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

# Ensure only one of regexp or glob are passed in
validate_regexp_glob_exclusive <- function(regexp, glob) {
  if (!is.null(regexp) && !is.null(glob)) {
    cli::cli_abort(
      "Only one of {.arg regexp} or {.arg glob} may be supplied.",
      class = "sharefs_error_regexp_and_glob"
    )
  }
}

resolve_dir_info_pattern <- function(regexp, glob) {
  validate_regexp_glob_exclusive(regexp, glob)
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

# type/regexp/invert are applied here, after listing, so filtering is
# identical regardless of what changes in the backend above it.
# size is forced to 0 for directories, matching fs::dir_info()'s own
# behavior on Windows. glob is resolved to a regexp here, the first and
# only place it's used.
filter_dir_info <- function(info, type, regexp, glob, invert, ...) {
  regexp <- resolve_dir_info_pattern(regexp, glob)

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
# Not exported -- other callers that want this check should define their
# own copy, since their requirements may differ (e.g. accepting "mtime"
# as well as "modification_time").
is_dir_info_table <- function(x) {
  required <- c("path", "size", "modification_time")
  is.data.frame(x) && all(required %in% names(x))
}

# Whether a caught error from the PowerShell backend is worth retrying.
# A permission error against the whole requested path won't resolve no
# matter how many times it's retried. Everything else -- including
# "path not found"/network-unreachable errors -- is retried by default:
# sfs_dir_info() already confirms the path exists (fs::dir_exists())
# before calling PowerShell, so a not-found result at that point is
# more likely a transient network blip than a genuine mistake.
is_permission_error <- function(e) {
  # ignore.case is silently ignored by grepl() whenever fixed = TRUE, so
  # case is normalized here instead of relying on it.
  msg <- tolower(conditionMessage(e))
  literals <- tolower(c("UnauthorizedAccess", "PermissionDenied", "is denied"))

  # Position() short-circuits (stops at the first match), unlike
  # vapply()/sapply(), which always evaluate every element.
  !is.na(Position(function(x) grepl(x, msg, fixed = TRUE), literals))
}

# Primary (and only) backend: one Get-ChildItem call across every path in
# `path`, returning metadata for every entry from a single request.
dir_info_powershell <- function(path, all, recurse) {
  # path_abs(), path_norm(), and gsub() (inside to_windows_path()) are all
  # vectorized over the whole path vector, so a single call handles every
  # path at once.
  normalized_paths <- to_windows_path(path)

  ps_paths <- escape_ps_string(normalized_paths)
  ps_path_array <- paste0("@(", paste0("'", ps_paths, "'", collapse = ", "), ")")
  recurse_flag <- if (recurse) " -Recurse" else ""
  force_flag <- if (all) " -Force" else ""

  lines <- sprintf(
    "$items = Get-ChildItem -LiteralPath %s%s%s",
    ps_path_array, recurse_flag, force_flag
  )

  # Timestamps are formatted explicitly since their default string form
  # depends on the session's locale.
  lines <- c(
    lines,
    paste(
      "$items | Select-Object FullName,",
      "@{Name='Type';Expression={",
      "if ($_.LinkType) {'symlink'}",
      "elseif ($_.PSIsContainer) {'directory'}",
      "else {'file'} }},",
      "Length,",
      "@{Name='ModificationTime';",
      "Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}},",
      "@{Name='AccessTime';",
      "Expression={$_.LastAccessTime.ToString('yyyy-MM-dd HH:mm:ss')}},",
      "@{Name='BirthTime';",
      "Expression={$_.CreationTime.ToString('yyyy-MM-dd HH:mm:ss')}} |",
      "ConvertTo-Csv -NoTypeInformation"
    )
  )

  csv_lines <- run_powershell(lines)

  if (length(csv_lines) <= 1) {
    return(empty_dir_info())
  }

  parsed <- utils::read.csv(
    text = paste(csv_lines, collapse = "\n"),
    stringsAsFactors = FALSE,
    colClasses = "character"
  )

  parse_time <- function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S")

  tibble::tibble(
    path = fs::as_fs_path(parsed$FullName),
    type = factor(parsed$Type, levels = dir_info_type_levels()),
    size = fs::as_fs_bytes(as.double(parsed$Length)),
    modification_time = parse_time(parsed$ModificationTime),
    access_time = parse_time(parsed$AccessTime),
    birth_time = parse_time(parsed$BirthTime)
  )
}
