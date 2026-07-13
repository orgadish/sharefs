# Primary backend: one Get-ChildItem call across every path in `path`,
# returning metadata for every entry from a single request.
dir_info_powershell <- function(path, all, recurse) {
  ps_paths <- vapply(
    normalizePath(path, winslash = "\\", mustWork = FALSE),
    escape_ps_string,
    character(1),
    USE.NAMES = FALSE
  )
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
    stringsAsFactors = FALSE
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
