#' Check whether a usable PowerShell executable is available
#'
#' @description
#' Looks for `powershell.exe`, falling back to `pwsh`.
#'
#' @return A single logical value.
#' @export
#'
#' @examples
#' sfs_powershell_available()
sfs_powershell_available <- function() {
  nzchar(find_powershell())
}

robocopy_available <- function() {
  nzchar(Sys.which("robocopy"))
}

find_powershell <- function() {
  exe <- Sys.which("powershell")
  if (nzchar(exe)) {
    return(exe)
  }
  Sys.which("pwsh")
}

# PowerShell's own escaping rule for single-quoted strings: double the quote.
escape_ps_string <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

# Run via -Command rather than writing a temp .ps1 and using -File: no file
# to write (no permission/AV-scan concerns), and the default "Restricted"
# execution policy only blocks running script *files* -- -Command still
# works under it. -ExecutionPolicy Bypass is kept anyway as a harmless
# extra layer; it only affects this one process, no elevation needed.
run_powershell <- function(script_lines) {
  exe <- find_powershell()
  if (!nzchar(exe)) {
    cli::cli_abort(
      "No PowerShell executable found on the {.envvar PATH}.",
      class = "sharefs_error_no_powershell"
    )
  }

  args <- c(
    "-NoProfile", "-NonInteractive",
    "-ExecutionPolicy", "Bypass",
    "-Command", paste(script_lines, collapse = "\n")
  )

  result <- suppressWarnings(
    system2(exe, args, stdout = TRUE, stderr = TRUE)
  )
  status <- attr(result, "status") %||% 0L

  if (!identical(status, 0L)) {
    cli::cli_abort(
      c(
        "PowerShell script failed with status {status}.",
        "i" = "Output: {paste(result, collapse = ' / ')}"
      ),
      class = "sharefs_error_powershell_failed"
    )
  }

  result
}
