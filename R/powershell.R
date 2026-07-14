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

   # Windows PowerShell 5.1 (the default on most Windows installs) writes
   # stdout using the system's legacy OEM/ANSI codepage when its output is
   # redirected/captured rather than shown in a real console -- non-ASCII
   # output (e.g. file names) gets silently replaced with '?' or the wrong
   # character otherwise, confirmed against a real run. Forcing UTF-8
   # explicitly fixes this; PowerShell 7+ already defaults to UTF-8, so
   # this is a harmless no-op there.
   script <- paste(
      "[Console]::OutputEncoding = [System.Text.Encoding]::UTF8",
      paste(script_lines, collapse = "\n"),
      sep = "\n"
   )

   args <- c(
      "-NoProfile",
      "-NonInteractive",
      "-ExecutionPolicy",
      "Bypass",
      "-Command",
      script
   )

   result <- suppressWarnings(
      system2(exe, args, stdout = TRUE, stderr = TRUE)
   )
   Encoding(result) <- "UTF-8"
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
