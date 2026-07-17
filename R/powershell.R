# Package-level cache for sfs_powershell_available(), so a real listing
# call doesn't pay the cost of spawning PowerShell just to check it's
# usable, on every single call.
.sharefs_cache <- new.env(parent = emptyenv())

# How long a cached TRUE is trusted before being rechecked, to bound
# staleness in a long-running/persistent R session (e.g. Shiny, plumber)
# where PowerShell availability could change without the process
# restarting.
sharefs_powershell_cache_ttl <- function() {
   60 * 60 * 24 # 1 day, in seconds
}

#' Check whether a usable PowerShell executable is available
#'
#' @description
#' Looks for `pwsh` (PowerShell 7+), falling back to the older
#' `powershell.exe`, and runs a lightweight smoke test (`Write-Output 1`)
#' to confirm it's actually allowed to run -- the executable can exist
#' on `PATH` and still be blocked by an AppLocker/WDAC policy.
#'
#' The result is cached for the rest of the R session. The cache is
#' asymmetric: a cached `FALSE` is always rechecked (so if you fix
#' whatever was blocking PowerShell, the very next call reflects that
#' immediately), while a cached `TRUE` is trusted for up to 1 day before
#' being rechecked.
#'
#' [sfs_dir_info()] and [sfs_robocopy()] both error, rather than falling
#' back to something else, when their respective tool isn't available.
#'
#' @return A single logical value.
#' @export
#'
#' @examples
#' sfs_powershell_available()
sfs_powershell_available <- function() {
   cached <- .sharefs_cache$powershell_usable
   
   if (!is.null(cached) && isTRUE(cached$value)) {
      age <- as.numeric(Sys.time() - cached$checked_at, units = "secs")
      if (age < sharefs_powershell_cache_ttl()) {
         return(TRUE)
      }
   }
   
   result <- check_powershell_usable()
   .sharefs_cache$powershell_usable <- list(value = result, checked_at = Sys.time())
   result
}

check_powershell_usable <- function() {
   exe <- find_powershell()
   if (!nzchar(exe)) {
      return(FALSE)
   }
   
   tryCatch(
      {
         res <- processx::run(
            command = exe,
            args = c(
               "-NoProfile", "-NonInteractive",
               "-ExecutionPolicy", "Bypass",
               "-Command", "Write-Output 1"
            ),
            error_on_status = FALSE,
            timeout = 3
         )
         identical(res$status, 0L) && identical(trimws(res$stdout), "1")
      },
      error = function(e) FALSE
   )
}

#' Check whether `robocopy` is available
#'
#' @description
#' Checks whether `robocopy` is on `PATH`. Unlike
#' [sfs_powershell_available()], this isn't cached -- it's a plain,
#' cheap `Sys.which()` check with no smoke test behind it.
#'
#' @return A single logical value.
#' @export
#'
#' @examples
#' sfs_robocopy_available()
sfs_robocopy_available <- function() {
   nzchar(find_robocopy())
}

find_robocopy <- function() {
   Sys.which("robocopy")
}

find_powershell <- function() {
   exe <- Sys.which("pwsh")
   if (nzchar(exe)) {
      return(exe)
   }
   Sys.which("powershell")
}

# PowerShell's own escaping rule for single-quoted strings: double the quote.
escape_ps_string <- function(x) {
   gsub("'", "''", x, fixed = TRUE)
}

run_powershell <- function(script_lines) {
   exe <- find_powershell()
   if (!nzchar(exe)) {
      cli::cli_abort(
         "No PowerShell executable found on the {.envvar PATH}.",
         class = "sharefs_error_no_powershell"
      )
   }
   
   # Windows PowerShell 5.1 writes stdout using the system's legacy
   # OEM/ANSI codepage when redirected, mangling non-ASCII output.
   # PowerShell 7+ already defaults to UTF-8, so this is a no-op there.
   script <- paste(
      "[Console]::OutputEncoding = [System.Text.Encoding]::UTF8",
      paste(script_lines, collapse = "\n"),
      sep = "\n"
   )
   
   # Windows caps a process's total command line at ~32,767 characters
   # (CreateProcess's lpCommandLine limit). A script built from many
   # paths, or a few very long UNC ones, could approach that -- checked
   # here with a clear error rather than letting the OS fail the process
   # spawn with a cryptic one.
   if (nchar(script) > 30000) {
      cli::cli_abort(
         c(
            "The PowerShell command for this call is too long to run.",
            "i" = "Windows limits a process's total command line length --
               try calling this with fewer or shorter paths at once."
         ),
         class = "sharefs_error_command_too_long"
      )
   }
   
   # -Command works even under the default "Restricted" execution
   # policy, since that only blocks running script *files*.
   # -ExecutionPolicy Bypass is kept anyway as a harmless extra layer.
   args <- c(
      "-NoProfile", "-NonInteractive",
      "-ExecutionPolicy", "Bypass",
      "-Command", script
   )
   
   # timeout bounds how long this can run, so a dead/hung network share
   # can't freeze the R session indefinitely; stdout/stderr come back
   # cleanly separated and correctly decoded.
   res <- tryCatch(
      processx::run(
         command = exe,
         args = args,
         error_on_status = FALSE,
         timeout = 30,
         encoding = "UTF-8"
      ),
      error = function(e) {
         spawn_error <- e$message
         cli::cli_abort(
            c(
               "The background PowerShell process failed to start or timed out.",
               "x" = "{spawn_error}"
            ),
            class = "sharefs_error_powershell_execution_failed"
         )
      }
   )
   
   status <- as.integer(res$status)
   if (is.na(status)) {
      status <- 16L
   }
   
   if (status != 0L) {
      combined_output <- paste(
         c(
            if (nzchar(res$stdout)) paste("[stdout]:", trimws(res$stdout)),
            if (nzchar(res$stderr)) paste("[stderr]:", trimws(res$stderr))
         ),
         collapse = " | "
      )
      cli::cli_abort(
         c(
            "PowerShell script failed with status {status}.",
            "i" = "Output: {combined_output}"
         ),
         class = "sharefs_error_powershell_failed"
      )
   }
   
   # Non-zero status is the only failure condition -- deliberately not
   # "status 0 but stderr non-empty": Get-ChildItem -Recurse continues
   # past e.g. a permission-denied subfolder and still exits 0, reporting
   # the problem only via stderr. Treating that as failure would discard
   # an otherwise-successful, mostly-complete listing; surface it as a
   # warning instead.
   if (nzchar(res$stderr)) {
      partial_stderr <- trimws(res$stderr)
      cli::cli_warn(
         c(
            "PowerShell reported non-fatal warnings while listing.",
            "i" = "The listing still completed; this may mean some items
               (e.g. a permission-denied subfolder) were skipped.",
            "x" = "{partial_stderr}"
         ),
         class = "sharefs_warning_powershell_partial"
      )
   }
   
   if (!nzchar(res$stdout)) {
      return(character(0))
   }
   
   strsplit(res$stdout, "\r?\n")[[1]]
}
