# Thin wrapper around processx::run() to allow mocking in tests.
#
# error_on_status is always FALSE: every caller interprets the exit
# code itself rather than letting processx throw on a non-zero one.
run_process <- function(command, args, timeout, ...) {
   processx::run(
      command = command,
      args = args,
      error_on_status = FALSE,
      timeout = timeout,
      ...
   )
}
