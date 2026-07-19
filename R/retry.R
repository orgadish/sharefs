#' Tripling backoff with proportional jitter
#'
#' @description
#' The wait time before retry `attempt` (1-based), tripling each time:
#' `initial_wait_seconds * 3^(attempt - 1)`. With jitter (the default),
#' each wait is independently varied by +/- 10% so that multiple retrying
#' callers hitting the same locked file or network resource don't wake up
#' and collide again in lockstep ("thundering herd").
#'
#' @param attempt Current attempt index (1-based).
#' @param initial_wait_seconds Baseline sleep time for the first retry.
#' @param backoff_factor Multiplier for successive delays.
#' @param jitter Logical. If `TRUE` (default), applies a +/- 10% random
#'   variation to the wait time. Set `FALSE` for exact, deterministic
#'   wait times (e.g. in tests).
#'
#' @return A single numeric number of seconds.
get_backoff_wait <- function(attempt,
                             initial_wait_seconds = 0.05,
                             backoff_factor = 3,
                             jitter = TRUE) {
   (
      initial_wait_seconds 
      * backoff_factor^(attempt - 1)
      * (if (jitter) stats::runif(1, 0.9, 1.1) else 1)
   )
}

#' Retry an action if it throws an error
#'
#' @description
#' Calls `action()`, retrying with backoff if it throws. Returns the
#' successful result, or re-raises the final error (with its original
#' class and attributes intact) if every attempt fails.
#'
#' @param action A function/closure to execute.
#' @param retries Maximum number of total attempts. Values less than 1
#'   are treated as 1 -- `action()` always runs at least once.
#' @param initial_wait_seconds Baseline backoff time.
#' @param jitter Logical. If `TRUE` (default), randomizes the sleep
#'   intervals; see [get_backoff_wait()].
#' @param retryable A function taking the caught error condition and
#'   returning `TRUE` if it's worth retrying, `FALSE` if it's certain to
#'   fail again no matter how many times it's retried (e.g. a permission
#'   error, as opposed to a transient network blip). Defaults to always
#'   retryable. When `FALSE`, the error is re-raised immediately without
#'   sleeping or consuming further attempts.
#'
#' @return The successful result of `action()`.
retry_on_error <- function(action,
                           retries = 5,
                           initial_wait_seconds = 0.05,
                           jitter = TRUE,
                           retryable = function(e) TRUE) {
   retries <- max(1L, as.integer(retries))
   
   for (i in seq_len(retries)) {
      result <- tryCatch(list(value = action()), error = function(e) e)
      
      if (!inherits(result, "error")) {
         return(result$value)
      }
      
      if (!isTRUE(retryable(result))) {
         break
      }
      
      if (i < retries) {
         Sys.sleep(get_backoff_wait(i, initial_wait_seconds, jitter = jitter))
      }
   }
   
   # Propagate the last caught error object directly, preserving its
   # original class/attributes rather than re-wrapping it.
   stop(result)
}
