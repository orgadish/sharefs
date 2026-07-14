# Tripling backoff starting at `initial_wait_seconds`: 0.05, 0.15, 0.45,
# 1.35 for the default (retries = 5 means 4 waits between the 5
# attempts). Starting low keeps the common case responsive -- most
# transient failures this package retries for (a brief file lock, a
# network blip) clear in well under a second -- while tripling reaches a
# meaningfully long final wait (>1.3s) in just 4 steps, for the rarer
# case that actually needs it.
backoff_wait <- function(
   attempt,
   initial_wait_seconds = 0.05,
   backoff_factor = 3
) {
   initial_wait_seconds * backoff_factor^(attempt - 1)
}

# Calls `action()`, then `done()` to check whether it actually worked;
# repeats (with a tripling wait between attempts) until `done()` returns
# TRUE or `retries` attempts are used up. Returns whether `done()` ever
# passed. Exists for the common Windows scenario where an operation fails
# not because anything is really wrong, but because some other process
# (antivirus, Search Indexing, Explorer's thumbnail cache) holds a
# transient handle on the file/directory in question.
retry_until <- function(
   action,
   done,
   retries = 5,
   initial_wait_seconds = 0.05
) {
   for (i in seq_len(retries)) {
      action()
      if (isTRUE(done())) {
         return(TRUE)
      }
      if (i < retries) Sys.sleep(backoff_wait(i, initial_wait_seconds))
   }
   isTRUE(done())
}

# Calls `action()`, retrying (with a tripling wait between attempts) if
# it throws, up to `retries` total attempts. Returns the result on
# success; re-raises the last error if every attempt fails. A different
# shape from retry_until() because there's no separate condition to
# check here -- the action either returns a value or throws -- for
# operations like a network directory listing, where a failure might be
# a transient blip rather than something permanently wrong.
retry_on_error <- function(action, retries = 5, initial_wait_seconds = 0.05) {
   for (i in seq_len(retries)) {
      result <- tryCatch(list(value = action()), error = function(e) e)
      if (!inherits(result, "error")) {
         return(result$value)
      }
      if (i < retries) Sys.sleep(backoff_wait(i, initial_wait_seconds))
   }
   stop(result)
}
