test_that("backoff_wait() triples from the initial wait when jitter = FALSE", {
   # jitter = FALSE allows exact determinism checks
   expect_equal(backoff_wait(1, initial_wait_seconds = 0.05, jitter = FALSE), 0.05)
   expect_equal(backoff_wait(2, initial_wait_seconds = 0.05, jitter = FALSE), 0.15)
   expect_equal(backoff_wait(3, initial_wait_seconds = 0.05, jitter = FALSE), 0.45)
   expect_equal(backoff_wait(4, initial_wait_seconds = 0.05, jitter = FALSE), 1.35)
})

test_that("backoff_wait() applies +/- 10% proportional jitter when jitter = TRUE", {
   set.seed(42)
   wait1 <- backoff_wait(1, initial_wait_seconds = 10, jitter = TRUE)
   expect_true(wait1 >= 9.0 && wait1 <= 11.0)
   expect_true(wait1 != 10.0)
})

test_that("retry_on_error() returns the result immediately on success, without retrying", {
   call_count <- 0
   result <- retry_on_error(
      function() {
         call_count <<- call_count + 1
         "ok"
      },
      retries = 5, initial_wait_seconds = 0, jitter = FALSE
   )
   expect_equal(result, "ok")
   expect_equal(call_count, 1)
})

test_that("retry_on_error() retries after an error and returns the result once it succeeds", {
   call_count <- 0
   result <- retry_on_error(
      function() {
         call_count <<- call_count + 1
         if (call_count < 3) stop("simulated transient failure")
         "ok"
      },
      retries = 5, initial_wait_seconds = 0, jitter = FALSE
   )
   expect_equal(result, "ok")
   expect_equal(call_count, 3)
})

test_that("retry_on_error() re-raises the last error after exhausting retries", {
   call_count <- 0
   expect_error(
      retry_on_error(
         function() {
            call_count <<- call_count + 1
            stop("persistent failure")
         },
         retries = 3, initial_wait_seconds = 0, jitter = FALSE
      ),
      "persistent failure"
   )
   expect_equal(call_count, 3)
})

test_that("retry_on_error() actually sleeps with the tripling backoff schedule", {
   sleep_calls <- numeric(0)
   
   with_mocked_bindings(
      Sys.sleep = function(seconds) sleep_calls <<- c(sleep_calls, seconds),
      .package = "base",
      code = {
         expect_error(
            retry_on_error(
               function() stop("always fails"),
               retries = 5, initial_wait_seconds = 0.05, jitter = FALSE
            ),
            "always fails"
         )
      }
   )
   
   expect_equal(sleep_calls, c(0.05, 0.15, 0.45, 1.35))
})

test_that("retry_on_error() stops immediately, without sleeping or retrying, when retryable() is FALSE", {
   call_count <- 0
   sleep_calls <- 0
   
   with_mocked_bindings(
      Sys.sleep = function(...) sleep_calls <<- sleep_calls + 1,
      .package = "base",
      code = {
         expect_error(
            retry_on_error(
               function() {
                  call_count <<- call_count + 1
                  stop("permission denied, not worth retrying")
               },
               retries = 5, initial_wait_seconds = 0.05, jitter = FALSE,
               retryable = function(e) FALSE
            ),
            "permission denied"
         )
      }
   )
   
   expect_equal(call_count, 1)
   expect_equal(sleep_calls, 0)
})

test_that("retry_on_error() only calls retryable() on errors, and lets a later success through", {
   seen_by_retryable <- character(0)
   call_count <- 0
   
   result <- retry_on_error(
      function() {
         call_count <<- call_count + 1
         if (call_count < 3) stop(paste("attempt", call_count))
         "ok"
      },
      retries = 5, initial_wait_seconds = 0, jitter = FALSE,
      retryable = function(e) {
         seen_by_retryable <<- c(seen_by_retryable, conditionMessage(e))
         TRUE
      }
   )
   
   expect_equal(result, "ok")
   expect_equal(seen_by_retryable, c("attempt 1", "attempt 2"))
})

test_that("retry_on_error() runs at least once even with a degenerate retries value", {
   call_count_on_err <- 0
   res <- retry_on_error(
      action = function() {
         call_count_on_err <<- call_count_on_err + 1
         "success"
      },
      retries = -5, initial_wait_seconds = 0, jitter = FALSE
   )
   expect_equal(res, "success")
   expect_equal(call_count_on_err, 1)
})
