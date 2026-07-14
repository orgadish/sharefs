test_that("backoff_wait() triples from the initial wait", {
   expect_equal(backoff_wait(1, initial_wait_seconds = 0.05), 0.05)
   expect_equal(backoff_wait(2, initial_wait_seconds = 0.05), 0.15)
   expect_equal(backoff_wait(3, initial_wait_seconds = 0.05), 0.45)
   expect_equal(backoff_wait(4, initial_wait_seconds = 0.05), 1.35)
})

test_that("retry_until() returns TRUE immediately once done() passes, without extra attempts", {
   call_count <- 0
   result <- retry_until(
      action = function() call_count <<- call_count + 1,
      done = function() TRUE,
      retries = 5,
      initial_wait_seconds = 0
   )
   expect_true(result)
   expect_equal(call_count, 1)
})

test_that("retry_until() retries until done() passes, then stops", {
   call_count <- 0
   result <- retry_until(
      action = function() call_count <<- call_count + 1,
      done = function() call_count >= 3,
      retries = 10,
      initial_wait_seconds = 0
   )
   expect_true(result)
   expect_equal(call_count, 3)
})

test_that("retry_until() returns FALSE after exhausting retries without success", {
   call_count <- 0
   result <- retry_until(
      action = function() call_count <<- call_count + 1,
      done = function() FALSE,
      retries = 4,
      initial_wait_seconds = 0
   )
   expect_false(result)
   expect_equal(call_count, 4)
})

test_that("retry_on_error() returns the result immediately on success, without retrying", {
   call_count <- 0
   result <- retry_on_error(
      function() {
         call_count <<- call_count + 1
         "ok"
      },
      retries = 5,
      initial_wait_seconds = 0
   )
   expect_equal(result, "ok")
   expect_equal(call_count, 1)
})

test_that("retry_on_error() retries after an error and returns the result once it succeeds", {
   call_count <- 0
   result <- retry_on_error(
      function() {
         call_count <<- call_count + 1
         if (call_count < 3) {
            stop("simulated transient failure")
         }
         "ok"
      },
      retries = 5,
      initial_wait_seconds = 0
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
         retries = 3,
         initial_wait_seconds = 0
      ),
      "persistent failure"
   )
   expect_equal(call_count, 3)
})

test_that("retry_on_error() actually sleeps with the tripling backoff schedule", {
   sleep_calls <- numeric(0)
   local_mocked_bindings(
      Sys.sleep = function(seconds) sleep_calls <<- c(sleep_calls, seconds),
      .package = "base"
   )

   expect_error(
      retry_on_error(
         function() stop("always fails"),
         retries = 5,
         initial_wait_seconds = 0.05
      ),
      "always fails"
   )

   expect_equal(sleep_calls, c(0.05, 0.15, 0.45, 1.35))
})
