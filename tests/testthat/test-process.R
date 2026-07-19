test_that("run_process() forwards command/args/timeout to processx::run()", {
   captured <- NULL
   
   with_mocked_bindings(
      run = function(command, args, ..., timeout) {
         captured <<- list(command = command, args = args, timeout = timeout)
         list(status = 0L, stdout = "", stderr = "")
      },
      .package = "processx",
      code = {
         run_process("some-command", c("-Arg1", "-Arg2"), timeout = 7)
      }
   )
   
   expect_equal(captured$command, "some-command")
   expect_equal(captured$args, c("-Arg1", "-Arg2"))
   expect_equal(captured$timeout, 7)
})

test_that("run_process() always passes error_on_status = FALSE", {
   captured_error_on_status <- NULL
   
   with_mocked_bindings(
      run = function(..., error_on_status) {
         captured_error_on_status <<- error_on_status
         list(status = 1L, stdout = "", stderr = "")
      },
      .package = "processx",
      code = {
         run_process("some-command", character(0), timeout = 1)
      }
   )
   
   expect_false(captured_error_on_status)
})

test_that("run_process() forwards additional arguments (e.g. encoding) via ...", {
   captured_encoding <- NULL
   
   with_mocked_bindings(
      run = function(..., encoding) {
         captured_encoding <<- encoding
         list(status = 0L, stdout = "", stderr = "")
      },
      .package = "processx",
      code = {
         run_process("some-command", character(0), timeout = 1, encoding = "UTF-8")
      }
   )
   
   expect_equal(captured_encoding, "UTF-8")
})
