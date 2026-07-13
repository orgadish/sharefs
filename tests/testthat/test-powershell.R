test_that("escape_ps_string() doubles single quotes", {
  expect_equal(escape_ps_string("it's"), "it''s")
  expect_equal(escape_ps_string("no quotes"), "no quotes")
  expect_equal(escape_ps_string("''"), "''''")
})

test_that("find_powershell() prefers powershell.exe over pwsh", {
  local_mocked_bindings(
    Sys.which = function(names) {
      if (identical(names, "powershell")) {
        c(powershell = "/fake/powershell")
      } else {
        c(pwsh = "")
      }
    },
    .package = "base"
  )
  expect_equal(find_powershell(), c(powershell = "/fake/powershell"))
})

test_that("find_powershell() falls back to pwsh if powershell.exe isn't found", {
  local_mocked_bindings(
    Sys.which = function(names) {
      if (identical(names, "powershell")) {
        c(powershell = "")
      } else {
        c(pwsh = "/fake/pwsh")
      }
    },
    .package = "base"
  )
  expect_equal(find_powershell(), c(pwsh = "/fake/pwsh"))
})

test_that("find_powershell() returns an empty result if neither is found", {
  local_mocked_bindings(
    Sys.which = function(names) stats::setNames("", names),
    .package = "base"
  )
  expect_false(nzchar(find_powershell()))
})

test_that("sfs_powershell_available() reflects find_powershell()", {
  local_mocked_bindings(find_powershell = function() c(powershell = "/fake/path"))
  expect_true(sfs_powershell_available())

  local_mocked_bindings(find_powershell = function() c(powershell = ""))
  expect_false(sfs_powershell_available())
})

test_that("robocopy_available() reflects whether robocopy is on PATH", {
  local_mocked_bindings(Sys.which = function(...) c(robocopy = "/fake/robocopy"), .package = "base")
  expect_true(robocopy_available())

  local_mocked_bindings(Sys.which = function(...) c(robocopy = ""), .package = "base")
  expect_false(robocopy_available())
})

test_that("run_powershell() errors if no PowerShell executable is found", {
  local_mocked_bindings(find_powershell = function() c(powershell = ""))

  expect_error(
    run_powershell("Get-Date"),
    class = "sharefs_error_no_powershell"
  )
})

test_that("run_powershell() constructs the expected system2() call", {
  local_mocked_bindings(find_powershell = function() c(powershell = "/fake/powershell"))

  captured <- NULL
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      captured <<- list(command = command, args = args)
      "output"
    },
    .package = "base"
  )

  run_powershell(c("line1", "line2"))

  expect_equal(captured$command, c(powershell = "/fake/powershell"))
  expect_true("-NoProfile" %in% captured$args)
  expect_true("-NonInteractive" %in% captured$args)
  expect_true("-ExecutionPolicy" %in% captured$args)
  expect_true("Bypass" %in% captured$args)
  expect_true("-Command" %in% captured$args)
  # The whole multi-line script is one array element, not split up.
  expect_true("line1\nline2" %in% captured$args)
})

test_that("run_powershell() errors on a non-zero exit status", {
  local_mocked_bindings(find_powershell = function() c(powershell = "/fake/powershell"))
  local_mocked_bindings(
    system2 = function(...) structure("some error output", status = 1L),
    .package = "base"
  )

  expect_error(
    run_powershell("some-command"),
    class = "sharefs_error_powershell_failed"
  )
})

test_that("run_powershell() returns the script's output on success", {
  local_mocked_bindings(find_powershell = function() c(powershell = "/fake/powershell"))
  local_mocked_bindings(
    system2 = function(...) c("a", "b"), # no status attribute: real system2() omits it on success
    .package = "base"
  )

  expect_equal(run_powershell("some-command"), c("a", "b"))
})
