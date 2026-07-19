test_that("escape_ps_string() doubles single quotes", {
  expect_equal(escape_ps_string("it's"), "it''s")
  expect_equal(escape_ps_string("no quotes"), "no quotes")
  expect_equal(escape_ps_string("''"), "''''")
})

test_that("find_powershell() prefers pwsh over powershell.exe", {
  with_mocked_bindings(
    Sys.which = function(names) {
      if (identical(names, "pwsh")) {
        c(pwsh = "/fake/pwsh")
      } else {
        c(powershell = "")
      }
    },
    .package = "base",
    code = {
      expect_equal(find_powershell(), c(pwsh = "/fake/pwsh"))
    }
  )
})

test_that("find_powershell() falls back to powershell.exe if pwsh isn't found", {
  with_mocked_bindings(
    Sys.which = function(names) {
      if (identical(names, "pwsh")) {
        c(pwsh = "")
      } else {
        c(powershell = "/fake/powershell")
      }
    },
    .package = "base",
    code = {
      expect_equal(find_powershell(), c(powershell = "/fake/powershell"))
    }
  )
})

test_that("find_powershell() returns an empty result if neither is found", {
  with_mocked_bindings(
    Sys.which = function(names) stats::setNames("", names),
    .package = "base",
    code = {
      expect_false(nzchar(find_powershell()))
    }
  )
})

test_that("find_robocopy() reflects whether robocopy is on PATH", {
  with_mocked_bindings(
    Sys.which = function(...) c(robocopy = "/fake/robocopy"),
    .package = "base",
    code = {
      expect_equal(find_robocopy(), c(robocopy = "/fake/robocopy"))
    }
  )

  with_mocked_bindings(
    Sys.which = function(...) c(robocopy = ""),
    .package = "base",
    code = {
      expect_false(nzchar(find_robocopy()))
    }
  )
})

test_that("sfs_robocopy_available() reflects whether robocopy is on PATH", {
  with_mocked_bindings(
    Sys.which = function(...) c(robocopy = "/fake/robocopy"),
    .package = "base",
    code = expect_true(sfs_robocopy_available())
  )

  with_mocked_bindings(
    Sys.which = function(...) c(robocopy = ""),
    .package = "base",
    code = expect_false(sfs_robocopy_available())
  )
})

# --- check_powershell_usable() (the uncached smoke test itself) ------------

test_that("check_powershell_usable() requires both status 0 and stdout '1'", {
  with_mocked_bindings(
    find_powershell = function() c(powershell = "/fake/powershell"),
    code = {
      with_mocked_bindings(
        run_process = function(...) list(status = 0L, stdout = "1\n", stderr = ""),
        code = expect_true(check_powershell_usable())
      )

      with_mocked_bindings(
        run_process = function(...) list(status = 1L, stdout = "", stderr = "blocked by policy"),
        code = expect_false(check_powershell_usable())
      )

      with_mocked_bindings(
        run_process = function(...) list(status = 0L, stdout = "not 1", stderr = ""),
        code = expect_false(check_powershell_usable())
      )
    }
  )
})

test_that("check_powershell_usable() returns FALSE, not an error, if processx itself throws", {
  with_mocked_bindings(
    find_powershell = function() c(powershell = "/fake/powershell"),
    run_process = function(...) stop("spawn failed"),
    code = expect_false(check_powershell_usable())
  )
})

test_that("check_powershell_usable() returns FALSE without spawning anything if nothing is on PATH", {
  spawned <- FALSE

  with_mocked_bindings(
    find_powershell = function() c(powershell = ""),
    run_process = function(...) {
      spawned <<- TRUE
      list(status = 0L, stdout = "1", stderr = "")
    },
    code = expect_false(check_powershell_usable())
  )

  expect_false(spawned)
})

# --- sfs_powershell_available() caching -------------------------------------
# .sharefs_cache is package-level, mutable, global state -- reset it before
# and after every test in this section so tests can't leak into each other.

reset_powershell_cache <- function() {
  .sharefs_cache$powershell_usable <- NULL
}

test_that("sfs_powershell_available() caches a TRUE result and doesn't recheck", {
  reset_powershell_cache()
  withr::defer(reset_powershell_cache())

  call_count <- 0
  with_mocked_bindings(
    check_powershell_usable = function() {
      call_count <<- call_count + 1
      TRUE
    },
    code = {
      expect_true(sfs_powershell_available())
      expect_true(sfs_powershell_available())
      expect_true(sfs_powershell_available())
    }
  )

  expect_equal(call_count, 1)
})

test_that("sfs_powershell_available() always rechecks a cached FALSE result", {
  reset_powershell_cache()
  withr::defer(reset_powershell_cache())

  call_count <- 0
  with_mocked_bindings(
    check_powershell_usable = function() {
      call_count <<- call_count + 1
      FALSE
    },
    code = {
      expect_false(sfs_powershell_available())
      expect_false(sfs_powershell_available())
    }
  )

  expect_equal(call_count, 2)
})

test_that("sfs_powershell_available() recovers immediately once a cached FALSE becomes TRUE", {
  # This is the actual point of the asymmetric cache: if the user fixed
  # something (e.g. an AppLocker exception) after seeing an error, the
  # very next call should reflect that -- not wait a day, not require a
  # session restart.
  reset_powershell_cache()
  withr::defer(reset_powershell_cache())

  usable <- FALSE
  with_mocked_bindings(
    check_powershell_usable = function() usable,
    code = {
      expect_false(sfs_powershell_available())
      usable <- TRUE
      expect_true(sfs_powershell_available())
    }
  )
})

test_that("sfs_powershell_available() rechecks a cached TRUE once it exceeds the TTL", {
  reset_powershell_cache()
  withr::defer(reset_powershell_cache())

  call_count <- 0
  with_mocked_bindings(
    check_powershell_usable = function() {
      call_count <<- call_count + 1
      TRUE
    },
    code = {
      expect_true(sfs_powershell_available())
      expect_equal(call_count, 1)

      # Simulate the cache having aged past the TTL, rather than actually
      # waiting a day.
      .sharefs_cache$powershell_usable$checked_at <-
        Sys.time() - sharefs_powershell_cache_ttl() - 1

      expect_true(sfs_powershell_available())
      expect_equal(call_count, 2)
    }
  )
})

test_that("sfs_powershell_available() does not recheck a cached TRUE within the TTL", {
  reset_powershell_cache()
  withr::defer(reset_powershell_cache())

  call_count <- 0
  with_mocked_bindings(
    check_powershell_usable = function() {
      call_count <<- call_count + 1
      TRUE
    },
    code = {
      expect_true(sfs_powershell_available())
      .sharefs_cache$powershell_usable$checked_at <-
        Sys.time() - sharefs_powershell_cache_ttl() + 60 # just under the TTL
      expect_true(sfs_powershell_available())
    }
  )

  expect_equal(call_count, 1)
})

# --- run_powershell() --------------------------------------------------------

test_that("run_powershell() errors clearly if the command would be too long to run", {
  with_mocked_bindings(
    find_powershell = function() c(powershell = "/fake/powershell"),
    code = {
      expect_error(
        run_powershell(strrep("x", 40000)),
        class = "sharefs_error_command_too_long"
      )
    }
  )
})

test_that("run_powershell() errors if no PowerShell executable is found", {
  with_mocked_bindings(
    find_powershell = function() c(powershell = ""),
    code = {
      expect_error(run_powershell("Get-Date"), class = "sharefs_error_no_powershell")
    }
  )
})

test_that("run_powershell() constructs the expected processx::run() call", {
  captured <- NULL

  with_mocked_bindings(
    find_powershell = function() c(powershell = "/fake/powershell"),
    run_process = function(command, args, ...) {
      captured <<- list(command = command, args = args)
      list(status = 0L, stdout = "output", stderr = "")
    },
    code = run_powershell(c("line1", "line2"))
  )

  expect_equal(captured$command, c(powershell = "/fake/powershell"))
  expect_true("-NoProfile" %in% captured$args)
  expect_true("-NonInteractive" %in% captured$args)
  expect_true("-ExecutionPolicy" %in% captured$args)
  expect_true("Bypass" %in% captured$args)
  expect_true("-Command" %in% captured$args)
  # The whole multi-line script (with the UTF-8 preamble prepended) is
  # one array element, not split up.
  script_arg <- captured$args[length(captured$args)]
  expect_true(grepl("line1\nline2", script_arg, fixed = TRUE))
})

test_that("run_powershell() prepends a UTF-8 output-encoding preamble", {
  # Windows PowerShell 5.1 writes stdout using the system codepage when
  # redirected, silently mangling non-ASCII output. This must run before
  # any of the caller's own script lines produce output.
  captured <- NULL

  with_mocked_bindings(
    find_powershell = function() c(powershell = "/fake/powershell"),
    run_process = function(command, args, ...) {
      captured <<- args
      list(status = 0L, stdout = "output", stderr = "")
    },
    code = run_powershell("some-command")
  )

  script_arg <- captured[length(captured)]
  expect_true(grepl(
    "$OutputEncoding = [System.Text.Encoding]::UTF8",
    script_arg,
    fixed = TRUE
  ))
  expect_true(
    which(grepl("OutputEncoding", strsplit(script_arg, "\n")[[1]], fixed = TRUE))[1] <
      which(grepl("some-command", strsplit(script_arg, "\n")[[1]], fixed = TRUE))[1]
  )
})

test_that("run_powershell() errors on a non-zero exit status", {
  with_mocked_bindings(
    find_powershell = function() c(powershell = "/fake/powershell"),
    run_process = function(...) list(status = 1L, stdout = "", stderr = "some error output"),
    code = {
      expect_error(run_powershell("some-command"), class = "sharefs_error_powershell_failed")
    }
  )
})

test_that("run_powershell() returns the script's output on success", {
  with_mocked_bindings(
    find_powershell = function() c(powershell = "/fake/powershell"),
    run_process = function(...) list(status = 0L, stdout = "a\nb", stderr = ""),
    code = {
      expect_equal(run_powershell("some-command"), c("a", "b"))
    }
  )
})

test_that("run_powershell() does NOT treat status 0 with non-empty stderr as a failure", {
  # Get-ChildItem -Recurse continues past e.g. a permission-denied
  # subfolder and still exits 0, reporting the problem only via stderr --
  # treating any stderr as failure would discard an otherwise-successful,
  # mostly-complete listing.
  with_mocked_bindings(
    find_powershell = function() c(powershell = "/fake/powershell"),
    run_process = function(...) {
      list(status = 0L, stdout = "a\nb", stderr = "Access to the path 'X' is denied.")
    },
    code = {
      expect_warning(
        result <- run_powershell("some-command"),
        class = "sharefs_warning_powershell_partial"
      )
      expect_equal(result, c("a", "b"))
    }
  )
})

test_that("run_powershell() passes a timeout to processx::run() so a hung call can't block forever", {
  # system2() has no timeout argument at all; this locks in that the
  # processx replacement actually sets one, rather than just being
  # capable of it.
  captured_timeout <- NULL

  with_mocked_bindings(
    find_powershell = function() c(powershell = "/fake/powershell"),
    run_process = function(command, args, ..., timeout) {
      captured_timeout <<- timeout
      list(status = 0L, stdout = "output", stderr = "")
    },
    code = run_powershell("some-command")
  )

  expect_true(is.numeric(captured_timeout) && captured_timeout > 0)
})

test_that("run_powershell() surfaces stderr containing literal braces without erroring", {
  # cli_warn()/cli_abort() treat every message as a glue template and
  # rescan it for {...}. PowerShell's stderr is arbitrary external text
  # (it can include file paths, which can contain literal braces) and
  # must be passed through a variable reference so it's substituted once
  # rather than re-parsed as R code.
  with_mocked_bindings(
    find_powershell = function() c(powershell = "/fake/powershell"),
    run_process = function(...) {
      list(status = 0L, stdout = "a", stderr = "error near '{some_object}' in path")
    },
    code = {
      expect_warning(
        result <- run_powershell("some-command"),
        class = "sharefs_warning_powershell_partial"
      )
      expect_equal(result, "a")
    }
  )
})

test_that("run_powershell() surfaces a failure message containing literal braces without erroring", {
  with_mocked_bindings(
    find_powershell = function() c(powershell = "/fake/powershell"),
    run_process = function(...) {
      list(status = 1L, stdout = "", stderr = "error near '{some_object}' in path")
    },
    code = {
      expect_error(run_powershell("some-command"), class = "sharefs_error_powershell_failed")
    }
  )
})

test_that("run_powershell() propagates a processx-level failure (e.g. spawn/timeout) distinctly", {
  with_mocked_bindings(
    find_powershell = function() c(powershell = "/fake/powershell"),
    run_process = function(...) stop("spawn or timeout failure"),
    code = {
      expect_error(run_powershell("some-command"), class = "sharefs_error_powershell_execution_failed")
    }
  )
})

test_that("run_powershell() surfaces a spawn-failure message containing literal braces without erroring", {
  with_mocked_bindings(
    find_powershell = function() c(powershell = "/fake/powershell"),
    run_process = function(...) stop("failed near '{some_object}'"),
    code = {
      expect_error(run_powershell("some-command"), class = "sharefs_error_powershell_execution_failed")
    }
  )
})
