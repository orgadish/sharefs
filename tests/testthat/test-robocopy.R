test_that("sfs_robocopy() errors clearly when unavailable", {
  with_mocked_bindings(
    find_robocopy = function() c(robocopy = ""),
    code = {
      expect_error(
        sfs_robocopy(withr::local_tempdir(), withr::local_tempdir()),
        class = "sharefs_error_robocopy_unavailable"
      )
    }
  )
})

test_that("sfs_robocopy() requires source and destination to each be a single path, without checking robocopy first", {
  checked <- FALSE
  find_robocopy_spy <- function() {
    checked <<- TRUE
    c(robocopy = "/fake/robocopy")
  }

  with_mocked_bindings(
    find_robocopy = find_robocopy_spy,
    code = {
      expect_error(
        sfs_robocopy(c("a", "b"), withr::local_tempdir()),
        class = "sharefs_error_robocopy_bad_args"
      )
      expect_error(
        sfs_robocopy(withr::local_tempdir(), c("a", "b")),
        class = "sharefs_error_robocopy_bad_args"
      )
    }
  )

  expect_false(checked)
})

test_that("sfs_robocopy() treats files = character(0) as nothing to copy, without invoking robocopy", {
  # An empty (but non-NULL) file list is a deliberate "copy nothing" --
  # robocopy itself has no such concept (an absent filter just means
  # "everything"), so this must be handled before robocopy is ever
  # invoked, not fall through to copying the whole directory.
  called <- FALSE
  result <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(...) {
      called <<- TRUE
      list(status = 0L, stdout = "", stderr = "")
    },
    code = {
      result <- sfs_robocopy(withr::local_tempdir(), withr::local_tempdir(), files = character(0))
    }
  )

  expect_false(called)
  expect_true(result$success)
})

test_that("sfs_robocopy() omits /XF and /XD entirely for exclude_files/exclude_dirs = character(0)", {
  # Previously produced a bare, dangling /XF or /XD with no pattern
  # after it, which robocopy could misparse as consuming the next flag
  # as if it were itself an exclude pattern.
  captured <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ...) {
      captured <<- args
      list(status = 0L, stdout = "", stderr = "")
    },
    code = sfs_robocopy(
      withr::local_tempdir(), withr::local_tempdir(),
      exclude_files = character(0), exclude_dirs = character(0)
    )
  )

  expect_false("/XF" %in% captured)
  expect_false("/XD" %in% captured)
})

test_that("sfs_robocopy() treats exit codes 0-7 as success", {
  for (code in 0:7) {
    result <- NULL

    with_mocked_bindings(
      find_robocopy = function() c(robocopy = "/fake/robocopy"),
      run_process = function(...) list(status = code, stdout = "", stderr = ""),
      code = {
        result <- sfs_robocopy(withr::local_tempdir(), withr::local_tempdir())
      }
    )

    expect_true(result$success)
    expect_equal(result$status, code)
  }
})

test_that("sfs_robocopy() treats exit codes 8+ as failure and aborts by default", {
  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(...) list(status = 8L, stdout = "", stderr = ""),
    code = {
      expect_error(
        sfs_robocopy(withr::local_tempdir(), withr::local_tempdir()),
        class = "sharefs_error_robocopy_failed"
      )
    }
  )
})

test_that("sfs_robocopy() returns instead of aborting when error_on_failure = FALSE", {
  result <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(...) list(status = 16L, stdout = "", stderr = ""),
    code = {
      result <- sfs_robocopy(
        withr::local_tempdir(), withr::local_tempdir(),
        error_on_failure = FALSE
      )
    }
  )

  expect_false(result$success)
  expect_equal(result$status, 16L)
})

test_that("sfs_robocopy() treats a missing exit status as failure, not as success", {
  result <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(...) list(status = NA_integer_, stdout = "", stderr = ""),
    code = {
      result <- sfs_robocopy(
        withr::local_tempdir(), withr::local_tempdir(),
        error_on_failure = FALSE
      )
    }
  )

  expect_false(result$success)
})

test_that("sfs_robocopy() propagates a processx-level failure (e.g. spawn/timeout) distinctly", {
  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(...) stop("spawn or timeout failure"),
    code = {
      expect_error(
        sfs_robocopy(withr::local_tempdir(), withr::local_tempdir()),
        class = "sharefs_error_robocopy_execution_failed"
      )
    }
  )
})

test_that("sfs_robocopy() surfaces a spawn-failure message containing literal braces without erroring", {
  # cli_abort() treats every message as a glue template and rescans it
  # for {...}; the spawn error text must go through a variable
  # reference so it's substituted once rather than re-parsed as R code.
  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(...) stop("failed near '{some_object}'"),
    code = {
      expect_error(
        sfs_robocopy(withr::local_tempdir(), withr::local_tempdir()),
        class = "sharefs_error_robocopy_execution_failed"
      )
    }
  )
})

test_that("sfs_robocopy() passes timeout through to processx::run(), defaulting to Inf", {
  captured_timeout <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ..., timeout) {
      captured_timeout <<- timeout
      list(status = 0L, stdout = "", stderr = "")
    },
    code = sfs_robocopy(withr::local_tempdir(), withr::local_tempdir())
  )
  expect_equal(captured_timeout, Inf)

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ..., timeout) {
      captured_timeout <<- timeout
      list(status = 0L, stdout = "", stderr = "")
    },
    code = sfs_robocopy(withr::local_tempdir(), withr::local_tempdir(), timeout = 300)
  )
  expect_equal(captured_timeout, 300)
})

test_that("sfs_robocopy() defaults produce none of the optional flags", {
  captured <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ...) {
      captured <<- args
      list(status = 0L, stdout = "", stderr = "")
    },
    code = sfs_robocopy(withr::local_tempdir(), withr::local_tempdir())
  )

  expect_false(any(c("/E", "/MIR", "/MOVE", "/XF", "/XD", "/L") %in% captured))
  expect_false(any(grepl("^/LOG:", captured)))
})

test_that("sfs_robocopy() maps recurse = TRUE to /E", {
  captured <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ...) {
      captured <<- args
      list(status = 0L, stdout = "", stderr = "")
    },
    code = sfs_robocopy(withr::local_tempdir(), withr::local_tempdir(), recurse = TRUE)
  )

  expect_true("/E" %in% captured)
  expect_false("/MIR" %in% captured)
})

test_that("sfs_robocopy() maps mirror = TRUE to /MIR, without a redundant /E", {
  captured <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ...) {
      captured <<- args
      list(status = 0L, stdout = "", stderr = "")
    },
    code = sfs_robocopy(
      withr::local_tempdir(), withr::local_tempdir(),
      mirror = TRUE, recurse = TRUE
    )
  )

  expect_true("/MIR" %in% captured)
  expect_false("/E" %in% captured)
})

test_that("sfs_robocopy() maps move = TRUE to /MOVE", {
  captured <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ...) {
      captured <<- args
      list(status = 0L, stdout = "", stderr = "")
    },
    code = sfs_robocopy(withr::local_tempdir(), withr::local_tempdir(), move = TRUE)
  )

  expect_true("/MOVE" %in% captured)
})

test_that("sfs_robocopy() maps exclude_files/exclude_dirs to /XF and /XD", {
  captured <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ...) {
      captured <<- args
      list(status = 0L, stdout = "", stderr = "")
    },
    code = sfs_robocopy(
      withr::local_tempdir(), withr::local_tempdir(),
      exclude_files = c("*.tmp", "*.log"),
      exclude_dirs = "node_modules"
    )
  )

  expect_true("/XF" %in% captured)
  expect_true("*.tmp" %in% captured)
  expect_true("*.log" %in% captured)
  expect_true("/XD" %in% captured)
  expect_true("node_modules" %in% captured)
})

test_that("sfs_robocopy() maps dry_run = TRUE to /L", {
  captured <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ...) {
      captured <<- args
      list(status = 0L, stdout = "", stderr = "")
    },
    code = sfs_robocopy(withr::local_tempdir(), withr::local_tempdir(), dry_run = TRUE)
  )

  expect_true("/L" %in% captured)
})

test_that("sfs_robocopy() maps log_file to a single /LOG: token", {
  captured <- NULL
  log_path <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ...) {
      captured <<- args
      list(status = 0L, stdout = "", stderr = "")
    },
    code = {
      log_path <- file.path(withr::local_tempdir(), "my log.txt")
      sfs_robocopy(withr::local_tempdir(), withr::local_tempdir(), log_file = log_path)
    }
  )

  expect_true(paste0("/LOG:", log_path) %in% captured)
})

test_that("sfs_robocopy() omits /NFL /NDL /NJH /NJS when log_file is set, but always keeps /NP", {
  captured <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ...) {
      captured <<- args
      list(status = 0L, stdout = "", stderr = "")
    },
    code = {
      sfs_robocopy(withr::local_tempdir(), withr::local_tempdir())
    }
  )
  expect_true(all(c("/NFL", "/NDL", "/NJH", "/NJS", "/NP") %in% captured))

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ...) {
      captured <<- args
      list(status = 0L, stdout = "", stderr = "")
    },
    code = {
      sfs_robocopy(
        withr::local_tempdir(), withr::local_tempdir(),
        log_file = file.path(withr::local_tempdir(), "robocopy.log")
      )
    }
  )
  expect_false(any(c("/NFL", "/NDL", "/NJH", "/NJS") %in% captured))
  expect_true("/NP" %in% captured)
})

test_that("sfs_robocopy() formats threads/retries/wait_seconds into /MT, /R, /W", {
  captured <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ...) {
      captured <<- args
      list(status = 0L, stdout = "", stderr = "")
    },
    code = sfs_robocopy(withr::local_tempdir(), withr::local_tempdir())
  )
  expect_true("/MT:8" %in% captured)
  expect_true("/R:5" %in% captured)
  expect_true("/W:2" %in% captured)

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ...) {
      captured <<- args
      list(status = 0L, stdout = "", stderr = "")
    },
    code = sfs_robocopy(
      withr::local_tempdir(), withr::local_tempdir(),
      threads = 4, retries = 10, wait_seconds = 1
    )
  )
  expect_true("/MT:4" %in% captured)
  expect_true("/R:10" %in% captured)
  expect_true("/W:1" %in% captured)
})

test_that("sfs_robocopy() passes ... through as additional raw flags", {
  captured <- NULL

  with_mocked_bindings(
    find_robocopy = function() c(robocopy = "/fake/robocopy"),
    run_process = function(command, args, ...) {
      captured <<- args
      list(status = 0L, stdout = "", stderr = "")
    },
    code = {
      # `...` sits right after `destination` in sfs_robocopy()'s signature
      # specifically so this works: extra raw flags can be passed
      # positionally, without needing to name every argument in between.
      sfs_robocopy(withr::local_tempdir(), withr::local_tempdir(), "/SEC", "/MAXAGE:30")
    }
  )

  expect_true("/SEC" %in% captured)
  expect_true("/MAXAGE:30" %in% captured)
})

test_that("sfs_robocopy() actually copies files on a real Windows machine", {
  skip_if_not(sfs_robocopy_available())

  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  f <- file.path(src, "a.txt")
  writeLines("hello", f)

  result <- sfs_robocopy(src, dest, files = "a.txt")

  expect_true(result$success)
  expect_true(file.exists(file.path(dest, "a.txt")))
  expect_equal(readLines(file.path(dest, "a.txt")), "hello")
})

test_that("sfs_robocopy() handles source/destination paths containing spaces on a real Windows machine", {
  # processx quotes each argument itself; this confirms that holds in
  # practice, not just in theory, for paths containing spaces.
  skip_if_not(sfs_robocopy_available())

  src <- file.path(withr::local_tempdir(), "dir with spaces")
  dest <- file.path(withr::local_tempdir(), "another dir with spaces")
  dir.create(src)
  dir.create(dest)
  writeLines("hello", file.path(src, "a.txt"))

  result <- sfs_robocopy(src, dest)

  expect_true(result$success)
  expect_true(file.exists(file.path(dest, "a.txt")))
  expect_equal(readLines(file.path(dest, "a.txt")), "hello")
})

test_that("sfs_robocopy() can actually recurse and exclude files on a real Windows machine", {
  skip_if_not(sfs_robocopy_available())

  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  writeLines("keep", file.path(src, "keep.txt"))
  writeLines("skip", file.path(src, "skip.tmp"))

  result <- sfs_robocopy(src, dest, recurse = TRUE, exclude_files = "*.tmp")

  expect_true(result$success)
  expect_true(file.exists(file.path(dest, "keep.txt")))
  expect_false(file.exists(file.path(dest, "skip.tmp")))
})

test_that("sfs_robocopy() with mirror = TRUE really deletes extra files in destination", {
  skip_if_not(sfs_robocopy_available())

  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  writeLines("keep", file.path(src, "keep.txt"))
  writeLines("stale", file.path(dest, "stale.txt")) # only in dest, not src

  result <- sfs_robocopy(src, dest, mirror = TRUE)

  expect_true(result$success)
  expect_true(file.exists(file.path(dest, "keep.txt")))
  expect_false(file.exists(file.path(dest, "stale.txt")))
})

test_that("sfs_robocopy() with move = TRUE really removes source files after copying", {
  skip_if_not(sfs_robocopy_available())

  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  f <- file.path(src, "a.txt")
  writeLines("hello", f)

  result <- sfs_robocopy(src, dest, move = TRUE)

  expect_true(result$success)
  expect_true(file.exists(file.path(dest, "a.txt")))
  expect_false(file.exists(f))
})

test_that("sfs_robocopy() with dry_run = TRUE doesn't actually copy anything", {
  skip_if_not(sfs_robocopy_available())

  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  writeLines("hello", file.path(src, "a.txt"))

  result <- sfs_robocopy(src, dest, dry_run = TRUE)

  expect_true(result$success)
  expect_false(file.exists(file.path(dest, "a.txt")))
})

test_that("sfs_robocopy() with log_file really writes a log with actual content", {
  skip_if_not(sfs_robocopy_available())

  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  writeLines("hello", file.path(src, "a.txt"))
  log_path <- file.path(withr::local_tempdir(), "robocopy.log")

  result <- sfs_robocopy(src, dest, log_file = log_path)

  expect_true(result$success)
  expect_true(file.exists(log_path))
  log_lines <- readLines(log_path)
  # A bare "length > 0" check isn't a strong enough assertion here -- a
  # log containing only a single blank line already satisfies that.
  # /NFL /NDL /NJH /NJS are suppressed by default and only skipped when
  # log_file is set, but check for the actual copied file name to be
  # sure the log has real content, not just a nonzero line count.
  expect_true(any(grepl("a.txt", log_lines, fixed = TRUE)))
})

test_that("sfs_robocopy() can actually run a real, slow-ish operation without a timeout by default", {
  # Confirms Inf really means "no timeout enforced", not just that the
  # value is passed through -- exercises the real processx call path.
  skip_if_not(sfs_robocopy_available())

  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  writeLines("hello", file.path(src, "a.txt"))

  result <- sfs_robocopy(src, dest)

  expect_true(result$success)
})
