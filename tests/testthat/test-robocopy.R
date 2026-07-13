test_that("robocopy() errors clearly when unavailable", {
  local_mocked_bindings(
    robocopy_available = function() FALSE
  )

  expect_error(
    robocopy(withr::local_tempdir(), withr::local_tempdir()),
    class = "sharefs_error_robocopy_unavailable"
  )
})

test_that("robocopy() treats exit codes 0-7 as success", {
  local_mocked_bindings(
    robocopy_available = function() TRUE
  )

  for (code in 0:7) {
    local_mocked_bindings(system2 = function(...) code, .package = "base")

    result <- robocopy(withr::local_tempdir(), withr::local_tempdir())

    expect_true(result$success)
    expect_equal(result$status, code)
  }
})

test_that("robocopy() treats exit codes 8+ as failure and aborts by default", {
  local_mocked_bindings(
    robocopy_available = function() TRUE
  )
  local_mocked_bindings(system2 = function(...) 8L, .package = "base")

  expect_error(
    robocopy(withr::local_tempdir(), withr::local_tempdir()),
    class = "sharefs_error_robocopy_failed"
  )
})

test_that("robocopy() returns instead of aborting when error_on_failure = FALSE", {
  local_mocked_bindings(
    robocopy_available = function() TRUE
  )
  local_mocked_bindings(system2 = function(...) 16L, .package = "base")

  result <- robocopy(
    withr::local_tempdir(), withr::local_tempdir(),
    error_on_failure = FALSE
  )

  expect_false(result$success)
  expect_equal(result$status, 16L)
})

test_that("robocopy() treats a missing exit status as failure, not as success", {
  local_mocked_bindings(
    robocopy_available = function() TRUE
  )
  local_mocked_bindings(system2 = function(...) NA_integer_, .package = "base")

  result <- robocopy(
    withr::local_tempdir(), withr::local_tempdir(),
    error_on_failure = FALSE
  )

  expect_false(result$success)
})

test_that("robocopy() defaults produce none of the optional flags", {
  local_mocked_bindings(
    robocopy_available = function() TRUE
  )
  captured <- NULL
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      captured <<- args
      0L
    },
    .package = "base"
  )

  robocopy(withr::local_tempdir(), withr::local_tempdir())

  expect_false(any(c("/E", "/MIR", "/MOVE", "/XF", "/XD", "/L") %in% captured))
  expect_false(any(grepl("^/LOG:", captured)))
})

test_that("robocopy() maps recurse = TRUE to /E", {
  local_mocked_bindings(
    robocopy_available = function() TRUE
  )
  captured <- NULL
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      captured <<- args
      0L
    },
    .package = "base"
  )

  robocopy(withr::local_tempdir(), withr::local_tempdir(), recurse = TRUE)

  expect_true("/E" %in% captured)
  expect_false("/MIR" %in% captured)
})

test_that("robocopy() maps mirror = TRUE to /MIR, without a redundant /E", {
  local_mocked_bindings(
    robocopy_available = function() TRUE
  )
  captured <- NULL
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      captured <<- args
      0L
    },
    .package = "base"
  )

  robocopy(
    withr::local_tempdir(), withr::local_tempdir(),
    mirror = TRUE, recurse = TRUE
  )

  expect_true("/MIR" %in% captured)
  expect_false("/E" %in% captured)
})

test_that("robocopy() maps move = TRUE to /MOVE", {
  local_mocked_bindings(
    robocopy_available = function() TRUE
  )
  captured <- NULL
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      captured <<- args
      0L
    },
    .package = "base"
  )

  robocopy(withr::local_tempdir(), withr::local_tempdir(), move = TRUE)

  expect_true("/MOVE" %in% captured)
})

test_that("robocopy() maps exclude_files/exclude_dirs to /XF and /XD", {
  local_mocked_bindings(
    robocopy_available = function() TRUE
  )
  captured <- NULL
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      captured <<- args
      0L
    },
    .package = "base"
  )

  robocopy(
    withr::local_tempdir(), withr::local_tempdir(),
    exclude_files = c("*.tmp", "*.log"),
    exclude_dirs = "node_modules"
  )

  expect_true("/XF" %in% captured)
  expect_true(shQuote("*.tmp") %in% captured)
  expect_true(shQuote("*.log") %in% captured)
  expect_true("/XD" %in% captured)
  expect_true(shQuote("node_modules") %in% captured)
})

test_that("robocopy() maps dry_run = TRUE to /L", {
  local_mocked_bindings(
    robocopy_available = function() TRUE
  )
  captured <- NULL
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      captured <<- args
      0L
    },
    .package = "base"
  )

  robocopy(withr::local_tempdir(), withr::local_tempdir(), dry_run = TRUE)

  expect_true("/L" %in% captured)
})

test_that("robocopy() maps log_file to a single /LOG: token", {
  local_mocked_bindings(
    robocopy_available = function() TRUE
  )
  captured <- NULL
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      captured <<- args
      0L
    },
    .package = "base"
  )

  log_path <- file.path(withr::local_tempdir(), "my log.txt")
  robocopy(withr::local_tempdir(), withr::local_tempdir(), log_file = log_path)

  expect_true(paste0("/LOG:", shQuote(log_path)) %in% captured)
})

test_that("robocopy() formats threads/retries/wait_seconds into /MT, /R, /W", {
  local_mocked_bindings(
    robocopy_available = function() TRUE
  )
  captured <- NULL
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      captured <<- args
      0L
    },
    .package = "base"
  )

  robocopy(withr::local_tempdir(), withr::local_tempdir())
  expect_true("/MT:8" %in% captured)
  expect_true("/R:5" %in% captured)
  expect_true("/W:2" %in% captured)

  robocopy(
    withr::local_tempdir(), withr::local_tempdir(),
    threads = 4, retries = 10, wait_seconds = 1
  )
  expect_true("/MT:4" %in% captured)
  expect_true("/R:10" %in% captured)
  expect_true("/W:1" %in% captured)
})

test_that("robocopy() passes ... through as additional raw flags", {
  local_mocked_bindings(
    robocopy_available = function() TRUE
  )
  captured <- NULL
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      captured <<- args
      0L
    },
    .package = "base"
  )

  # `...` sits right after `destination` in robocopy()'s signature
  # specifically so this works: extra raw flags can be passed
  # positionally, without needing to name every argument in between.
  robocopy(withr::local_tempdir(), withr::local_tempdir(), "/SEC", "/MAXAGE:30")

  expect_true("/SEC" %in% captured)
  expect_true("/MAXAGE:30" %in% captured)
})

test_that("robocopy() actually copies files on a real Windows machine", {
  skip_if_not(robocopy_available())

  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  f <- file.path(src, "a.txt")
  writeLines("hello", f)

  result <- robocopy(src, dest, files = "a.txt")

  expect_true(result$success)
  expect_true(file.exists(file.path(dest, "a.txt")))
  expect_equal(readLines(file.path(dest, "a.txt")), "hello")
})

test_that("robocopy() can actually recurse and exclude files on a real Windows machine", {
  skip_if_not(robocopy_available())

  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  writeLines("keep", file.path(src, "keep.txt"))
  writeLines("skip", file.path(src, "skip.tmp"))

  result <- robocopy(src, dest, recurse = TRUE, exclude_files = "*.tmp")

  expect_true(result$success)
  expect_true(file.exists(file.path(dest, "keep.txt")))
  expect_false(file.exists(file.path(dest, "skip.tmp")))
})

test_that("robocopy() with mirror = TRUE really deletes extra files in destination", {
  skip_if_not(robocopy_available())

  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  writeLines("keep", file.path(src, "keep.txt"))
  writeLines("stale", file.path(dest, "stale.txt")) # only in dest, not src

  result <- robocopy(src, dest, mirror = TRUE)

  expect_true(result$success)
  expect_true(file.exists(file.path(dest, "keep.txt")))
  expect_false(file.exists(file.path(dest, "stale.txt")))
})

test_that("robocopy() with move = TRUE really removes source files after copying", {
  skip_if_not(robocopy_available())

  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  f <- file.path(src, "a.txt")
  writeLines("hello", f)

  result <- robocopy(src, dest, move = TRUE)

  expect_true(result$success)
  expect_true(file.exists(file.path(dest, "a.txt")))
  expect_false(file.exists(f))
})

test_that("robocopy() with dry_run = TRUE doesn't actually copy anything", {
  skip_if_not(robocopy_available())

  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  writeLines("hello", file.path(src, "a.txt"))

  result <- robocopy(src, dest, dry_run = TRUE)

  expect_true(result$success)
  expect_false(file.exists(file.path(dest, "a.txt")))
})

test_that("robocopy() with log_file really writes a log", {
  skip_if_not(robocopy_available())

  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  writeLines("hello", file.path(src, "a.txt"))
  log_path <- file.path(withr::local_tempdir(), "robocopy.log")

  result <- robocopy(src, dest, log_file = log_path)

  expect_true(result$success)
  expect_true(file.exists(log_path))
  expect_true(length(readLines(log_path)) > 0)
})
