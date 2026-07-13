test_that("sfs_stage_local() requires at least one file", {
  expect_error(sfs_stage_local(character(0)), class = "sharefs_error_no_files")
})

test_that("sfs_stage_local() requires existing files", {
  expect_error(
    sfs_stage_local(tempfile()),
    class = "sharefs_error_missing_files"
  )
})

test_that("sfs_stage_local() errors on duplicate basenames", {
  src1 <- withr::local_tempdir()
  src2 <- withr::local_tempdir()
  f1 <- file.path(src1, "a.txt")
  f2 <- file.path(src2, "a.txt")
  file.create(f1)
  file.create(f2)

  expect_error(
    sfs_stage_local(c(f1, f2)),
    class = "sharefs_error_duplicate_basenames"
  )
})

test_that("sfs_stage_local() errors clearly when robocopy is unavailable", {
  # Mocked (rather than relying on the real platform) so this is
  # deterministic everywhere, not just on a Windows box without robocopy.
  local_mocked_bindings(
    robocopy_available = function() FALSE
  )

  f <- withr::local_tempfile()
  file.create(f)

  expect_error(
    sfs_stage_local(f),
    class = "sharefs_error_robocopy_unavailable"
  )
})

test_that("stage_local_robocopy() propagates a robocopy() failure", {
  # Mocks robocopy() itself (a plain package-internal function) rather
  # than base::system2, now that stage_local_robocopy() delegates the
  # actual copy/retry/exit-code logic to it.
  local_mocked_bindings(
    robocopy = function(...) {
      cli::cli_abort(
        "simulated robocopy failure",
        class = "sharefs_error_robocopy_failed"
      )
    }
  )

  src <- withr::local_tempdir()
  f <- file.path(src, "a.txt")
  file.create(f)

  expect_error(
    stage_local_robocopy(f, withr::local_tempdir()),
    class = "sharefs_error_robocopy_failed"
  )
})

test_that("sfs_stage_local() copies files and returns local paths", {
  skip_if_not(robocopy_available())

  src <- withr::local_tempdir()
  files <- file.path(src, c("a.txt", "b.txt"))
  writeLines("hello", files[1])
  writeLines("world!", files[2])

  staged <- sfs_stage_local(files)

  expect_s3_class(staged, "tbl_df")
  expect_equal(staged$path, files)
  expect_true(all(file.exists(staged$local_path)))
  expect_equal(
    readLines(staged$local_path[staged$path == files[1]]),
    "hello"
  )

  stage_dir <- attr(staged, "sharefs_stage_dir")
  expect_true(dir.exists(stage_dir))

  sfs_stage_cleanup(staged)
  expect_false(dir.exists(stage_dir))
})

test_that("sfs_stage_local() always returns size/modification_time, even for a plain path vector", {
  skip_if_not(robocopy_available())

  src <- withr::local_tempdir()
  files <- file.path(src, c("a.txt", "b.txt"))
  writeLines("hello", files[1])
  writeLines("world!", files[2])

  staged <- sfs_stage_local(files)

  expect_true(all(c("path", "local_path", "size", "modification_time") %in% names(staged)))
  expect_true(is.numeric(staged$size))
  expect_s3_class(staged$modification_time, "POSIXct")
})

test_that("sfs_stage_local() metadata reflects the staged copy's (robocopy-preserved) timestamps", {
  skip_if_not(robocopy_available())

  src <- withr::local_tempdir()
  f <- file.path(src, "a.txt")
  writeLines("hello", f)

  staged <- sfs_stage_local(f)
  source_mtime <- file.info(f)$mtime

  # robocopy preserves timestamps by default, so the staged copy's mtime
  # should match the source's (small tolerance for filesystem timestamp
  # rounding).
  expect_true(abs(as.numeric(staged$modification_time - source_mtime)) < 2)
})

test_that("sfs_stage_local() accepts a file-info table, using only its path column to copy", {
  skip_if_not(robocopy_available())

  src <- withr::local_tempdir()
  files <- file.path(src, c("a.txt", "b.txt"))
  writeLines("hello", files[1])
  writeLines("world!", files[2])

  table <- data.frame(
    path = files,
    size = file.info(files)$size,
    modification_time = file.info(files)$mtime,
    stringsAsFactors = FALSE
  )

  staged <- sfs_stage_local(table)

  expect_equal(staged$path, table$path)
  expect_true(all(file.exists(staged$local_path)))
})

test_that("sfs_stage_local() creates the staging dir if it doesn't exist", {
  skip_if_not(robocopy_available())

  src <- withr::local_tempdir()
  f <- file.path(src, "a.txt")
  file.create(f)

  target <- file.path(withr::local_tempdir(), "nested", "stage")
  staged <- sfs_stage_local(f, dir = target)

  expect_true(dir.exists(target))
  expect_equal(staged$local_path, file.path(target, "a.txt"))
})

test_that("sfs_stage_cleanup() requires a staged tibble", {
  expect_error(
    sfs_stage_cleanup(data.frame(path = "x")),
    class = "sharefs_error_not_staged"
  )
})
