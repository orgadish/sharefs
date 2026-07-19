test_that("sfs_stage_local() requires at least one file", {
  expect_error(sfs_stage_local(character(0)), class = "sharefs_error_no_files")
})

test_that("is_dir_info_table() recognizes a data.frame with the 3 required columns", {
  df <- data.frame(path = "a", size = 1, modification_time = Sys.time())
  expect_true(is_dir_info_table(df))
})

test_that("is_dir_info_table() accepts extra columns beyond the required 3", {
  df <- data.frame(path = "a", size = 1, modification_time = Sys.time(), type = "file")
  expect_true(is_dir_info_table(df))
})

test_that("is_dir_info_table() rejects a data.frame missing any required column", {
  now <- Sys.time()
  expect_false(is_dir_info_table(data.frame(path = "a", size = 1)))
  expect_false(is_dir_info_table(data.frame(path = "a", modification_time = now)))
  expect_false(is_dir_info_table(data.frame(size = 1, modification_time = now)))
})

test_that("is_dir_info_table() rejects non-data.frame input", {
  expect_false(is_dir_info_table(c("a", "b")))
  expect_false(is_dir_info_table(list(path = "a", size = 1, modification_time = Sys.time())))
  expect_false(is_dir_info_table(NULL))
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
  with_mocked_bindings(
    sfs_robocopy_available = function() FALSE,
    code = {
      f <- withr::local_tempfile()
      file.create(f)

      expect_error(
        sfs_stage_local(f),
        class = "sharefs_error_robocopy_unavailable"
      )
    }
  )
})

test_that("stage_local_robocopy() propagates a sfs_robocopy() failure", {
  # Mocks sfs_robocopy() itself (a plain package-internal function) rather
  # than base::system2, now that stage_local_robocopy() delegates the
  # actual copy/retry/exit-code logic to it.
  with_mocked_bindings(
    sfs_robocopy = function(...) {
      cli::cli_abort(
        "simulated robocopy failure",
        class = "sharefs_error_robocopy_failed"
      )
    },
    code = {
      src <- withr::local_tempdir()
      f <- file.path(src, "a.txt")
      file.create(f)

      expect_error(
        stage_local_robocopy(f, withr::local_tempdir()),
        class = "sharefs_error_robocopy_failed"
      )
    }
  )
})

test_that("stage_local_robocopy() calls sfs_robocopy() once per source directory, not once per file", {
  call_count <- 0

  with_mocked_bindings(
    sfs_robocopy = function(...) {
      call_count <<- call_count + 1
    },
    code = {
      dir1 <- withr::local_tempdir()
      dir2 <- withr::local_tempdir()
      files <- c(
        file.path(dir1, c("a.txt", "b.txt")),
        file.path(dir2, "c.txt")
      )

      stage_local_robocopy(files, withr::local_tempdir())
    }
  )

  expect_equal(call_count, 2)
})

test_that("stage_local_robocopy() groups mixed-case references to the same directory into one call", {
  # Unlike the previous implementation (which relied on normalizePath()'s
  # OS-level path resolution, only checkable on a real Windows machine),
  # grouping now happens on an explicit tolower() key computed in R, so
  # this is fully verifiable with a mock.
  call_count <- 0

  with_mocked_bindings(
    sfs_robocopy = function(...) call_count <<- call_count + 1,
    code = {
      dir <- withr::local_tempdir()
      files <- c(file.path(dir, "a.txt"), file.path(toupper(dir), "b.txt"))

      stage_local_robocopy(files, withr::local_tempdir())
    }
  )

  expect_equal(call_count, 1)
})

test_that("stage_local_robocopy() groups files in the same directory into one call, even referenced with different case, on a real Windows machine", {
  # The mocked test above already exercises the grouping logic itself;
  # this confirms real Windows path handling doesn't undermine it.
  skip_if_not(sfs_robocopy_available())

  src <- withr::local_tempdir()
  writeLines("a", file.path(src, "a.txt"))
  writeLines("b", file.path(src, "b.txt"))

  files <- c(file.path(src, "a.txt"), file.path(toupper(src), "b.txt"))

  call_count <- 0
  with_mocked_bindings(
    sfs_robocopy = function(...) call_count <<- call_count + 1,
    code = {
      stage_local_robocopy(files, withr::local_tempdir())
    }
  )

  expect_equal(call_count, 1)
})

test_that("sfs_stage_local() handles a source path containing spaces on a real Windows machine", {
  skip_if_not(sfs_robocopy_available())

  src <- file.path(withr::local_tempdir(), "dir with spaces")
  dir.create(src)
  f <- file.path(src, "a file.txt")
  writeLines("hello", f)

  staged <- sfs_stage_local(f)

  expect_true(file.exists(staged$local_path))
  expect_equal(readLines(staged$local_path), "hello")
})

test_that("sfs_stage_local() copies files and returns local paths", {
  skip_if_not(sfs_robocopy_available())

  src <- withr::local_tempdir()
  files <- file.path(src, c("a.txt", "b.txt"))
  writeLines("hello", files[1])
  writeLines("world!", files[2])

  staged <- sfs_stage_local(files)

  expect_s3_class(staged, "data.frame")
  expect_equal(staged$path, files)
  expect_true(all(file.exists(staged$local_path)))
  expect_equal(
    readLines(staged$local_path[staged$path == files[1]]),
    "hello"
  )
  expect_equal(
    readLines(staged$local_path[staged$path == files[2]]),
    "world!"
  )

  stage_dir <- attr(staged, "sharefs_stage_dir")
  expect_true(dir.exists(stage_dir))

  sfs_stage_cleanup(staged)
  expect_false(dir.exists(stage_dir))
})

test_that("sfs_stage_local() correctly copies content for files spanning multiple source directories", {
  skip_if_not(sfs_robocopy_available())

  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  writeLines("from dir1 a", file.path(dir1, "a.txt"))
  writeLines("from dir1 b", file.path(dir1, "b.txt"))
  writeLines("from dir2 c", file.path(dir2, "c.txt"))
  files <- c(file.path(dir1, c("a.txt", "b.txt")), file.path(dir2, "c.txt"))

  staged <- sfs_stage_local(files)

  expect_equal(nrow(staged), 3)
  expect_true(all(file.exists(staged$local_path)))
  expect_equal(readLines(staged$local_path[basename(staged$local_path) == "a.txt"]), "from dir1 a")
  expect_equal(readLines(staged$local_path[basename(staged$local_path) == "b.txt"]), "from dir1 b")
  expect_equal(readLines(staged$local_path[basename(staged$local_path) == "c.txt"]), "from dir2 c")

  sfs_stage_cleanup(staged)
})

test_that("sfs_stage_local() correctly copies non-ASCII file names and content", {
  skip_if_not(sfs_robocopy_available())
  skip_if_not(sfs_powershell_available()) # sfs_dir_info() below no longer falls back to fs

  # Regression test locking in what a real-Windows diagnostic run
  # confirmed: names and content both survive an actual robocopy
  # operation, not just a listing.
  src <- withr::local_tempdir()
  names <- c("cafe_\u00e9.txt", "\u65e5\u672c\u8a9e.txt")
  contents <- c("hello", "world")
  for (i in seq_along(names)) writeLines(contents[i], file.path(src, names[i]))

  staged <- sfs_stage_local(sfs_dir_info(src))

  expect_setequal(basename(staged$local_path), names)
  for (i in seq_along(names)) {
    local_path <- staged$local_path[basename(staged$local_path) == names[i]]
    expect_equal(readLines(local_path), contents[i])
  }

  sfs_stage_cleanup(staged)
})

test_that("sfs_stage_local() always returns size/modification_time, even for a plain path vector", {
  skip_if_not(sfs_robocopy_available())

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
  skip_if_not(sfs_robocopy_available())

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
  skip_if_not(sfs_robocopy_available())

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
  skip_if_not(sfs_robocopy_available())

  src <- withr::local_tempdir()
  f <- file.path(src, "a.txt")
  file.create(f)

  target <- file.path(withr::local_tempdir(), "nested", "stage")
  staged <- sfs_stage_local(f, dir = target)

  expect_true(dir.exists(target))
  expect_equal(staged$local_path, file.path(target, "a.txt"))
})

test_that("sfs_stage_cleanup() only removes the files it staged, not a pre-existing directory or its other contents", {
  # dir= can be a directory the caller already had other, unrelated
  # content in -- cleanup must never delete more than what sharefs
  # itself put there.
  skip_if_not(sfs_robocopy_available())

  src <- withr::local_tempdir()
  f <- file.path(src, "a.txt")
  writeLines("hello", f)

  existing_dir <- withr::local_tempdir()
  unrelated_file <- file.path(existing_dir, "unrelated.txt")
  writeLines("please don't delete me", unrelated_file)

  staged <- sfs_stage_local(f, dir = existing_dir)
  sfs_stage_cleanup(staged)

  expect_false(file.exists(staged$local_path))
  expect_true(dir.exists(existing_dir))
  expect_true(file.exists(unrelated_file))
})

test_that("sfs_stage_cleanup() removes the staging dir if sharefs created it, even when a path was supplied", {
  skip_if_not(sfs_robocopy_available())

  src <- withr::local_tempdir()
  f <- file.path(src, "a.txt")
  file.create(f)

  # dir doesn't exist yet -- sharefs creates it, so it's safe to remove
  # in full, unlike an existing directory the caller already had.
  target <- file.path(withr::local_tempdir(), "nested", "stage")
  staged <- sfs_stage_local(f, dir = target)
  sfs_stage_cleanup(staged)

  expect_false(dir.exists(target))
})

test_that("sfs_stage_cleanup() requires a staged tibble", {
  expect_error(
    sfs_stage_cleanup(data.frame(path = "x")),
    class = "sharefs_error_not_staged"
  )
})

test_that("sfs_stage_local() errors on a table missing a required column, rather than silently misbehaving", {
  src <- withr::local_tempdir()
  f <- file.path(src, "a.txt")
  file.create(f)

  # Missing modification_time: not recognized as a dir-info-shaped table
  # by is_dir_info_table(), so it falls through to being treated as the
  # path vector itself -- which a data.frame is not. Documents that this
  # fails loudly rather than doing something confusing silently.
  incomplete <- data.frame(path = f, size = 1, stringsAsFactors = FALSE)

  expect_error(sfs_stage_local(incomplete))
})
