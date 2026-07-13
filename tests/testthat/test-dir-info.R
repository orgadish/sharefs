test_that("sfs_dir_info() validates path", {
  expect_error(
    sfs_dir_info(tempfile()),
    class = "sharefs_error_path_not_found"
  )
})

test_that("sfs_dir_info() validates every element of a vectorized path", {
  existing_dir <- withr::local_tempdir()
  nonexistent_dir <- tempfile()

  err <- tryCatch(
    sfs_dir_info(c(existing_dir, nonexistent_dir)),
    error = function(e) e
  )

  expect_s3_class(err, "sharefs_error_path_not_found")
  expect_true(grepl(nonexistent_dir, conditionMessage(err), fixed = TRUE))
})

test_that("sfs_dir_info() with fail = FALSE drops non-existent paths instead of erroring", {
  existing_dir <- withr::local_tempdir()
  file.create(file.path(existing_dir, "a.txt"))
  nonexistent_dir <- tempfile()

  expect_warning(
    info <- sfs_dir_info(c(existing_dir, nonexistent_dir), fail = FALSE),
    class = "sharefs_warning_path_not_found"
  )
  expect_equal(basename(info$path), "a.txt")
})

test_that("sfs_dir_info() with fail = FALSE and no valid paths returns an empty result", {
  expect_warning(
    info <- sfs_dir_info(tempfile(), fail = FALSE),
    class = "sharefs_warning_path_not_found"
  )
  expect_equal(nrow(info), 0)
  expect_equal(names(info), names(empty_dir_info()))
})

test_that("sfs_dir_info() validates type", {
  expect_error(
    sfs_dir_info(withr::local_tempdir(), type = "not_a_real_type"),
    class = "sharefs_error_bad_type"
  )
})

test_that("sfs_dir_info() errors if both regexp and glob are supplied", {
  expect_error(
    sfs_dir_info(withr::local_tempdir(), regexp = "a", glob = "*.csv"),
    class = "sharefs_error_regexp_and_glob"
  )
})

test_that("sfs_dir_info() accepts a vector of directories and combines their results", {
  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  file.create(file.path(dir1, "a.txt"))
  file.create(file.path(dir2, "b.txt"))

  info <- sfs_dir_info(c(dir1, dir2))

  expect_setequal(basename(info$path), c("a.txt", "b.txt"))
})

test_that("sfs_dir_info() returns only the backend-consistent columns", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "a.txt"))

  info <- sfs_dir_info(dir)

  expect_equal(
    names(info),
    c("path", "type", "size", "modification_time", "access_time", "birth_time")
  )
})

test_that("sfs_dir_info() lists files with metadata", {
  dir <- withr::local_tempdir()
  files <- file.path(dir, c("a.txt", "b.txt"))
  writeLines("hello", files[1])
  writeLines("world!", files[2])

  info <- sfs_dir_info(dir)

  expect_s3_class(info, "tbl_df")
  expect_setequal(basename(info$path), c("a.txt", "b.txt"))
  expect_true(is.numeric(info$size))
  expect_s3_class(info$modification_time, "POSIXct")
  expect_s3_class(info$access_time, "POSIXct")
  expect_s3_class(info$birth_time, "POSIXct")
})

test_that("sfs_dir_info() reports size 0 for directories, on either backend", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "subdir"))

  info <- sfs_dir_info(dir, type = "directory")

  expect_equal(info$size, 0)
})

test_that("sfs_dir_info() type = 'any' (default) includes both files and directories", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "a.txt"))
  dir.create(file.path(dir, "subdir"))

  info <- sfs_dir_info(dir)

  expect_setequal(as.character(info$type), c("file", "directory"))
})

test_that("sfs_dir_info() type = 'file' excludes directories", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "a.txt"))
  dir.create(file.path(dir, "subdir"))

  info <- sfs_dir_info(dir, type = "file")

  expect_equal(basename(info$path), "a.txt")
})

test_that("sfs_dir_info() type = 'directory' excludes files", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "a.txt"))
  dir.create(file.path(dir, "subdir"))

  info <- sfs_dir_info(dir, type = "directory")

  expect_equal(basename(info$path), "subdir")
})

test_that("sfs_dir_info() respects glob and recurse", {
  dir <- withr::local_tempdir()
  subdir <- file.path(dir, "sub")
  dir.create(subdir)
  file.create(file.path(dir, c("top.csv", "top.txt")))
  file.create(file.path(subdir, "nested.csv"))

  top_only <- sfs_dir_info(dir, glob = "*.csv", type = "file")
  expect_equal(basename(top_only$path), "top.csv")

  recursive <- sfs_dir_info(dir, glob = "*.csv", recurse = TRUE, type = "file")
  expect_setequal(basename(recursive$path), c("top.csv", "nested.csv"))
})

test_that("sfs_dir_info() glob with invert = TRUE returns non-matching entries", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("a.csv", "b.txt")))

  info <- sfs_dir_info(dir, glob = "*.csv", invert = TRUE, type = "file")

  expect_equal(basename(info$path), "b.txt")
})

test_that("sfs_dir_info() regexp filters like glob does", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("a.csv", "b.txt")))

  info <- sfs_dir_info(dir, regexp = "[.]csv$", type = "file")

  expect_equal(basename(info$path), "a.csv")
})

test_that("dir_info_fs() all = FALSE excludes hidden files, all = TRUE includes them", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c(".hidden", "visible.txt")))

  default_info <- dir_info_fs(dir, all = FALSE, recurse = FALSE)
  expect_equal(basename(default_info$path[default_info$type == "file"]), "visible.txt")

  all_info <- dir_info_fs(dir, all = TRUE, recurse = FALSE)
  expect_setequal(
    basename(all_info$path[all_info$type == "file"]),
    c(".hidden", "visible.txt")
  )
})

test_that("sfs_dir_info() handles an empty directory", {
  dir <- withr::local_tempdir()

  info <- sfs_dir_info(dir)

  expect_equal(nrow(info), 0)
  expect_equal(names(info), names(empty_dir_info()))
})

test_that("sfs_dir_info() falls back to fs with a warning if powershell fails", {
  local_mocked_bindings(
    sfs_powershell_available = function() TRUE,
    dir_info_powershell = function(...) stop("simulated powershell failure")
  )

  dir <- withr::local_tempdir()
  file.create(file.path(dir, "a.txt"))

  expect_warning(
    info <- sfs_dir_info(dir, type = "file"),
    class = "sharefs_warning_powershell_fallback"
  )
  expect_equal(basename(info$path), "a.txt")
})

test_that("sfs_dir_info() uses powershell when it's available, without erroring", {
  skip_if_not(sfs_powershell_available())

  dir <- withr::local_tempdir()
  files <- file.path(dir, c("a.txt", "b.txt"))
  writeLines("hello", files[1])
  writeLines("world!", files[2])

  info <- sfs_dir_info(dir, type = "file")

  expect_setequal(basename(info$path), c("a.txt", "b.txt"))
})

test_that("dir_info_powershell() batches multiple directories into one call", {
  skip_if_not(sfs_powershell_available())

  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  file.create(file.path(dir1, "a.txt"))
  file.create(file.path(dir2, "b.txt"))

  info <- dir_info_powershell(c(dir1, dir2), all = FALSE, recurse = FALSE)

  expect_setequal(basename(info$path), c("a.txt", "b.txt"))
})

test_that("dir_info_powershell() correctly classifies files vs. directories", {
  skip_if_not(sfs_powershell_available())

  dir <- withr::local_tempdir()
  file.create(file.path(dir, "a.txt"))
  dir.create(file.path(dir, "subdir"))

  info <- dir_info_powershell(dir, all = FALSE, recurse = FALSE)

  expect_equal(
    as.character(info$type[basename(info$path) == "a.txt"]),
    "file"
  )
  expect_equal(
    as.character(info$type[basename(info$path) == "subdir"]),
    "directory"
  )
})

# --- sfs_dir_ls() ---------------------------------------------------------------

test_that("sfs_dir_ls() returns just the paths from sfs_dir_info()", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("a.txt", "b.txt")))

  expect_equal(sfs_dir_ls(dir, type = "file"), sfs_dir_info(dir, type = "file")$path)
})

test_that("sfs_dir_ls() supports the same filtering arguments as sfs_dir_info()", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("a.csv", "b.txt")))

  expect_equal(basename(sfs_dir_ls(dir, glob = "*.csv", type = "file")), "a.csv")
})
