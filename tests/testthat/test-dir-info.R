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

# --- filter_dir_info() (pure logic: no filesystem or PowerShell involved) ---
# type/glob/regexp/invert filtering, and the directory-size-0 override, all
# live here, backend-agnostically -- so they're tested here directly against
# a synthetic tibble, rather than via a real (and much slower) end-to-end
# sfs_dir_info() call for every case.

make_info <- function(path, type, size = 1) {
  tibble::tibble(
    path = path,
    type = factor(type, levels = dir_info_type_levels()),
    size = size,
    modification_time = as.POSIXct("2024-01-01"),
    access_time = as.POSIXct("2024-01-01"),
    birth_time = as.POSIXct("2024-01-01")
  )
}

test_that("resolve_dir_info_pattern() converts glob to regexp and enforces mutual exclusivity", {
  expect_equal(resolve_dir_info_pattern(NULL, "*.csv"), utils::glob2rx("*.csv"))
  expect_null(resolve_dir_info_pattern(NULL, NULL))
  expect_equal(resolve_dir_info_pattern("some_regexp", NULL), "some_regexp")
  expect_error(
    resolve_dir_info_pattern("a", "*.csv"),
    class = "sharefs_error_regexp_and_glob"
  )
})

test_that("filter_dir_info() type = 'any' keeps every type", {
  info <- make_info(c("a", "b"), c("file", "directory"))
  expect_equal(nrow(filter_dir_info(info, "any", NULL, FALSE)), 2)
})

test_that("filter_dir_info() type = 'file'/'directory' filters correctly", {
  info <- make_info(c("a", "b"), c("file", "directory"))
  expect_equal(filter_dir_info(info, "file", NULL, FALSE)$path, "a")
  expect_equal(filter_dir_info(info, "directory", NULL, FALSE)$path, "b")
})

test_that("filter_dir_info() forces size to 0 for directories", {
  info <- make_info("d", "directory", size = 999)
  expect_equal(filter_dir_info(info, "any", NULL, FALSE)$size, 0)
})

test_that("filter_dir_info() regexp filters by path", {
  info <- make_info(c("a.csv", "b.txt"), "file")
  expect_equal(filter_dir_info(info, "any", "[.]csv$", FALSE)$path, "a.csv")
})

test_that("filter_dir_info() invert = TRUE flips the match", {
  info <- make_info(c("a.csv", "b.txt"), "file")
  expect_equal(filter_dir_info(info, "any", "[.]csv$", TRUE)$path, "b.txt")
})

# --- fs backend (no PowerShell involved) ------------------------------------

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

# --- real end-to-end dispatch ------------------------------------------------
# Each call here spawns a real backend process (PowerShell, if available) on
# a real Windows runner -- the actual cost driver for this file. Consolidated
# into as few real calls as coverage allows; anything that's really about
# filter_dir_info()'s logic is tested above instead.

test_that("sfs_dir_info() lists real files and directories with correct metadata", {
  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  writeLines("hello", file.path(dir1, "a.txt"))
  dir.create(file.path(dir1, "subdir"))
  file.create(file.path(dir2, "b.txt"))

  info <- sfs_dir_info(c(dir1, dir2)) # path vectorization: one real call

  expect_s3_class(info, "tbl_df")
  expect_equal(
    names(info),
    c("path", "type", "size", "modification_time", "access_time", "birth_time")
  )
  expect_setequal(basename(info$path), c("a.txt", "subdir", "b.txt"))

  file_row <- info[basename(info$path) == "a.txt", ]
  expect_equal(as.character(file_row$type), "file")
  expect_true(file_row$size > 0)

  dir_row <- info[basename(info$path) == "subdir", ]
  expect_equal(as.character(dir_row$type), "directory")
  expect_equal(dir_row$size, 0)

  expect_s3_class(info$modification_time, "POSIXct")
  expect_s3_class(info$access_time, "POSIXct")
  expect_s3_class(info$birth_time, "POSIXct")
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

test_that("sfs_dir_info() handles an empty directory", {
  dir <- withr::local_tempdir()

  info <- sfs_dir_info(dir)

  expect_equal(nrow(info), 0)
  expect_equal(names(info), names(empty_dir_info()))
})

test_that("sfs_dir_info() recurse controls whether nested files are found", {
  dir <- withr::local_tempdir()
  subdir <- file.path(dir, "sub")
  dir.create(subdir)
  file.create(file.path(dir, "top.txt"))
  file.create(file.path(subdir, "nested.txt"))

  top_only <- sfs_dir_info(dir, type = "file")
  expect_equal(basename(top_only$path), "top.txt")

  recursive <- sfs_dir_info(dir, recurse = TRUE, type = "file")
  expect_setequal(basename(recursive$path), c("top.txt", "nested.txt"))
})

test_that("dir_info_powershell() batches multiple directories and classifies files vs. directories", {
  skip_if_not(sfs_powershell_available())

  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  file.create(file.path(dir1, "a.txt"))
  dir.create(file.path(dir1, "subdir"))
  file.create(file.path(dir2, "b.txt"))

  info <- dir_info_powershell(c(dir1, dir2), all = FALSE, recurse = FALSE)

  expect_setequal(basename(info$path), c("a.txt", "subdir", "b.txt"))
  expect_equal(as.character(info$type[basename(info$path) == "a.txt"]), "file")
  expect_equal(as.character(info$type[basename(info$path) == "subdir"]), "directory")
})

# --- sfs_dir_ls() -- mocked (a thin wrapper; no need to re-run a real
# dispatch call just to check it forwards arguments and extracts $path) ------

test_that("sfs_dir_ls() returns sfs_dir_info()'s path column and forwards arguments", {
  captured_args <- NULL
  local_mocked_bindings(
    sfs_dir_info = function(...) {
      captured_args <<- list(...)
      tibble::tibble(path = c("a", "b"))
    }
  )

  result <- sfs_dir_ls("some/dir", glob = "*.csv", type = "file")

  expect_equal(result, c("a", "b"))
  expect_equal(captured_args$path, "some/dir")
  expect_equal(captured_args$glob, "*.csv")
  expect_equal(captured_args$type, "file")
})
