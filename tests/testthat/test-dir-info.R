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

test_that("validate_dir_info_type() accepts 'any' and known types, rejects unknown ones", {
   expect_equal(validate_dir_info_type("any"), "any")
   expect_equal(
      validate_dir_info_type(c("file", "directory")),
      c("file", "directory")
   )
   expect_error(
      validate_dir_info_type("nonsense"),
      class = "sharefs_error_bad_type"
   )
   # A mix of valid and invalid values should still error.
   expect_error(
      validate_dir_info_type(c("file", "nonsense")),
      class = "sharefs_error_bad_type"
   )
})

test_that("sfs_dir_info() defaults path to the current working directory", {
   dir <- withr::local_tempdir()
   file.create(file.path(dir, "a.txt"))
   withr::local_dir(dir)

   info <- sfs_dir_info()

   expect_equal(basename(info$path), "a.txt")
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
   expect_equal(
      resolve_dir_info_pattern(NULL, "*.csv"),
      utils::glob2rx("*.csv")
   )
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

test_that("filter_dir_info() accepts a vector of multiple types", {
   info <- make_info(c("a", "b", "c"), c("file", "directory", "symlink"))
   result <- filter_dir_info(info, c("file", "symlink"), NULL, FALSE)
   expect_setequal(result$path, c("a", "c"))
})

test_that("filter_dir_info() passes ... through to grepl (e.g. ignore.case)", {
   info <- make_info(c("A.CSV", "b.txt"), "file")
   expect_equal(
      filter_dir_info(info, "any", "[.]csv$", FALSE, ignore.case = TRUE)$path,
      "A.CSV"
   )
   expect_equal(nrow(filter_dir_info(info, "any", "[.]csv$", FALSE)), 0)
})

# --- fs backend (no PowerShell involved) ------------------------------------

test_that("dir_info_fs() all = FALSE excludes hidden files, all = TRUE includes them", {
   dir <- withr::local_tempdir()
   file.create(file.path(dir, c(".hidden", "visible.txt")))

   default_info <- dir_info_fs(dir, all = FALSE, recurse = FALSE)
   expect_equal(
      basename(default_info$path[default_info$type == "file"]),
      "visible.txt"
   )

   all_info <- dir_info_fs(dir, all = TRUE, recurse = FALSE)
   expect_setequal(
      basename(all_info$path[all_info$type == "file"]),
      c(".hidden", "visible.txt")
   )
})

test_that("dir_info_fs() respects recurse", {
   dir <- withr::local_tempdir()
   subdir <- file.path(dir, "sub")
   dir.create(subdir)
   file.create(file.path(dir, "top.txt"))
   file.create(file.path(subdir, "nested.txt"))

   top_only <- dir_info_fs(dir, all = FALSE, recurse = FALSE)
   expect_equal(basename(top_only$path[top_only$type == "file"]), "top.txt")

   recursive <- dir_info_fs(dir, all = FALSE, recurse = TRUE)
   expect_setequal(
      basename(recursive$path[recursive$type == "file"]),
      c("top.txt", "nested.txt")
   )
})

test_that("dir_info_fs() returns the expected column set", {
   dir <- withr::local_tempdir()
   file.create(file.path(dir, "a.txt"))

   expect_equal(
      names(dir_info_fs(dir, all = FALSE, recurse = FALSE)),
      dir_info_columns()
   )
})

test_that("sfs_dir_info() retries dir_info_powershell() 5 times before falling back to fs", {
   call_count <- 0
   local_mocked_bindings(
      sfs_powershell_available = function() TRUE,
      dir_info_powershell = function(...) {
         call_count <<- call_count + 1
         stop("simulated powershell failure")
      }
   )
   local_mocked_bindings(
      Sys.sleep = function(...) invisible(NULL),
      .package = "base"
   )

   dir <- withr::local_tempdir()
   file.create(file.path(dir, "a.txt"))

   expect_warning(
      info <- sfs_dir_info(dir, type = "file"),
      class = "sharefs_warning_powershell_fallback"
   )
   expect_equal(basename(info$path), "a.txt") # still correct, via the fs fallback
   expect_equal(call_count, 5) # retried, not just one attempt then immediate fallback
})

test_that("sfs_dir_info() recovers from a transient powershell failure without ever falling back", {
   call_count <- 0
   local_mocked_bindings(
      sfs_powershell_available = function() TRUE,
      dir_info_powershell = function(path, all, recurse) {
         call_count <<- call_count + 1
         if (call_count < 2) {
            stop("simulated transient failure")
         }
         tibble::tibble(
            path = file.path(path, "a.txt"),
            type = factor("file", levels = dir_info_type_levels()),
            size = 1,
            modification_time = Sys.time(),
            access_time = Sys.time(),
            birth_time = Sys.time()
         )
      }
   )
   local_mocked_bindings(
      Sys.sleep = function(...) invisible(NULL),
      .package = "base"
   )

   expect_no_warning(
      info <- sfs_dir_info(withr::local_tempdir(), type = "file")
   )
   expect_equal(call_count, 2)
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
   expected_mtime <- file.info(file.path(dir1, "a.txt"))$mtime

   info <- sfs_dir_info(c(dir1, dir2)) # path vectorization: one real call

   expect_s3_class(info, "tbl_df")
   expect_equal(
      names(info),
      c(
         "path",
         "type",
         "size",
         "modification_time",
         "access_time",
         "birth_time"
      )
   )
   expect_setequal(basename(info$path), c("a.txt", "subdir", "b.txt"))

   file_row <- info[basename(info$path) == "a.txt", ]
   expect_equal(as.character(file_row$type), "file")
   expect_true(file_row$size > 0)
   # Explicit second-level timestamp formatting in the powershell path
   # means sub-second precision isn't preserved; a small tolerance covers
   # that (and any filesystem rounding) without making the check
   # meaningless.
   expect_true(abs(as.numeric(file_row$modification_time - expected_mtime)) < 2)

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
   expect_equal(
      as.character(info$type[basename(info$path) == "subdir"]),
      "directory"
   )
})

test_that("dir_info_powershell() all = FALSE excludes attribute-hidden files, all = TRUE includes them", {
   skip_if_not(sfs_powershell_available())

   dir <- withr::local_tempdir()
   hidden <- file.path(dir, "hidden.txt")
   file.create(hidden)
   file.create(file.path(dir, "visible.txt"))

   # Windows hides files via the Hidden attribute, not a dot-prefixed
   # name, so it has to be set explicitly for -Force to have anything to
   # reveal. Set via run_powershell() itself (already exercised
   # elsewhere in this file) rather than a separate attrib.exe call, to
   # avoid a second, less-tested quoting/invocation path.
   set_hidden <- tryCatch(
      {
         run_powershell(sprintf(
            "(Get-Item -LiteralPath '%s').Attributes = 'Hidden'",
            escape_ps_string(hidden)
         ))
         TRUE
      },
      error = function(e) FALSE
   )
   skip_if_not(set_hidden, "couldn't set the Hidden attribute")

   default_info <- dir_info_powershell(dir, all = FALSE, recurse = FALSE)
   all_info <- dir_info_powershell(dir, all = TRUE, recurse = FALSE)

   expect_false("hidden.txt" %in% basename(default_info$path))
   expect_true("hidden.txt" %in% basename(all_info$path))
})

test_that("dir_info_powershell() respects recurse", {
   skip_if_not(sfs_powershell_available())

   dir <- withr::local_tempdir()
   subdir <- file.path(dir, "sub")
   dir.create(subdir)
   file.create(file.path(dir, "top.txt"))
   file.create(file.path(subdir, "nested.txt"))

   top_only <- dir_info_powershell(dir, all = FALSE, recurse = FALSE)
   expect_equal(basename(top_only$path[top_only$type == "file"]), "top.txt")

   recursive <- dir_info_powershell(dir, all = FALSE, recurse = TRUE)
   expect_setequal(
      basename(recursive$path[recursive$type == "file"]),
      c("top.txt", "nested.txt")
   )
})

test_that("dir_info_powershell() correctly parses tricky file names (commas, non-ASCII)", {
   skip_if_not(sfs_powershell_available())

   # Regression test: commas need correct CSV escaping (legal in Windows
   # filenames), and non-ASCII names were previously mangled because
   # Windows PowerShell 5.1 writes stdout using the system's legacy
   # codepage when redirected, not UTF-8 -- confirmed against a real run
   # before run_powershell() forced UTF-8 output explicitly.
   dir <- withr::local_tempdir()
   names <- c(
      "a,b.txt",
      "cafe_\u00e9.txt",
      "\u65e5\u672c\u8a9e.txt",
      "\u0420\u0443\u0441.txt"
   )
   for (nm in names) {
      writeLines("test", file.path(dir, nm))
   }

   info <- dir_info_powershell(dir, all = FALSE, recurse = FALSE)

   expect_setequal(basename(info$path), names)
})

test_that("sfs_dir_info() identifies symlinks", {
   dir <- withr::local_tempdir()
   target <- file.path(dir, "target.txt")
   link <- file.path(dir, "link.txt")
   writeLines("hello", target)

   created <- tryCatch(
      isTRUE(file.symlink(target, link)),
      error = function(e) FALSE,
      warning = function(w) FALSE
   )
   skip_if_not(
      created,
      "couldn't create a symlink (may need admin rights or Developer Mode)"
   )

   info <- sfs_dir_info(dir, type = "symlink")

   expect_equal(basename(info$path), "link.txt")
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
