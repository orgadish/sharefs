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
  # basename() rather than the full path: the message displays paths in
  # fs's normalized (forward-slash) form, same as sfs_dir_info()'s own
  # output, not the raw platform-native string tempfile() produced.
  expect_true(grepl(basename(nonexistent_dir), conditionMessage(err), fixed = TRUE))
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

test_that("sfs_dir_info() errors if both regexp and glob are supplied, without checking PowerShell first", {
  checked_powershell <- FALSE

  with_mocked_bindings(
    sfs_powershell_available = function() {
      checked_powershell <<- TRUE
      TRUE
    },
    code = {
      expect_error(
        sfs_dir_info(withr::local_tempdir(), regexp = "a", glob = "*.csv"),
        class = "sharefs_error_regexp_and_glob"
      )
    }
  )

  expect_false(checked_powershell)
})

test_that("validate_dir_info_type() accepts 'any' and known types, rejects unknown ones", {
  expect_equal(validate_dir_info_type("any"), "any")
  expect_equal(validate_dir_info_type(c("file", "directory")), c("file", "directory"))
  expect_error(validate_dir_info_type("nonsense"), class = "sharefs_error_bad_type")
  # A mix of valid and invalid values should still error.
  expect_error(validate_dir_info_type(c("file", "nonsense")), class = "sharefs_error_bad_type")
})

test_that("unique() does NOT preserve fs_path's class -- dedupe first, then re-wrap with fs::as_fs_path()", {
  # Confirmed empirically: unlike logical-indexing subsetting (x[i]),
  # unique() returns a plain character vector even when given an
  # fs_path. sfs_dir_info() relies on this: it deduplicates path before
  # converting to fs_path, not after.
  deduped <- unique(fs::as_fs_path(c("a", "a", "b")))
  expect_false(inherits(deduped, "fs_path"))
  expect_equal(as.character(deduped), c("a", "b"))
})

test_that("setNames() preserves fs_path's class -- sfs_dir_ls() relies on this for its named fs_path return", {
  result <- setNames(fs::as_fs_path(c("a", "b")), c("a", "b"))
  expect_s3_class(result, "fs_path")
})

test_that("sfs_dir_info() deduplicates path before listing, avoiding redundant work and duplicate rows", {
  received_path <- NULL

  with_mocked_bindings(
    sfs_powershell_available = function() TRUE,
    dir_info_powershell = function(path, all, recurse) {
      received_path <<- path
      empty_dir_info()
    },
    code = {
      dir <- withr::local_tempdir()
      sfs_dir_info(c(dir, dir, dir))
    }
  )

  expect_length(received_path, 1)
})

test_that("sfs_dir_info() defaults path to the current working directory", {
  skip_if_not(sfs_powershell_available())

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

test_that("validate_regexp_glob_exclusive() errors only when both are supplied", {
  expect_no_error(validate_regexp_glob_exclusive("a", NULL))
  expect_no_error(validate_regexp_glob_exclusive(NULL, "*.csv"))
  expect_no_error(validate_regexp_glob_exclusive(NULL, NULL))
  expect_error(
    validate_regexp_glob_exclusive("a", "*.csv"),
    class = "sharefs_error_regexp_and_glob"
  )
})

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
  expect_equal(nrow(filter_dir_info(info, "any", NULL, NULL, FALSE)), 2)
})

test_that("filter_dir_info() type = 'file'/'directory' filters correctly", {
  info <- make_info(c("a", "b"), c("file", "directory"))
  expect_equal(filter_dir_info(info, "file", NULL, NULL, FALSE)$path, "a")
  expect_equal(filter_dir_info(info, "directory", NULL, NULL, FALSE)$path, "b")
})

test_that("filter_dir_info() forces size to 0 for directories", {
  info <- make_info("d", "directory", size = 999)
  expect_equal(filter_dir_info(info, "any", NULL, NULL, FALSE)$size, 0)
})

test_that("filter_dir_info() regexp filters by path", {
  info <- make_info(c("a.csv", "b.txt"), "file")
  expect_equal(filter_dir_info(info, "any", "[.]csv$", NULL, FALSE)$path, "a.csv")
})

test_that("filter_dir_info() invert = TRUE flips the match", {
  info <- make_info(c("a.csv", "b.txt"), "file")
  expect_equal(filter_dir_info(info, "any", "[.]csv$", NULL, TRUE)$path, "b.txt")
})

test_that("filter_dir_info() accepts a vector of multiple types", {
  info <- make_info(c("a", "b", "c"), c("file", "directory", "symlink"))
  result <- filter_dir_info(info, c("file", "symlink"), NULL, NULL, FALSE)
  expect_setequal(result$path, c("a", "c"))
})

test_that("filter_dir_info() passes ... through to grepl (e.g. ignore.case)", {
  info <- make_info(c("A.CSV", "b.txt"), "file")
  expect_equal(
    filter_dir_info(info, "any", "[.]csv$", NULL, FALSE, ignore.case = TRUE)$path,
    "A.CSV"
  )
  expect_equal(nrow(filter_dir_info(info, "any", "[.]csv$", NULL, FALSE)), 0)
})

test_that("filter_dir_info() resolves glob to a regexp itself", {
  info <- make_info(c("a.csv", "b.txt"), "file")
  expect_equal(
    filter_dir_info(info, "any", NULL, "*.csv", FALSE)$path,
    "a.csv"
  )
})

test_that("filter_dir_info() enforces the regexp/glob mutual exclusivity itself", {
  info <- make_info(c("a.csv", "b.txt"), "file")
  expect_error(
    filter_dir_info(info, "any", "a", "*.csv", FALSE),
    class = "sharefs_error_regexp_and_glob"
  )
})

# --- is_permission_error() (pure logic) -------------------------------------

test_that("is_permission_error() matches permission-denied text, not other failures", {
  expect_true(is_permission_error(simpleError("UnauthorizedAccessException")))
  expect_true(is_permission_error(simpleError(
    "Access to the path 'C:\\foo\\bar' is denied."
  )))
  expect_true(is_permission_error(simpleError("PermissionDenied: something")))
  # A real path is routinely much longer than "X", and PowerShell wraps
  # this exact message onto a new line before the quoted path -- both
  # must still match.
  expect_true(is_permission_error(simpleError(
    "Access to the path 'C:\\Users\\someone\\very\\deeply\\nested\\folder\\structure\\that\\goes\\on\\for\\a\\while\\locked_subdir' is denied."
  )))
  expect_true(is_permission_error(simpleError(
    "Access to the path \n'C:\\Users\\someone\\locked_subdir' is denied."
  )))
  expect_false(is_permission_error(simpleError(
    "Cannot find path '\\\\server\\share' because it does not exist."
  )))
  expect_false(is_permission_error(simpleError("some other failure")))
})

test_that("is_permission_error() matches case-insensitively, without warning", {
  # grepl() silently ignores ignore.case whenever fixed = TRUE -- case
  # must be normalized manually instead, or this both misses lowercase
  # variants and emits a spurious warning on every single call.
  expect_no_warning(
    result <- is_permission_error(simpleError("access to the path 'x' is DENIED."))
  )
  expect_true(result)
})

test_that("is_permission_error() stops at the first literal match instead of checking all of them", {
  call_count <- 0

  with_mocked_bindings(
    grepl = function(...) {
      call_count <<- call_count + 1
      TRUE # the first literal always "matches"
    },
    .package = "base",
    code = {
      result <- is_permission_error(simpleError("irrelevant"))
    }
  )

  expect_true(result)
  expect_equal(call_count, 1)
})

# --- no fallback: PowerShell unavailable or failing always errors ---------
# fs::dir_info(recurse = TRUE) aborts entirely on one inaccessible
# subdirectory (see dir-info-impl.R), so it isn't a safe silent
# fallback -- sfs_dir_info() errors instead of switching backends.

test_that("sfs_dir_info() errors, without attempting a listing, if PowerShell is unavailable", {
  attempted <- FALSE

  with_mocked_bindings(
    sfs_powershell_available = function() FALSE,
    dir_info_powershell = function(...) {
      attempted <<- TRUE
      stop("should never be called")
    },
    code = {
      dir <- withr::local_tempdir()
      file.create(file.path(dir, "a.txt"))

      expect_error(
        sfs_dir_info(dir),
        class = "sharefs_error_powershell_unavailable"
      )
    }
  )

  expect_false(attempted)
})

test_that("sfs_dir_info() retries a transient PowerShell failure and succeeds without erroring", {
  call_count <- 0
  info <- NULL

  with_mocked_bindings(
    sfs_powershell_available = function() TRUE,
    dir_info_powershell = function(path, all, recurse) {
      call_count <<- call_count + 1
      if (call_count < 3) stop("simulated transient failure")
      tibble::tibble(
        path = file.path(path, "a.txt"),
        type = factor("file", levels = dir_info_type_levels()),
        size = 1, modification_time = Sys.time(),
        access_time = Sys.time(), birth_time = Sys.time()
      )
    },
    code = {
      with_mocked_bindings(
        Sys.sleep = function(...) invisible(NULL),
        .package = "base",
        code = {
          info <- sfs_dir_info(withr::local_tempdir(), type = "file")
        }
      )
    }
  )

  expect_equal(call_count, 3)
  expect_equal(basename(info$path), "a.txt")
})

test_that("sfs_dir_info() errors (no fallback) once PowerShell retries are exhausted", {
  call_count <- 0

  with_mocked_bindings(
    sfs_powershell_available = function() TRUE,
    dir_info_powershell = function(...) {
      call_count <<- call_count + 1
      stop("simulated persistent powershell failure")
    },
    code = {
      with_mocked_bindings(
        Sys.sleep = function(...) invisible(NULL),
        .package = "base",
        code = {
          expect_error(
            sfs_dir_info(withr::local_tempdir()),
            "simulated persistent powershell failure"
          )
        }
      )
    }
  )

  expect_equal(call_count, 5) # default retries
})

test_that("sfs_dir_info() does not retry a permission error -- fails on the first attempt", {
  call_count <- 0
  sleep_count <- 0

  with_mocked_bindings(
    sfs_powershell_available = function() TRUE,
    dir_info_powershell = function(...) {
      call_count <<- call_count + 1
      stop("Access to the path 'X' is denied.")
    },
    code = {
      with_mocked_bindings(
        Sys.sleep = function(...) sleep_count <<- sleep_count + 1,
        .package = "base",
        code = {
          expect_error(sfs_dir_info(withr::local_tempdir()), "is denied")
        }
      )
    }
  )

  expect_equal(call_count, 1)
  expect_equal(sleep_count, 0)
})

# --- real end-to-end dispatch ------------------------------------------------
# Each call here spawns a real PowerShell process on a real Windows runner --
# the actual cost driver for this file. Consolidated into as few real calls
# as coverage allows; anything that's really about filter_dir_info()'s logic
# is tested above instead. Skipped entirely if PowerShell isn't available,
# since there's no fs fallback for these to silently exercise instead.

test_that("sfs_dir_info() lists real files and directories with correct metadata", {
  skip_if_not(sfs_powershell_available())

  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  writeLines("hello", file.path(dir1, "a.txt"))
  dir.create(file.path(dir1, "subdir"))
  file.create(file.path(dir2, "b.txt"))
  expected_mtime <- file.info(file.path(dir1, "a.txt"))$mtime

  info <- sfs_dir_info(c(dir1, dir2)) # path vectorization: one real call

  expect_s3_class(info, "data.frame")
  expect_equal(
    names(info),
    c("path", "type", "size", "modification_time", "access_time", "birth_time")
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
  skip_if_not(sfs_powershell_available())

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
  skip_if_not(sfs_powershell_available())

  dir <- withr::local_tempdir()
  info <- sfs_dir_info(dir)

  expect_equal(nrow(info), 0)
  expect_equal(names(info), names(empty_dir_info()))
})

test_that("sfs_dir_info() recurse controls whether nested files are found", {
  skip_if_not(sfs_powershell_available())

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

test_that("dir_info_powershell() normalizes paths via fs::path_abs(), not base normalizePath()", {
  # fs::path_abs()/path_norm() are purely lexical and don't resolve a DFS
  # namespace path down to one physical server the way normalizePath()
  # does -- mocked here (rather than requiring a real DFS share) to
  # confirm the right function is actually being called.
  path_abs_called <- FALSE
  real_path_abs <- fs::path_abs

  with_mocked_bindings(
    run_powershell = function(script_lines) character(0),
    code = {
      with_mocked_bindings(
        path_abs = function(p) {
          path_abs_called <<- TRUE
          real_path_abs(p)
        },
        .package = "fs",
        code = {
          dir_info_powershell(withr::local_tempdir(), all = FALSE, recurse = FALSE)
        }
      )
    }
  )

  expect_true(path_abs_called)
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

test_that("dir_info_powershell() all = FALSE excludes attribute-hidden files, all = TRUE includes them", {
  skip_if_not(sfs_powershell_available())

  dir <- withr::local_tempdir()
  hidden <- file.path(dir, "hidden.txt")
  file.create(hidden)
  file.create(file.path(dir, "visible.txt"))

  # Windows hides files via the Hidden attribute, not a dot-prefixed
  # name, so it has to be set explicitly for -Force to have anything to
  # reveal. Set via run_powershell() itself (already exercised
  # elsewhere) rather than a separate attrib.exe call, to avoid a
  # second, less-tested quoting/invocation path.
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

  # Commas need correct CSV escaping (legal in Windows filenames).
  # Non-ASCII names need UTF-8 output forced explicitly: Windows
  # PowerShell 5.1 otherwise writes stdout using the system's legacy
  # codepage when redirected.
  dir <- withr::local_tempdir()
  names <- c("a,b.txt", "cafe_\u00e9.txt", "\u65e5\u672c\u8a9e.txt", "\u0420\u0443\u0441.txt")
  for (nm in names) writeLines("test", file.path(dir, nm))

  info <- dir_info_powershell(dir, all = FALSE, recurse = FALSE)

  expect_setequal(basename(info$path), names)
})

test_that("sfs_dir_info() identifies symlinks", {
  skip_if_not(sfs_powershell_available())

  dir <- withr::local_tempdir()
  target <- file.path(dir, "target.txt")
  link <- file.path(dir, "link.txt")
  writeLines("hello", target)

  created <- tryCatch(
    isTRUE(file.symlink(target, link)),
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  skip_if_not(created, "couldn't create a symlink (may need admin rights or Developer Mode)")

  info <- sfs_dir_info(dir, type = "symlink")

  expect_equal(basename(info$path), "link.txt")
})

# --- sfs_dir_ls() -- mocked (a thin wrapper; no need to re-run a real
# dispatch call just to check it forwards arguments and extracts $path) ------

test_that("sfs_dir_ls() returns sfs_dir_info()'s path column and forwards arguments", {
  captured_args <- NULL
  result <- NULL

  with_mocked_bindings(
    sfs_dir_info = function(...) {
      captured_args <<- list(...)
      tibble::tibble(path = c("a", "b"))
    },
    code = {
      result <- sfs_dir_ls("some/dir", glob = "*.csv", type = "file")
    }
  )

  expect_equal(result, c(a = "a", b = "b"))
  expect_equal(captured_args$path, "some/dir")
  expect_equal(captured_args$glob, "*.csv")
  expect_equal(captured_args$type, "file")
})
