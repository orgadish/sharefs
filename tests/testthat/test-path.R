test_that("to_windows_path() converts forward slashes to backslashes", {
   expect_equal(to_windows_path("C:/Users/someone/file.txt"), "C:\\Users\\someone\\file.txt")
})

test_that("to_windows_path() makes a relative path absolute", {
   dir <- withr::local_tempdir()
   withr::local_dir(dir)
   
   result <- to_windows_path("a.txt")
   
   expect_true(startsWith(result, to_windows_path(dir)))
   expect_true(endsWith(result, "a.txt"))
})

test_that("to_windows_path() is vectorized over multiple paths at once", {
   result <- to_windows_path(c("C:/a/b.txt", "C:/c/d.txt"))
   expect_equal(result, c("C:\\a\\b.txt", "C:\\c\\d.txt"))
})

test_that("to_windows_path() drops any names, matching the previous inline implementation", {
   result <- to_windows_path(c(x = "C:/a.txt"))
   expect_null(names(result))
})

test_that("to_windows_path() preserves a UNC-style path's identifying parts, in backslash form", {
   # A fake/non-existent path can't demonstrate actual DFS resolution --
   # there's nothing real to resolve to -- so this checks only what's
   # directly verifiable: the server/share/subpath survive intact, in
   # backslash form, not that any specific resolution is avoided.
   result <- to_windows_path("//intra.example.corp/File_Shares/someone")
   expect_true(startsWith(result, "\\\\intra.example.corp\\"))
   expect_true(grepl("File_Shares", result, fixed = TRUE))
   expect_true(grepl("someone", result, fixed = TRUE))
   expect_false(grepl("/", result, fixed = TRUE))
})
