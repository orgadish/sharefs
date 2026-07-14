# Fallback backend: delegates to fs::dir_info() itself. Still one stat()
# syscall per file underneath, so it doesn't reduce network round trips
# the way the powershell backend does.
dir_info_fs <- function(path, all, recurse) {
   info <- fs::dir_info(path = path, all = all, recurse = recurse, type = "any")
   tibble::as_tibble(info)[dir_info_columns()]
}
