# fs::path_abs()/path_norm() are purely lexical: a DFS namespace path is
# preserved exactly as given, never resolved down to one physical target
# server, so DFS failover isn't silently lost. Converted to backslash
# form since that's what Get-ChildItem/robocopy expect.
to_windows_path <- function(path) {
  windows_path <- gsub("/", "\\", fs::path_norm(fs::path_abs(path)), fixed = TRUE)
  # Explicitly plain character: gsub() actually preserves fs_path's
  # class here rather than stripping it, and this function should
  # always return a plain string, not leave that ambiguous for callers.
  unname(as.character(windows_path))
}
