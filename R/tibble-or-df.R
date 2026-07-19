# tibble if installed, otherwise base R data.frame
tibble_or_df <- function(...) {
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::tibble(...)
  } else {
    data.frame(..., stringsAsFactors = FALSE)
  }
}
