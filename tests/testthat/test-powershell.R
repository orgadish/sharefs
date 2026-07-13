test_that("escape_ps_string() doubles single quotes", {
  expect_equal(escape_ps_string("it's"), "it''s")
  expect_equal(escape_ps_string("no quotes"), "no quotes")
  expect_equal(escape_ps_string("''"), "''''")
})
