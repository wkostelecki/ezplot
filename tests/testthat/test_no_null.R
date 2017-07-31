context("no_null")



test_that("no_null works", {
  expect_true(is.null(no_null(NULL)))
  expect_true(is.null(no_null("NULL")))
  expect_equal(no_null("null"), "null")
})
