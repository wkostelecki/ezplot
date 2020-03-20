
context("ez_labels")

test_that("ez_labels works", {
  expect_equal(ez_labels(1), "1")
  expect_equal(ez_labels(1000), "1k")
  expect_equal(ez_labels(2000000), "2m")
  expect_equal(ez_labels(1234567), "1.234567m")
  expect_equal(ez_labels(1234567, signif = 3), "1.23m")
  expect_equal(ez_labels(c(10, 2), as_factor = TRUE), factor(c(10, 2), c("2", "10")))
  expect_equal(superscript(321), "\u00B3\u00B2\u00B9")
})
