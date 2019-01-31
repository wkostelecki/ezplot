
context("line_plot")

test_that("line_plot works", {
  g = line_plot(mtcars, "cyl", "1")
  expect_equal(g[["data"]][["x"]], c(4, 6, 8))
  expect_equal(g[["data"]][["y"]], c(11, 7, 14))
  expect_equal(names(g[["data"]]), c("x", "y"))
})
