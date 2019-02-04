context("side_plot")

test_that("side_plot works", {

  g = side_plot(mtcars, "cyl", "carb", reorder = FALSE)

  expect_equal(g[["data"]]$x, factor(c(4, 6, 8), c(8, 6, 4)))
  expect_equal(g[["data"]]$y, c(17, 24, 49))

})
