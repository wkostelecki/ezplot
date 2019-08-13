context("scatter_plot")


test_that("scatter_plot works", {

  g = scatter_plot(mtcars, "mpg", "wt")

  expect_equal(nrow(g[["data"]]), nrow(mtcars))

})
