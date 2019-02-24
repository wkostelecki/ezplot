context("distribution_plot")

test_that("base function works", {

  g = distribution_plot(mtcars, "mpg")

  expect_equal(nrow(g$data), 32)

})
