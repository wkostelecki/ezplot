
context("histogram_plot")


test_that("plot works", {

  g = histogram_plot(mtcars, "mpg")

  expect_equal(nrow(g$data), 32)

})



context("density_plot")


test_that("plot works", {

  g = density_plot(mtcars, "mpg")

  expect_equal(nrow(g$data), 32)

})
