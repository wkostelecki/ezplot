context("secondary_plot")

test_that("base function works", {
  g = secondary_plot(mtcars, "cyl", "mpg", "hp")
  expect_equal(dim(g$data), c(3, 3))

  g = secondary_plot(mtcars, "as.character(cyl)", "mpg", "hp")
  expect_equal(dim(g$data), c(3, 4))

})
