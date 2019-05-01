context("na_plot")

test_that("base function works", {

  g = na_plot(airquality)
  expect_equal(nrow(g$data), prod(dim(airquality)))

})
