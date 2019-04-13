context("area_plot")

test_that("area_plot works", {
  g = area_plot(mtcars, "cyl", "1")

  expect_equal(g[["data"]]$x, c(4, 6, 8))
  expect_equal(g[["data"]]$y, c(11, 7, 14))

  g = area_plot(mtcars, "cyl", "1", "am", position = "fill")
  expect_equal(nrow(g$data), 6)

})
