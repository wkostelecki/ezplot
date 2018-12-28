context("bar_plot")


test_that("bar_plot works", {

  g = bar_plot(mtcars, "cyl", "1")
  expect_equal(g$data$x, c(4, 6, 8))
  expect_equal(g$data$y, c(11, 7, 14))

  g = bar_plot(mtcars, "cyl", "1", "am", position = "fill")
  expect_equal(nrow(g$data), 6)

  g = bar_plot(mtcars, "cyl", "1", "am", label_pos = "top")
  expect_equal(nrow(g$data), 6)

  g = bar_plot(mtcars, "cyl", "1", position = "fill")
  expect_equal(g$data$y, c(1, 1, 1))

})
