context("tile_plot")

test_that("tile_plot works", {

  g = tile_plot(mtcars, "cyl", "am", "1")
  expect_equal(g[["data"]],
               data.frame(x = c(4, 4, 6, 6, 8, 8),
                          y = c(0, 1, 0, 1, 0, 1),
                          z = c(3, 8, 4, 3, 12, 2)))

})
