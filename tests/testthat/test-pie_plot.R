context("pie_plot")

test_that("pie_plot works", {

  g = pie_plot(mtcars, "cyl", "1")

  expect_equal(g[["data"]][["share"]], c(14, 11, 7) / 32)

})
