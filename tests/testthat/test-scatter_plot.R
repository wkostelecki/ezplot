context("scatter_plot")


test_that("scatter_plot works", {

  g = scatter_plot(mtcars, "cyl", "wt")
  expect_error(print(g), NA)

  g = scatter_plot(mtcars, "as.character(cyl)", "wt", "'a'")
  expect_error(print(g), NA)

  g = scatter_plot(mtcars, "as.character(cyl)", "wt", "am")
  expect_error(print(g), NA)

})
