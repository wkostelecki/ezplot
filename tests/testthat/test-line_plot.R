
context("line_plot")

test_that("line_plot works", {
  g = line_plot(mtcars, "cyl", "1")
  expect_equal(g[["data"]][["x"]], c(4, 6, 8))
  expect_equal(g[["data"]][["y"]], c(11, 7, 14))
  expect_equal(names(g[["data"]]), c("x", "y"))
})

test_that("multiple y works", {
  g = line_plot(mtcars, "cyl", c("1", "am"))
  expect_equal(g[["data"]][["x"]], c(4, 6, 8, 4, 6, 8))
  expect_equal(g[["data"]][["group"]], factor(c("1", "1", "1",
                                                "am", "am", "am"),
                                              c("1", "am")))
  expect_equal(g[["data"]][["y"]], c(11, 7, 14, 8, 3, 2))
  expect_equal(names(g[["data"]]), c("x", "group", "y"))
})

test_that("special case x-axes work", {
  df = ez_data()

  g = line_plot(df, "day", "units", yoy = TRUE)
  expect_equal(range(g[["data"]][["x"]]), c(1, 366))

  g = line_plot(df, "char", "units")
  expect_equal(g[["data"]][["x"]], c(1, 2))

})
