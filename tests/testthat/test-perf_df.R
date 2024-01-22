context("perf_df")

test_that("perf_df works", {

  df1 = perf_df(mtcars$mpg, mtcars$am)

  expect_equal(df1$cutoff, c(Inf, sort(unique(mtcars$mpg), decreasing = TRUE)))
  expect_true(all(with(df1, fp+tp+tn+fn) == 32))

  df2 = perf_df(mtcars$mpg, mtcars$am, quantiles = 4)
  expect_equal(df2$pp, c(8, 16, 24, 32))
  expect_equal(df2$cutoff, sort(mtcars$mpg, decreasing = TRUE)[c(8, 16, 24, 32)])
  expect_equal(df2$rpp, c(0.25, 0.5, 0.75, 1))

  df3 = perf_df(mtcars$mpg, mtcars$am, quantiles = 10)
  expect_equal(nrow(df3), 10)

  df4 = perf_df(mtcars$mpg, mtcars$am, quantiles = 31)
  expect_equal(nrow(df4), 31)

  df5 = perf_df(mtcars$mpg, mtcars$am, quantiles = 33)
  expect_equal(nrow(df5), 32)

})
