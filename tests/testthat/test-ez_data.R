context("ez_data")

test_that("base case works", {

  expected_names = c("char", "fct", "num",
                     "day", "week", "month", "year", "year2",
                     "units", "value")
  df = ez_data()

  expect_true(all(expected_names %in% names(df)))

})
