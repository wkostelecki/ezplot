context("ez_data")

test_that("base case works", {

  expected_names = c("char", "fct", "num",
                     "day", "week", "month", "year", "year2",
                     "units", "value", "price")
  df = ez_data()

  expect_true(all(expected_names %in% names(df)))

})


test_that("ez_data() works", {

  df = ez_data(start_date = as.Date("2000-01-02"),
               end_date = as.Date("2000-01-08"),
               char = "a",
               fct = factor("1"),
               num = 10)
  expect_equal(nrow(df), 7)
})


test_that("ez_data2() works", {

  df = ez_data2(start_date = as.Date("2000-01-02"),
                end_date = as.Date("2000-01-08"),
                xsec = "apples")
  expect_equal(nrow(df), 1)
})
