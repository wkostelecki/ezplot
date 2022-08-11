
context("variable_plot")
suppressPackageStartupMessages(library(tsibble))

test_that("variable_plot line geom works", {

  g = variable_plot(tsibbledata::pelt, "Year", c("Lynx", "Hare"))
  expect_equal(2 * nrow(tsibbledata::pelt), nrow(g[["data"]]))

  g = variable_plot(tsibbledata::ansett,
                    "as.Date(Week)", "Passengers", facet_x = "Class", yoy = TRUE)
  # expect_error(print(g), NA)
  expect_equal(dim(g$data), c(742, 6))

})


test_that("variable_plot bar geom works", {
  g = variable_plot(tsibbledata::hh_budget, "Year",
                    c("Debt\n(% of disposable income)" = "Debt",
                      "Expenditure\nGrowth (%)" = "Expenditure",
                      "Unemployment (%)" = "Unemployment"),
                    facet_x = "Country", geom = "bar")
  # expect_error(print(g), NA)
  expect_equal(dim(g$data), c(264, 6))

  g = variable_plot(tsibbledata::PBS,
                    "Type", "Scripts", "Concession", switch = "y", geom = "col")
  # expect_error(print(g), NA)
  expect_equal(dim(g$data), c(4, 6))

})
