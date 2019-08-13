
context("variable_plot")


test_that("variable_plot line geom works", {

  g = variable_plot(tsibbledata::pelt, "Year", c("Lynx", "Hare"))

  expect_equal(2 * nrow(tsibbledata::pelt), nrow(g[["data"]]))

  expect_error(variable_plot(tsibbledata::ansett, "Week", "Passengers", facet_x = "Class", yoy = TRUE), NA)

})


test_that("variable_plot bar geom works", {

  expect_error(variable_plot(tsibbledata::hh_budget, "Year",
                             c("Debt\n(% of disposable income)" = "Debt",
                               "Expenditure\nGrowth (%)" = "Expenditure",
                               "Unemployment (%)" = "Unemployment"),
                             facet_x = "Country", geom = "bar"),
               NA)

  expect_error(variable_plot(tsibbledata::PBS, "Type", "Scripts", "Concession", switch = "y", geom = "col"),
               NA)

})
