

context("agg_data2")

test_that("", {

  agg_data(mtcars,
           c(x = "factor(cyl)", y = "hp", z = "factor(am)"),
           group_by = c(x = "factor(cyl)", z = "factor(am)"))

  agg_data(mtcars,
           c(x = "factor(cyl)", y = "hp", z = "factor(am)"),
           group_by = c(z = "factor(am)", x = "factor(cyl)"))

})

