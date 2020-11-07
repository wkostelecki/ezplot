context("performance_plot")

test_that("base case works", {
  g = performance_plot(mtcars, "-disp", "am")
  expect_equal(g$data$cutoff,
               c(Inf, -sort(unique(mtcars$disp))))
})
