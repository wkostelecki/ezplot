context("classification plots")

df = data.frame(label = c(F, F, F, T, T, F, T, F, T, T, T),
                prediction = seq(0, 1, 0.1))

test_that("pr_plot", {

  g = pr_plot(df, "label", "prediction")
  expect_equal(nrow(g$data), 11)

})



test_that("lift_plot", {

  g = lift_plot(df, "label", "prediction")
  expect_equal(nrow(g$data), 11)

})
