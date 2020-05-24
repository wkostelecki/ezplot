context("roc_plot")


test_that("base functionality works", {
  df = data.frame(y = c(1, 0, 1),
             yhat = c(0.5, 0.8, 0.9),
             letter = c("a", "b", "b"))

  g = roc_plot(df, "yhat", "y")
  expect_equal(g[["data"]]$y, c(0, 0.5, 0.5, 1))
  expect_equal(g[["data"]]$x, c(0, 0, 1, 1))

  g = roc_plot(df, "yhat", "y", group = "letter")
  expect_equal(g[["data"]]$y, c(NA, 0, 1, 1))
  expect_equal(g[["data"]]$x, c(NA, 0, 0, 1))


})

test_that("ROC calculation works", {
  expect_equal(ezplot:::roc(0, 1),
               data.frame(x = NA,
                          y = NA,
                          auc = NA))
})
