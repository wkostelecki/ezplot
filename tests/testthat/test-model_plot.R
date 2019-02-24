context("model_plot")

test_that("model_plot works", {

  y = 1:26
  df = data.frame(ID = 1:26,
                  actual = y + rep(c(-1, 0, 1, 0), 7)[c(-1,-2)],
                  fitted = y,
                  id = letters)
  g = model_plot(df, "ID", "actual", "fitted")

  expect_equal(nrow(g$data), length(y))

  g = model_plot(df, "id", "actual", "fitted",
                 facet_x = "id %in% letters[1:13]",
                 res_bins = 20)

  expect_equal(nrow(g$data), length(y))

})
