context("ez_col")

test_that("ez_col works", {
  expect_equal(ez_col(3), c("dodgerblue4", "olivedrab3", "mediumorchid4"))
  expect_equal(ez_col(9), c("#104E8B", "#558D5E", "#9ACD32",
                            "#7A378B", "#EE5C42", "#FFB90F",
                            "#228B22", "#5CACEE", "#CD3333"))
  expect_equal(length(ez_col(20)), 20)
})
