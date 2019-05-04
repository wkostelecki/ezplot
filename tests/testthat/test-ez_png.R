context("ez_png")

test_that("ez_png works", {
  ez_png(ggplot2::ggplot(),
         "test.png")
  expect_true(file.exists("test.png"))
  unlink("test.png")

  expect_error(ez_png(ggplot2::ggplot(),
                      "temp/test.png"))

  ez_png(ggplot2::ggplot(),
         "temp/test.png",
         dir.create = TRUE)
  expect_true(file.exists("temp/test.png"))
  unlink("temp/test.png")

})
