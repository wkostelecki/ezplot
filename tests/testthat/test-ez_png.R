context("ez_png")

test_that("ez_png works", {
  ez_png(ggplot2::ggplot(),
         "test.png",
         check = FALSE)
  expect_true(file.exists("test.png"))
  unlink("test.png")

  suppressWarnings(
    expect_error(ez_png(ggplot2::ggplot(),
                      "temp_dir/test.png",
                      check = FALSE))
  )

  ez_png(ggplot2::ggplot(),
         "temp_dir/test.png",
         dir.create = TRUE,
         check = FALSE)
  expect_true(file.exists("temp_dir/test.png"))
  unlink("temp_dir", recursive = TRUE)

})
