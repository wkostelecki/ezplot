

context("ez_agg")


test_that("ez_agg works", {
  expect_equal(agg_table(mtcars, cyl, mpg),
               mtcars %>% group_by(x = cyl) %>% summarize(mpg = sum(mpg)) %>% ungroup)
  agg_table(mtcars, cyl, mpg, group = am)
})
