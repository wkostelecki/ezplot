context("get_incr")


test_that("get_incr works", {
  expect_equal(ezplot:::get_incr(letters), 1)
  expect_equal(ezplot:::get_incr(factor(letters)), 1)

  seq = seq(as.POSIXct("2011-01-01"), by = "1 day", length.out = 5)
  expect_equal(ezplot:::get_incr(seq), 60 * 60 * 24)

})
