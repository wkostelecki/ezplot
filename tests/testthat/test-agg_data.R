
context("agg_data")

test_that("agg_data base cases", {

  expect_equal(agg_data(mtcars), summarize_all(mtcars, sum))
  expect_equal(mtcars %>%
                 mutate(cyl = as.character(cyl)) %>%
                 agg_data,
               mtcars %>%
                 group_by(cyl = as.character(cyl)) %>%
                 summarize_all(sum) %>%
                 as.data.frame %>%
                 select(!!!syms(names(mtcars))))

  expect_equal(agg_data(mtcars, group_by = "cyl"),
               mtcars %>%
                 group_by(cyl) %>%
                 summarize_all(sum) %>%
                 as.data.frame %>%
                 select(!!!syms(names(mtcars))))

  expect_equal(agg_data(mtcars, c(x = "as.character(cyl)", "mpg")),
               mtcars %>%
                 group_by(x = as.character(cyl)) %>%
                 summarize(mpg = sum(mpg)) %>%
                 as.data.frame)

  expect_equal(agg_data(mtcars,
                        c(x = "as.character(cyl)", "mpg"),
                        agg_fun = mean),
               mtcars %>%
                 group_by(x = as.character(cyl)) %>%
                 summarize(mpg = mean(mpg)) %>%
                 as.data.frame)

  expect_equal(agg_data(mtcars, c(cyl = "as.character(cyl)")),
               mtcars %>%
                 distinct(cyl = as.character(cyl)) %>%
                 tibble::remove_rownames() %>%
                 arrange(cyl))

  expect_equal(agg_data(mtcars, c("cyl", "mpg * hp"), "cyl"),
               mtcars %>%
                 group_by(cyl) %>%
                 summarize(`mpg * hp` = sum(mpg * hp)) %>%
                 as.data.frame)

  expect_equal(agg_data(mtcars, "~ cumsum(mpg)", "cyl"),
               mtcars %>%
                 group_by(cyl) %>%
                 summarize(mpg = sum(mpg)) %>%
                 as.data.frame %>%
                 mutate(`~ cumsum(mpg)` = cumsum(mpg)) %>%
                 select(-mpg, -cyl))

  expect_equal(agg_data(mtcars, c(y = "~ cumsum(mpg)"), "cyl"),
               mtcars %>%
                 group_by(cyl) %>%
                 summarize(mpg = sum(mpg)) %>%
                 as.data.frame %>%
                 mutate(y = cumsum(mpg)) %>%
                 select(-mpg, -cyl))


  expect_equal(agg_data(mtcars, c("cyl", y = "~ cumsum(mpg)", "wt"), "cyl"),
               mtcars %>%
                 group_by(cyl) %>%
                 summarize(y = sum(mpg),
                           wt = sum(wt)) %>%
                 as.data.frame %>%
                 mutate(y = cumsum(y)))

  expect_equal(agg_data(mtcars, c("cyl", y = "~ wt / cyl"), "cyl"),
               mtcars %>%
                 group_by(cyl) %>%
                 summarize(cyl2 = sum(cyl),
                           wt = sum(wt)) %>%
                 as.data.frame %>%
                 mutate(y = wt / cyl2) %>%
                 select(-cyl2, -wt))


})


test_that("select renamings", {

  expect_equal(
    agg_data(mtcars,
             c(x = "gear", "gear"),
             c(x = "gear")),
    mtcars %>%
      group_by(x = gear) %>%
      summarize(gear = sum(gear)) %>%
      as.data.frame
  )

})



context("more agg_data")

test_that("mixed groups/numeric/categorical", {

  df = agg_data(mtcars,
                c(x = "factor(cyl)", y = "hp", z = "factor(am)"),
                group_by = c(x = "factor(cyl)", z = "factor(am)"))

  expected = mtcars %>%
    group_by(x = factor(cyl), z = factor(am)) %>%
    summarize(y = sum(hp)) %>%
    ungroup() %>%
    select(x, y, z) %>%
    as.data.frame()

  expect_equal(df, expected)

  df = agg_data(mtcars,
                c(x = "factor(cyl)", y = "hp", z = "factor(am)"),
                group_by = c(z = "factor(am)", x = "factor(cyl)"))

  expected = mtcars %>%
    group_by(z = factor(am), x = factor(cyl)) %>%
    summarize(y = sum(hp)) %>%
    ungroup() %>%
    select(x, y, z) %>%
    as.data.frame()

  expect_equal(df, expected)

})

test_that("mixed '~' works", {

  df = agg_data(airquality,
                c("Month",
                  "Ozone",
                  x = "Wind",
                  "Ozone / Wind * Solar.R",
                  "~ Ozone / Wind * Solar.R"),
                group_by = "Month")
  target = airquality %>%
    group_by(Month) %>%
    summarize(Ozone2 = sum(Ozone, na.rm = TRUE),
              x = sum(Wind, na.rm = TRUE),
              `Ozone / Wind * Solar.R` = sum(Ozone / Wind * Solar.R, na.rm = TRUE),
              Solar.R = sum(Solar.R, na.rm = TRUE)) %>%
    as.data.frame() %>%
    rename(Ozone = Ozone2) %>%
    mutate(`~ Ozone / Wind * Solar.R` = Ozone / x * Solar.R) %>%
    select(-Solar.R)
  expect_equal(df, target)
})

