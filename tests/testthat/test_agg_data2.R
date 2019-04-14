

context("agg_data2")

test_that("agg_data works", {

  df = agg_data(mtcars,
                c(x = "factor(cyl)", y = "hp", z = "factor(am)"),
                group_by = c(x = "factor(cyl)", z = "factor(am)"))

  df_expected = mtcars %>%
    dplyr::group_by(x = factor(cyl), z = factor(am)) %>%
    dplyr::summarize(y = sum(hp, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(x, y, z) %>%
    as.data.frame()

  expect_equal(df, df_expected)

})

