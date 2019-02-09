#' ez_data
#' @description Creates example data using \code{expand.grid()} on \code{char},
#'   \code{fct} and \code{num} and then adding random \code{units}, \code{value}
#'   and \code{price} columns.
#' @param start_date A start date value.
#' @param end_date An end date value.
#' @param char Vector of character values.
#' @param fct Vector of factor values.
#' @param num Vector of numeric values.
#' @param seed A random seed.
#' @return A data.frame.
#' @export
#' @examples
#' df = ez_data()
#' summary(df)
ez_data = function(
  start_date = as.Date("2012-01-01"),
  end_date = as.Date("2016-12-31"),
  char = c("A", "B"),
  fct = factor(c("X", "Y", "Other"), c("X", "Y", "Other")),
  num = c(10, 20),
  seed = 9
) {

  set.seed(seed)
  expand.grid(char = as.character(char),
              fct = factor(fct),
              num = as.numeric(num),
              day = seq(lubridate::floor_date(start_date, "week"),
                        lubridate::ceiling_date(end_date, "week") - 1,
                        by = "day"),
              stringsAsFactors = FALSE) %>%
    mutate(week = lubridate::floor_date(day, "week"),
           month = lubridate::floor_date(day, "month"),
           year = lubridate::year(day),
           year2 = year + (lubridate::month(day) - 1) / 12) %>%
    group_by(char, fct, num) %>%
    mutate(units = as.numeric(
      stats::arima.sim(
        list(ar = 0.5),
        n(),
        function(x) {
          sample(10, x,
                 replace = TRUE)
        }
      )),
      price = round(seq(sample(5:10, 1),
                        sample(5:10, 1),
                        length.out = n())),
      value = units * price) %>%
    ungroup %>%
    select(-price, price)

}

#' ez_data2
#' @inheritParams ez_data
#' @param xsec (optional) vector of cross sections.
#' @export
ez_data2 = function(start_date = as.Date("2012-01-01"),
                    end_date = as.Date("2016-12-31"),
                    xsec = c("apples" ,"oranges"),
                    seed = 9) {

  expand.grid(xsec = as.character(xsec),
              obs = seq(lubridate::floor_date(start_date, "week"),
                        lubridate::ceiling_date(end_date, "week") - 1,
                        by = "week"),
              stringsAsFactors = FALSE) %>%
    arrange(xsec, obs) %>%
    group_by(xsec) %>%
    mutate(units = as.numeric(
      stats::arima.sim(
        model = list(ar = 0.5),
        n = n(),
        rand.gen = function(x) {
          sample(10, x,
                 replace = TRUE)
        }
      )),
      price = round(seq(sample(5:10, 1),
                        sample(5:10, 1),
                        length.out = n())),
      value = units * price) %>%
    ungroup %>%
    select(-price, price)

}

globalVariables(c("day", "year"))
