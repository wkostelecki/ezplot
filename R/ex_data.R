#' example data
#' @export
#' @examples
#' ex_data()

ex_data = function(
  start_date = as.Date("2012-01-01"),
  end_date = as.Date("2016-12-31"),
  char = c("A", "B"),
  fct = factor(c("X", "Y", "Other"), c("X", "Y", "Other")),
  num = c(10, 20)
) {

  expand.grid(char = char,
              fct = fct,
              num = num,
              Day = seq(lubridate::floor_date(start_date, "week"),
                        lubridate::ceiling_date(end_date, "week") - 1,
                        by = "day"),
              stringsAsFactors = FALSE) %>%
    mutate(Week = lubridate::floor_date(Day, "week"),
           Month = lubridate::floor_date(Day, "month"),
           Year = lubridate::year(Day),
           Year2 = Year + (lubridate::month(Day) - 1) / 12) %>%
    group_by(char, fct, num) %>%
    mutate(units = as.numeric(arima.sim(list(ar = 0.5),
                                        n(),
                                        function(x) sample(10, x, replace = TRUE))),
           value = units * round(seq(sample(5:10, 1),
                                     sample(5:10, 1),
                                     length.out = n()))) %>%
    ungroup

}
