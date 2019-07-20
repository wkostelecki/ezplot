#' calendar_plot
#' @inheritParams area_plot
#' @param x date column
#' @param ... additional arguments for tile_plot
#' @export
#' @examples
#' library(tsibbledata)
#' calendar_plot(vic_elec, "Time", "Demand", zlim = c(NA, NA))
calendar_plot = function(data, x, y, ...) {
  wd = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")


  cols = c(x = paste0("lubridate::floor_date(", unname(x), ", 'day')"),
           y = unname(y))

  gdata = agg_data(
    data, cols, cols["x"]
  )

  gdata = gdata %>%
    mutate(wday = factor(wd[lubridate::wday(x)], wd),
           week = lubridate::epiweek(x),
           year = lubridate::year(x),
           month = factor(month.abb[lubridate::month(x)], month.abb),
           week = ifelse(month == "Dec" & week == 1, 53, week)) %>%
    group_by(year, month) %>%
    mutate(month_week = week - min(week) + 1) %>%
    ungroup

  tile_plot(gdata, "wday", "month_week", "y", "year", "month", reorder = NULL, ...) %>%
    quick_facet(scales = "free_y") +
    scale_y_reverse(labels = function(x) rep("", length(x))) +
    theme(panel.spacing.y = grid::unit(0, "lines"),
          axis.ticks = element_blank())

}

globalVariables(c("month", "week"))
