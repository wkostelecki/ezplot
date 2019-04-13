#' secondary_plot
#' creates a plot with a secondary y-axis
#' @inheritParams area_plot
#' @param y1 Variable to plot on the left-hand axis
#' @param y2 Variable to plot on the right-hand axis
#' @param labels_y1 label formatting function
#' @param labels_y2 label formatting function
#' @param size_line line size
#' @param ylim1 (optional) left axis limits
#' @param ylim2 (optional) right axis limits
#' @return A ggplot object.
#' @export
#' @examples
#' df = ez_data2()
#' secondary_plot(df, "obs", "units", "price", "xsec")
#' df = tibble::rownames_to_column(mtcars, "name")
#' secondary_plot(df, "name", "mpg", "hp")
#' secondary_plot(df, "name", c(MPG = "mpg"), c(HP = "hp"), ylim1 = c(0, 35), ylim2 = c(0, 350))
secondary_plot = function (data,
                           x,
                           y1,
                           y2,
                           facet_x = NULL,
                           facet_y = NULL,
                           size_line = 1,
                           labels_y1 = ez_labels,
                           labels_y2 = ez_labels,
                           ylim1 = NULL,
                           ylim2 = NULL,
                           reorder = c("facet_x", "facet_y"),
                           size = 14) {

  cols = c(x = unname(x),
           y1 = unname(y1),
           y2 = unname(y2),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y))

  gdata = agg_data(
    data = data,
    cols = cols,
    group_by = cols[intersect(names(cols),
                              c("x", "facet_x", "facet_y"))],
    agg_fun = function(x) sum(x, na.rm = TRUE),
    group_by2 = cols[intersect(names(cols),
                               c("facet_x", "facet_y"))]
  )

  if (is.character(gdata[["x"]])) gdata[["x"]] = factor(gdata[["x"]])

  if (is.factor(gdata[["x"]])) {

    gdata = gdata %>%
      mutate(x_label = x,
             x = as.numeric(x))

  }

  if (is.null(ylim1)) {
    y1_range = max(diff(range(gdata[["y1"]],
                              na.rm = TRUE)),
                   gdata[["y1"]], na.rm = TRUE)
    y1_adjust = min(gdata[["y1"]], 0, na.rm = TRUE)
  } else {
    y1_adjust = ylim1[1]
    y1_range = diff(ylim1)
  }

  if (is.null(ylim2)) {
    y2_range = max(diff(range(gdata[["y2"]],
                              na.rm = TRUE)),
                   gdata[["y2"]], na.rm = TRUE)
    y2_adjust = min(gdata[["y2"]], 0, na.rm = TRUE)
  } else {
    y2_adjust = ylim2[1]
    y2_range = diff(ylim2)
  }

  gdata[["y2"]] = (gdata[["y2"]] - y2_adjust) / y2_range *
    y1_range + y1_adjust

  sec_trans = sprintf("~ (. - %f) * %f / %f + %f",
                      y1_adjust,
                      y2_range,
                      y1_range,
                      y2_adjust)

  g = ggplot(gdata) +
    geom_line(aes(x, y1),
              size = size_line,
              na.rm = TRUE) +
    theme_ez(size) +
    ylab(names(y1)) +
    xlab(NULL)

  g = g + geom_line(aes(x, y2),
                    size = size_line,
                    colour = ez_col(1),
                    na.rm = TRUE) +
    scale_y_continuous(labels = labels_y1,
                       sec.axis = sec_axis(as.formula(sec_trans),
                                           labels = labels_y2,
                                           name = names(y2))) +
    theme(axis.title.y.right = element_text(color = ez_col(1)),
          # axis.ticks.y.right = element_line(color = ez_col(1)),
          axis.text.y.right = element_text(color = ez_col(1)))

  if (lubridate::is.Date(gdata[["x"]])) {
    g = g + scale_x_date(labels = function(x) format(x, "%b %y"))
  } else if (exists("x_label", gdata)) {
    g = g +
      scale_x_continuous(breaks = gdata[["x"]],
                         labels = gdata[["x_label"]]) +
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       vjust = 0.38))
  }

  quick_facet(g) +
    coord_cartesian(expand = FALSE,
                    ylim = y1_adjust + c(0, y1_range))

}
