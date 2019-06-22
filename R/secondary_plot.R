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
#' library(tsibbledata)
#' secondary_plot(pelt, "Year", "Hare", "Lynx")
#' secondary_plot(pelt, "Year", c("Hare Population" = "Hare"), c("Lynx Population" = "Lynx"))
#' secondary_plot(aus_production, "Quarter", "Beer", "Cement",
#'                "lubridate::quarter(Quarter)",
#'                ylim1 = c(0, 600), ylim2 = c(0, 3000))
secondary_plot = function (data,
                           x,
                           y1 = "1",
                           y2 = "1",
                           facet_x = NULL,
                           facet_y = NULL,
                           size_line = 1,
                           labels_y1 = ez_labels,
                           labels_y2 = ez_labels,
                           ylim1 = NULL,
                           ylim2 = NULL,
                           reorder = c("facet_x", "facet_y"),
                           size = 14) {

  y1 = nameifnot(y1)
  y2 = nameifnot(y2)

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
  col1 = ez_col(1)
  col2 = ez_col(2)[2]

  g = ggplot(gdata) +
    geom_line(aes(x, y1),
              colour = col1,
              size = size_line,
              na.rm = TRUE) +
    theme_ez(size) +
    ylab(names(y1)) +
    xlab(NULL)

  g = g + geom_line(aes(x, y2),
                    size = size_line,
                    colour = col2,
                    na.rm = TRUE) +
    scale_y_continuous(labels = labels_y1,
                       sec.axis = sec_axis(as.formula(sec_trans),
                                           labels = labels_y2,
                                           name = names(y2))) +
    theme(axis.title.y.right = element_text(color = col2, vjust = 1),
          axis.text.y.right = element_text(color = col2),
          axis.title.y.left = element_text(color = col1),
          axis.text.y.left = element_text(color = col1))

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
