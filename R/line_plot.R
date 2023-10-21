#' @title line_plot
#' @name line_plot
#' @description Creates line plots.
#' @inheritParams area_plot
#' @param yoy Logical used to indicate whether a YOY grouping should be created.
#'   Default is \code{FALSE}.
#' @param size_line width of line for \code{geom_line()}. Default is 1.
#' @param points logical. Option to include points
#' @param na.rm logical. Option to exclude NAs
#' @param limits_y vector of c(min, max) y-axis limits
#' @return A ggplot object.
#' @export
#' @import ggplot2 dplyr
#' @examples
#' suppressPackageStartupMessages(library(tsibble))
#' library(tsibbledata)
#' line_plot(ansett, x = "Week", y = "Passengers")
#' line_plot(ansett, x = "Week", y = "Passengers", "Class")
#' line_plot(pelt, "Year", "Hare", limits_y = c(0, NA))
#' line_plot(pelt, "Year", c("Hare", "Lynx"), points = TRUE)
#' line_plot(pelt, "Year", c("Hare", "Lynx"), points = 0.5)
#' line_plot(pelt, "Year", c("Hare", "Lynx"), points = TRUE, limits_y = c(0, NA))
#' line_plot(pelt, "Year", "Hare", use_theme = ggplot2::theme_bw)
#' line_plot(pelt, "Year", c("Hare Population" = "Hare"))
#' line_plot(pelt[pelt$Year > 1930,], "factor(Year)", c("Hare Population" = "Hare"), points = TRUE)
#' line_plot(pelt[pelt$Year > 1930,], "factor(Year)", c("Hare", "Lynx"), points = TRUE)
line_plot = function(data,
                     x,
                     y = "1",
                     group = NULL,
                     facet_x = NULL,
                     facet_y = NULL,
                     yoy = FALSE,
                     size_line = 1,
                     points = FALSE,
                     size = 11,
                     reorder = c("group", "facet_x", "facet_y"),
                     palette = ez_col,
                     labels_y = ez_labels,
                     limits_y = c(NA, NA),
                     use_theme = theme_ez,
                     facet_scales = "fixed",
                     na.rm = FALSE,
                     legend_ncol = NULL) {

  stopifnot(sum(c(length(y) > 1, !is.null(group), yoy)) <= 1)

  y = nameifnot(y)

  cols = c(x = unname(x),
           stats::setNames(y, paste0("y", seq_along(y))),
           group = unname(group),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y))

  gdata = agg_data(
    data, cols,
    cols[intersect(names(cols),
                   c("x", "group", "facet_x", "facet_y"))],
    group_by2 = cols[intersect(names(cols),
                               c("group", "facet_x", "facet_y"))]
  )

  if (length(y) > 1) {
    gdata = tidyr::gather(gdata, "group", "y", paste0("y", seq_along(y)))
    gdata[["group"]] =  factor(gdata[["group"]],
                               paste0("y", seq_along(y)),
                               names(y))

  } else {
    gdata = rename(gdata, y = y1)
  }

  if (yoy) {
    gdata[["group"]] = lubridate::year(gdata[["x"]])
    gdata[["x"]] = lubridate::yday(gdata[["x"]])
  }

  for (i in intersect(names(gdata), c("group", "facet_x", "facet_y"))) {
    gdata[[i]] = factor(gdata[[i]])
  }

  gdata = reorder_levels(gdata, cols = reorder)

  if (is.character(gdata[["x"]]) | is.factor(gdata[["x"]])) {

    gdata[["x"]] = factor(gdata[["x"]])

  }

  g = ggplot(gdata)

  if ("group" %in% names(gdata)) {
    if (yoy) {
      g = g +
        geom_line(mapping = aes(x, y, colour = group, group = group),
                  linewidth = size_line,
                  na.rm = na.rm) +
        scale_color_manual(NULL,
                           values = palette(length(unique(gdata[["group"]]))),
                           labels = function(x) paste0(x, "   "),
                           guide = guide_legend(ncol = legend_ncol)) +
        scale_x_continuous(breaks = c(1, 91, 182, 274, 366),
                           limits = c(1, 366),
                           labels = c("Jan", "Apr", "Jul", "Oct", "Jan")) +
        theme(legend.position = "top")
    } else {
      g = g +
        geom_line(aes(x, y, colour = group, group = group),
                  linewidth = size_line,
                  na.rm = na.rm) +
        scale_colour_manual(NULL,
                            values = palette(length(unique(gdata[["group"]]))),
                            labels = function(x) paste0(x, "   "),
                            guide = guide_legend(ncol = legend_ncol))
    }
    if (points > 0)
      g = g + geom_point(aes(x, y, colour = group),
                         size = 3 * size_line * points,
                         na.rm = na.rm)
  } else {
    g = g +
      geom_line(aes(x, y, group = 1),
                linewidth = size_line,
                colour = palette(1),
                na.rm = na.rm)
    if (points > 0)
      g = g + geom_point(aes(x, y),
                         size = 3 * size_line * points,
                         colour = palette(1),
                         na.rm = na.rm)
  }

  g = quick_facet(g, scales = facet_scales)

  g = g +
    xlab(names(x)) +
    ylab(names(y)) +
    scale_y_continuous(labels = labels_y,
                       limits = limits_y,
                       expand = c(0.02 * is.na(limits_y[1]), 0,
                                  0.02 * is.na(limits_y[2]), 0)) +
    ylab(names(y)) +
    use_theme(size) +
    theme(legend.key.height = grid::unit(0, "lines"))

  g + coord_cartesian(clip = "off") +
    theme(axis.line.x = element_blank())
}

globalVariables("y1")
