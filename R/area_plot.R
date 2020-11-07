#' area_plot
#' @description Aggregates a data.frame and creates a stacked area chart.
#' @param data A data.frame.
#' @param x A named character value. Evaluates to a column.
#' @param y A named character value. Evaluates to a column.
#' @param group A character value. Evaluates to a column.
#' @param facet_x A character value. Evaluates to a column.
#' @param facet_y A character. Evaluates to a column.
#' @param size theme size for \code{use_theme()}. Default is 14.
#' @param reorder A character vector specifying the group variables to reorder.
#'   Default is \code{c("group", "facet_x", "facet_y")}.
#' @param palette Colour function.
#' @param labels_y label formatting function
#' @param labels_x label formatting function
#' @param use_theme ggplot theme function
#' @param position Either \code{"stack"} (default) or \code{"fill"}
#' @param facet_scales Option passed to scales argument in \code{facet_wrap} or
#'   \code{facet_grid}. Default is \code{"fixed"}.
#' @param facet_ncol Option passed to ncol argument in \code{facet_wrap} or
#'   \code{facet_grid}. Default is \code{NULL}.
#' @param legend_ncol Number of columns in legend.
#' @param env environment for evaluating expressions.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \donttest{
#' library(tsibble)
#' library(tsibbledata)
#' area_plot(ansett, x = "as.Date(Week)", y = "Passengers")
#' area_plot(ansett,
#'           x = "as.Date(Week)", y = c("Weekly Passengers" = "Passengers"), "Class")
#' area_plot(ansett, "as.Date(Week)",
#'           y = c("Weekly Passengers" = "Passengers"),
#'           group = "substr(Airports, 5, 7)",
#'           facet_x = "substr(Airports, 1, 3)",
#'           facet_y = "Class",
#'           facet_scales = "free_y")
#' }
area_plot = function(data,
                     x,
                     y = "1",
                     group = NULL,
                     facet_x = NULL,
                     facet_y = NULL,
                     size = 11,
                     reorder = c("group", "facet_x", "facet_y"),
                     palette = ez_col,
                     labels_y = if (position == "fill") {
                       function(x) ez_labels(100 * x, append = "%")
                     } else {
                       ez_labels
                     },
                     labels_x = NULL,
                     use_theme = theme_ez,
                     position = c("stack", "fill"),
                     facet_scales = "fixed",
                     facet_ncol = NULL,
                     legend_ncol = NULL,
                     env = parent.frame()) {

  y = nameifnot(y)

  position = match.arg(position)

  cols = c(x = unname(x),
           y = unname(y),
           group = unname(group),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y))

  gdata = agg_data(data,
                   cols,
                   group_by = cols[intersect(names(cols),
                                             c("x", "group",
                                               "facet_x", "facet_y"))],
                   env = env)

  if (any("group" == names(gdata))) gdata[["group"]] = factor(gdata[["group"]])

  gdata = reorder_levels(gdata, cols = reorder)

  if (any("group" == names(gdata))) {
    gdata[["group"]] = forcats::fct_rev(gdata[["group"]])
  }

  if (position == "fill") {
    gdata = gdata %>%
      group_by(!!!syms(intersect(c("x", "facet_x", "facet_y"),
                                 names(gdata)))) %>%
      mutate(y = y / sum(abs(y))) %>%
      ungroup
  }

  g = ggplot(gdata)

  if (exists("group", gdata)) {
    g = g +
      geom_col(aes(x, y,
                   fill = group),
               width = get_incr(gdata[["x"]]),
               orientation = "x") +
      scale_fill_manual(NULL,
                        values = rev(palette(length(unique(gdata[["group"]])))),
                        labels = function(x) paste0(x, "   "),
                        breaks = rev,
                        guide = guide_legend(ncol = legend_ncol))
  } else {
    g = g +
      geom_col(aes(x, y),
               fill = palette(1),
               width = get_incr(gdata[["x"]]),
               orientation = "x")
  }

  g = quick_facet(g, scales = facet_scales, ncol = facet_ncol)


  if (position == "fill") {
    expand = rep(0, 4)
  } else {
    expand = c(0.1 * any(gdata[["y"]] < 0),
               0,
               0.1 * any(gdata[["y"]] > 0),
               0)
  }

  if (!is.null(labels_x)) {

    scale_x = if (lubridate::is.Date(gdata[["x"]])) {
      scale_x_date
    } else if (lubridate::is.POSIXt(gdata[["x"]])) {
      scale_x_datetime
    } else {
      scale_x_continuous
    }

    g = g + scale_x(labels = labels_x)

  }

  g +
    xlab(names(x)) +
    ylab(names(y)) +
    scale_y_continuous(labels = labels_y,
                       expand = expand) +
    ylab(names(y)) +
    use_theme(size)

}




