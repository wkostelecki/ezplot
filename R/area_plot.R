#' area_plot
#' @description Aggregates a data.frame and creates a stacked area chart.
#' @param data A data.frame.
#' @param x A named character value. Evaluates to a column.
#' @param y A named character value. Evaluates to a column.
#' @param group A character value. Evaluates to a column.
#' @param facet_x A character value. Evaluates to a column.
#' @param facet_y A character. Evaluates to a column.
#' @param size theme size for \code{use_theme()}. Default is 20.
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
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' df = ez_data()
#' area_plot(df, "year2", "units", size = 10)
#' area_plot(df, "year2", "units", "fct", use_theme = ggplot2::theme_bw)
#' area_plot(df, "year2", "units", "fct", reorder = NULL)
#' area_plot(df, "year2", "units", "fct", position = "fill")
#' area_plot(df, "year2", c("Unit Sales" = "units"), size = 12)
#' area_plot(df, "year2", c("Unit Sales" = "units"), "fct", "char")
#' area_plot(df, "year2", "units", "fct", "char", "num", position = "fill")
area_plot = function(data,
                     x,
                     y,
                     group = NULL,
                     facet_x = NULL,
                     facet_y = NULL,
                     size = 12,
                     reorder = c("group", "facet_x", "facet_y"),
                     palette = ez_col,
                     labels_y = if (position == "fill") {
                       function(x) ez_labels(100 * x, append = "%")
                     } else {
                       ez_labels
                     },
                     labels_x = identity,
                     use_theme = theme_ez,
                     position = c("stack", "fill"),
                     facet_scales = "fixed",
                     facet_ncol = NULL) {

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
                                               "facet_x", "facet_y"))])

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

  if ("group" %in% names(gdata)){
    g = g +
      geom_col(aes(x, y,
                   fill = group),
               width = get_incr(gdata[["x"]])) +
      scale_fill_manual(NULL,
                        values = rev(palette(length(unique(gdata[["group"]])))),
                        labels = function(x) paste0(x, "   "),
                        breaks = rev)
  } else {
    g = g +
      geom_col(aes(x, y),
               fill = palette(1),
               width = get_incr(gdata[["x"]]))
  }

  g = quick_facet(g, scales = facet_scales, ncol = facet_ncol)

  if (any(gdata[["y"]] < 0)){
    expand = c(0.1, 0, 0.1, 0)
  } else {
    expand = c(0, 0, 0.1, 0)
  }

  g +
    xlab(names(x)) +
    ylab(names(y)) +
    scale_y_continuous(labels = labels_y,
                       expand = expand) +
    scale_x_continuous(labels = labels_x) +
    ylab(names(y)) +
    use_theme(size)

}




