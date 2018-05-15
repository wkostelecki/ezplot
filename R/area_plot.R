#' area_plot
#' @description Aggregates a data.frame and creates a stacked area chart.
#' @param data A data.frame.
#' @param x A named character.
#' @param y A named character.
#' @param group character
#' @param facet_x character
#' @param facet_y character
#' @param size theme size for \code{use_theme()}. Default is 20.
#' @param reorder A character vector specifying the group variables to reorder.
#'   Default is \code{c("group", "facet_x", "facet_y")}.
#' @param palette Colour function.
#' @param ylabels label formatting function
#' @param use_theme ggplot theme function
#' @param position Default is \code{"stack"}
#' @param facet_scales Option passed to scales argument in facet_wrap/grid.
#'   Default is \code{"fixed"}.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' area_plot(mtcars, "carb", "1", size = 12)
#' area_plot(mtcars, "carb", "1", "cyl", use_theme = ggplot2::theme_bw)
#' area_plot(mtcars, "carb", "1", "cyl", reorder = NULL)
#' area_plot(mtcars, "cyl", "1", "carb", position = "fill")
#' area_plot(mtcars, "carb", c(Count = "1"), size = 12)
#' area_plot(mtcars, "carb", c(Count = "1"), "cyl", "gear")
#' area_plot(mtcars, "carb", "1", "cyl", "gear", "am", position = "fill")
area_plot = function(data,
                     x,
                     y,
                     group = NULL,
                     facet_x = NULL,
                     facet_y = NULL,
                     size = 12,
                     reorder = c("group", "facet_x", "facet_y"),
                     palette = ez_col,
                     ylabels = if (position == "fill") {
                       function(x) ez_labels(100 * x, append = "%")
                     } else {
                       ez_labels
                     },
                     use_theme = theme_ez,
                     position = c("stack", "fill"),
                     facet_scales = "fixed") {

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

  g = quick_facet(g, scales = facet_scales)

  if (any(gdata[["y"]] < 0)){
    expand = c(0.1, 0, 0.1, 0)
  } else {
    expand = c(0, 0, 0.1, 0)
  }

  g +
    xlab(names(x)) +
    ylab(names(y)) +
    scale_y_continuous(labels = ylabels,
                       expand = expand) +
    ylab(names(y)) +
    use_theme(size)

}




