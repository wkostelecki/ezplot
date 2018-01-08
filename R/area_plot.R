#' area_plot
#'
#' @param data A data.frame.
#' @param x A named character.
#' @param y A named character.
#' @param group character
#' @param facet_x character
#' @param facet_y character
#' @param size theme size for \code{use_theme()}. Default is 20.
#' @param palette Colour function.
#' @param ylabels label formatting function
#' @param use_theme ggplot theme function
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' library(ukbabynames)
#' area_plot(ukbabynames, "year", "n")
#' area_plot(ukbabynames, "year", "n", "toupper(substring(name, 1, 1))")
#'
#' area_plot(fruit, "OBS", "Units")
#' area_plot(fruit, "OBS", "Units", size = 14)
#' area_plot(fruit, "OBS", c("Weekly Units Sold" = "Units"))
#' area_plot(fruit, "OBS", "Units", "Product")
#' area_plot(fruit, "OBS", "Units", "Product", "Size")
#' area_plot(fruit, "OBS", "Units", "Product", "Size", "Store")
#' area_plot(fruit, "OBS", "Units", use_theme = ggplot2::theme_bw)
area_plot = function(data,
                     x,
                     y,
                     group = NULL,
                     facet_x = NULL,
                     facet_y = NULL,
                     size = 20,
                     reorder = c("group", "facet_x", "facet_y"),
                     palette = ez_col,
                     ylabels = ez_labels,
                     use_theme = theme_ez){

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

  gdata = reorder_levels(gdata, cols = reorder)
  if ("group" %in% reorder && "group" %in% names(gdata)) {
    gdata[["group"]] = forcats::fct_rev(gdata[["group"]])
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

  g = quick_facet(g)

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




