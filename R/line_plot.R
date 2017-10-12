#' @title line_plot
#' @name line_plot
#' @description Creates line plots.
#'
#' @param data A data.frame.
#' @param x A named character.
#' @param y A named character.
#' @param group character
#' @param facet_x character
#' @param facet_y character
#' @param linesize width of line for \code{geom_line()}. Default is 1.
#' @param size theme size for \code{use_theme()}. Default is 20.
#' @param palette Colour function.
#' @param ylabels label formatting function
#' @param use_theme ggplot theme function
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2 dplyr
#' @examples
#' line_plot(fruit, "OBS", "Units")
#' line_plot(fruit, "OBS", c("Weekly Units Sold" = "Units"))
#' line_plot(fruit, "OBS", "Units", "Product")
#' line_plot(fruit, "OBS", "Units", "Product", "Size")
#' line_plot(fruit, "OBS", "Units", "Product", "Size", "Store")
#' line_plot(fruit, "OBS", "Units", use_theme = ggplot2::theme_bw)
line_plot = function(data,
                     x,
                     y,
                     group = NULL,
                     facet_x = NULL,
                     facet_y = NULL,
                     linesize = 1,
                     size = 20,
                     palette = ez_col,
                     ylabels = ez_labels,
                     use_theme = theme_ez){

  cols = c(x = unname(x),
           y = unname(y),
           group = unname(group),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y))

  gdata = agg_data(data, cols)

  g = ggplot(gdata)

  if ("group" %in% names(gdata)){
    g = g +
      geom_line(aes(x, y, colour = group),
                size = linesize) +
      scale_colour_manual(NULL,
                          values = palette(length(unique(gdata[["group"]]))),
                          labels = function(x) paste0(x, "   "))
  } else {
    g = g +
      geom_line(aes(x, y),
                size = linesize,
                colour = palette(1))
  }

  g = quick_facet(g)

  g +
    xlab(names(x)) +
    ylab(names(y)) +
    scale_y_continuous(labels = ylabels) +
    ylab(names(y)) +
    use_theme(size)

}



#' lineplot
#' @name lineplot
#' @description see \code{line_plot}.
#' @param data A data.frame.
#' @param x Bare expression.
#' @param y Bare expression.
#' @param group Bare expression.
#' @param facet_x Bare expression.
#' @param facet_y Bare expression.
#' @param ... arguments to pass to \code{line_plot}
#'
#' @return ggplot
#' @export
#' @examples
#' lineplot(fruit, OBS, Units)
lineplot = function(data,
                    x,
                    y,
                    group = NULL,
                    facet_x = NULL,
                    facet_y = NULL,
                    ...){

  x = deparse(substitute(x))
  y = deparse(substitute(y))
  group = no_null(deparse(substitute(group)))
  facet_x = no_null(deparse(substitute(facet_x)))
  facet_y = no_null(deparse(substitute(facet_y)))
  line_plot(data, x, y, group, facet_x, facet_y, ...)

}
