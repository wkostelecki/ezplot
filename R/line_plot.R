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
#' @return A ggplot object.
#' @export
#' @import ggplot2 dplyr
#' @examples
#' line_plot(mtcars, "cyl", "1", use_theme = ggplot2::theme_bw)
#' line_plot(mtcars, "cyl", c(Count = "1"))
#' line_plot(mtcars, "cyl", c(Count = "1"), "gear")
#' line_plot(mtcars, "cyl", c(Count = "1"), "gear", "am", size = 12)
#' line_plot(mtcars, "cyl", c(Count = "1"), "vs", "gear", "am", size = 12)
#' line_plot(mtcars, "cyl", c(Count = "1"), "gear", "am", facet_scales = "free_y")
line_plot = function(data,
                     x,
                     y,
                     group = NULL,
                     facet_x = NULL,
                     facet_y = NULL,
                     linesize = 1,
                     size = 12,
                     palette = ez_col,
                     ylabels = ez_labels,
                     use_theme = theme_ez,
                     facet_scales = "fixed") {

  stopifnot(!(length(y) > 1 & !is.null(group)))

  y = nameifnot(y)

  cols = c(x = unname(x),
           setNames(y, paste0("y", seq_along(y))),
           group = unname(group),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y))

  gdata = agg_data(data, cols,
                   cols[intersect(names(cols),
                                  c("x", "group", "facet_x", "facet_y"))],
                   group_by2 = cols[intersect(names(cols),
                                              c("group", "facet_x", "facet_y"))])

  if (length(y) > 1) {
    gdata = tidyr::gather_(gdata, "group", "y", paste0("y", seq_along(y)))
    gdata[["group"]] =  factor(gdata[["group"]],
                               paste0("y", seq_along(y)),
                               names(y))

  } else {
    gdata = rename(gdata, y = y1)
  }

  for (i in intersect(names(gdata), c("group", "facet_x", "facet_y"))) {
    gdata[[i]] = factor(gdata[[i]])
  }

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

  g = quick_facet(g, scales = facet_scales)

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
