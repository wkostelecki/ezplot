#' @title line_plot
#' @name line_plot
#' @description Creates line plots.
#' @param data data.frame
#' @param x named value
#' @param y named value
#' @param group value
#' @param facet_x value
#' @param facet_y value
#' @export
#' @import ggplot2
#' @examples line_plot(fruit, "OBS", "Units")
#' @examples line_plot(fruit, "OBS", "Units", "Product")
#' @examples line_plot(fruit, "OBS", "Units", "Product", "Size")
#' @examples line_plot(fruit, "OBS", "Units", "Product", "Size", "Store")
line_plot = function(data,
                     x,
                     y,
                     group = NULL,
                     facet_x = NULL,
                     facet_y = NULL){

  cols = c(x = unname(x),
           y = unname(y),
           group = unname(group),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y))

  gdata = data %>%
    transmute(!!!rlang::syms(cols)) %>%
    group_by(!!!rlang::syms(setdiff(names(cols), "y"))) %>%
    summarize(y = sum(y, na.rm = TRUE)) %>%
    ungroup

  g = ggplot(gdata)

  if ("group" %in% names(gdata)){
    g = g +
      geom_line(aes(x, y, colour = group)) +
      scale_colour_manual(NULL,
                          values = palette(length(unique(gdata[["group"]]))))
  } else {
    g = g +
      geom_line(aes(x, y), colour = palette(1))
  }
  g = quick_facet(g)

  g +
    xlab(names(x)) +
    ylab(names(y))

}

