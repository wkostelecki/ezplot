#' side_plot
#'
#' @param data A data.frame.
#' @param x A named character.
#' @param y A named character.
#' @param ylabels label formatting function
#' @param size theme size for \code{use_theme()}. Default is 20.
#' @param palette Colour function.
#'
#' @export
#' @examples
#' side_plot(fruit, "Product", "Units")
#' side_plot(fruit, "Store", "Units")
side_plot = function(data,
                     x,
                     y,
                     ylabels = ez_labels,
                     size = 20,
                     palette = ez_col){


  cols = c(x = unname(x),
           y = unname(y))

  gdata = agg_data(data,
                   cols)

  ggplot(gdata) +
    geom_col(aes(x, y),
             fill = palette(1)) +
    scale_y_continuous(labels = ez_labels,
                       expand = c(0, 0, 0.3, 0)) +
    coord_flip() +
    theme_ez(size) +
    xlab(NULL) +
    ylab(NULL)

}
