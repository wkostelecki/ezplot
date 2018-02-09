
#' mean_plot
#' @description Chart to compare the means across groups using a bar chart.
#' @param data data.frame
#' @param x quoted expression (required)
#' @param y quoted expression (required)
#' @param size base_size for ggplot2 theme (default is 20)
#' @param labels function for formatting labels (default is ez_labels)
#'
#' @examples
#' library(dplyr)
#' mean_plot(mtcars, c("Number of Cylinders" = "factor(cyl)"), "hp > 110",
#'           labels = function(x) ez_labels(100 * x, append = "%"))
#' @export
mean_plot = function(data, x, y,
                     size = 20,
                     labels = ez_labels) {
  y = nameifnot(y)
  cols = c(x = unname(x),
           y = unname(y))
  gdata = data %>%
    transmute_(.dots = cols) %>%
    group_by(x) %>%
    summarize(y = mean(y),
              n = n()) %>%
    as.data.frame

  ggplot(gdata) +
    geom_col(aes(x, y)) +
    theme_ez(size) +
    ylab(names(y)) +
    xlab(names(x)) +
    scale_y_continuous(labels = labels, expand = c(0, 0))

}
