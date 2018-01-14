
#' mean_plot
#' @examples
#' library(dplyr)
#' tibble::rownames_to_column(mtcars, "car") %>%
#'   mean_plot("cyl", "hp > 110",
#'             labels = function(x) ez_labels(100 * x, append = "%"))
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
