#' @title line_plot
#' @name line_plot
#' @description Creates line plots.
#' @inheritParams area_plot
#' @param yoy Logical used to indicate whether a YOY grouping should be created.
#'   Default is \code{FALSE}.
#' @param size_line width of line for \code{geom_line()}. Default is 1.
#' @return A ggplot object.
#' @export
#' @import ggplot2 dplyr
#' @examples
#' library(tsibbledata)
#' line_plot(pelt, "Year", "Hare")
#' line_plot(pelt, "Year", c("Hare", "Lynx"))
#' line_plot(pelt, "Year", "Hare", use_theme = ggplot2::theme_bw)
#' line_plot(pelt, "Year", c("Hare Population" = "Hare"))
line_plot = function(data,
                     x,
                     y = "1",
                     group = NULL,
                     facet_x = NULL,
                     facet_y = NULL,
                     yoy = FALSE,
                     size_line = 1,
                     size = 11,
                     palette = ez_col,
                     labels_y = ez_labels,
                     use_theme = theme_ez,
                     facet_scales = "fixed") {

  stopifnot(sum(c(length(y) > 1, !is.null(group), yoy)) <= 1)

  y = nameifnot(y)

  cols = c(x = unname(x),
           stats::setNames(y, paste0("y", seq_along(y))),
           group = unname(group),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y))

  gdata = agg_data(
    data, cols,
    cols[intersect(names(cols),
                   c("x", "group", "facet_x", "facet_y"))],
    group_by2 = cols[intersect(names(cols),
                               c("group", "facet_x", "facet_y"))]
  )

  if (length(y) > 1) {
    gdata = tidyr::gather_(gdata, "group", "y", paste0("y", seq_along(y)))
    gdata[["group"]] =  factor(gdata[["group"]],
                               paste0("y", seq_along(y)),
                               names(y))

  } else {
    gdata = rename(gdata, y = y1)
  }

  if (yoy) {
    gdata[["group"]] = lubridate::year(gdata[["x"]])
    gdata[["x"]] = lubridate::yday(gdata[["x"]])
  }

  for (i in intersect(names(gdata), c("group", "facet_x", "facet_y"))) {
    gdata[[i]] = factor(gdata[[i]])
  }

  if (is.character(gdata[["x"]]) | is.factor(gdata[["x"]])) {

    gdata[["x"]] = factor(gdata[["x"]])
    x_text = levels(gdata[["x"]])
    gdata[["x"]] = as.numeric(gdata[["x"]])

  } else {
    x_text = NULL
  }

  g = ggplot(gdata)

  if ("group" %in% names(gdata)) {
    if (yoy) {
      g = g +
        geom_line(mapping = aes(x, y, colour = group),
                  size = size_line) +
        scale_color_manual(NULL,
                           values = palette(length(unique(gdata[["group"]]))),
                           labels = function(x) paste0(x, "   ")) +
        scale_x_continuous(breaks = c(1, 91, 182, 274, 366),
                           limits = c(1, 366),
                           labels = c("Jan", "Apr", "Jul", "Oct", "Jan")) +
        theme(legend.position = "top")
    } else {
      g = g +
        geom_line(aes(x, y, colour = group),
                  size = size_line) +
        scale_colour_manual(NULL,
                            values = palette(length(unique(gdata[["group"]]))),
                            labels = function(x) paste0(x, "   "))
    }
  } else {
    g = g +
      geom_line(aes(x, y),
                size = size_line,
                colour = palette(1))
  }

  g = quick_facet(g, scales = facet_scales)

  g = g +
    xlab(names(x)) +
    ylab(names(y)) +
    scale_y_continuous(labels = labels_y) +
    ylab(names(y)) +
    use_theme(size) +
    theme(legend.key.height = grid::unit(0, "lines"))

  if (!is.null(x_text)) {
    g = g +
      scale_x_continuous(breaks = seq_along(x_text),
                         labels = x_text) +
      theme(axis.text.x = element_text(angle = 90,
                                       vjust = 0.38,
                                       hjust = 1))
  }

  g + coord_cartesian(expand = FALSE)

}

globalVariables("y1")
