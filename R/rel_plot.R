
#' rel_plot
#' @examples
#' rel_plot(fruit, "Units", "Value")
#' rel_plot(fruit, "Units", "Value", "paste(Product, Size, Store)")
#' rel_plot(fruit, "Product", "Value")
#' @export
rel_plot = function(data, x,  y, group = NULL,
                    size = 20,
                    point_size = 2.5) {

  cols = c(x = unname(x),
           y = unname(y),
           group = unname(group))

  gdata = data %>%
    transmute_(.dots = cols)

  if (!exists("group", gdata)) {
    gdata[["group"]] = ""
  }

  n_group = length(unique(gdata[["group"]]))

  if (is.numeric(gdata[["x"]])) {
    g = ggplot(gdata) +
      geom_point(aes(x, y, color = group), size = point_size) +
      geom_smooth(aes(x, y, color = group), method = "lm") +
      scale_color_manual(NULL, values = ez_col(n_group),
                         labels = function(x) paste0(x, "   ")) +
      scale_x_continuous(labels = ez_labels) +
      scale_y_continuous(labels = ez_labels)
  } else {
    g = ggplot(gdata) +
      geom_boxplot(aes(x, y))
  }

  g = g +
    theme_ez(size) +
    xlab(names(x)) +
    ylab(names(y))

  if (n_group == 1) {
    g = g + theme(legend.position = "none")
  } else {
    g = g + theme(legend.key = element_rect(colour = NA,
                                            fill = NA),
                  legend.key.height = grid::unit(1.5, "lines"))
  }

  g +
    guides(color=guide_legend(override.aes=list(fill = NA)))


}
