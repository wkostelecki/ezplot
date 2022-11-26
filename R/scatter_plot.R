
#' scatter plot
#' @description create a scatter plot
#' @inheritParams area_plot
#' @inheritParams model_plot
#' @examples
#' scatter_plot(mtcars, "wt", "hp")
#' scatter_plot(mtcars, "wt", "hp", "factor(cyl)")
#' scatter_plot(mtcars, "factor(cyl)", "hp")
#' @export
scatter_plot = function(data, x,  y, group = NULL,
                        palette = ez_col,
                        size = 11,
                        point_size = 2.5,
                        env = parent.frame()) {

  cols = c(x = unname(x),
           y = unname(y),
           group = unname(group))

  gdata = data %>%
    as.data.frame() %>%
    transmute(!!!lapply(cols,
                        function(x) rlang::parse_quo(x, env = env)))

  if (!exists("group", gdata)) {
    gdata[["group"]] = ""
  } else {
    gdata[["group"]] = factor(gdata[["group"]])
  }

  n_group = length(unique(gdata[["group"]]))

  if (is.numeric(gdata[["x"]])) {
    g = ggplot(gdata) +
      geom_point(aes(x, y, color = group), size = point_size) +
      geom_smooth(aes(x, y, color = group), method = "lm", formula = y ~ x) +
      scale_color_manual(NULL, values = palette(n_group),
                         labels = function(x) paste0(x, "   ")) +
      scale_x_continuous(labels = ez_labels) +
      scale_y_continuous(labels = ez_labels)
  } else {
    g = ggplot(gdata) +
      geom_point(aes(x, y, colour = group),
                 size = 0.8,
                 na.rm = TRUE) +
      scale_color_manual(NULL, values = palette(n_group),
                         labels = function(x) paste0(x, "   ")) +
      scale_y_continuous(labels = ez_labels)
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
    guides(color=guide_legend(override.aes = list(fill = NA))) +
    theme(axis.line.x = element_line(color = "grey85",
                                     linewidth = if (size > 16) 0.8 else 0.2))


}
