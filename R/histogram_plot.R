

#' histogram_plot
#' @description creates a histogram plot
#' @inheritParams area_plot
#' @param bins number of bins
#' @param alpha fill alpha
#' @export
#' @examples
#' histogram_plot(airquality, "Wind", group = "Month")
#' histogram_plot(airquality, "Wind", "..density..", facet_x = "Month")
histogram_plot = function(data, x,
                          y = "..count..",
                          group = NULL, facet_x = NULL,
                          facet_y = NULL,
                          palette = ez_col,
                          position = "stack",
                          bins = 30,
                          alpha = 0.5,
                          facet_scales = "fixed",
                          facet_ncol = NULL,
                          legend_ncol = NULL,
                          env = parent.frame()) {

  cols = c(x = unname(x),
           group = unname(group),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y))

  gdata = data %>%
    transmute(!!!lapply(cols,
                        function(x) rlang::parse_quo(x, env = env)))

  if (exists("group", gdata)) {
    g = ggplot(gdata) +
      geom_histogram(aes_string("x",
                                fill = "forcats::fct_rev(factor(group))",
                                y = y),
                     position = position,
                     bins = bins,
                     alpha = alpha) +
      scale_fill_manual(NULL,
                        values = rev(palette(n_distinct(gdata[["group"]]))),
                        breaks = rev,
                        labels = function(x) paste0(x, "   "),
                        guide = guide_legend(ncol = legend_ncol))
  } else {
    g = ggplot(gdata) +
      geom_histogram(aes_string("x",
                                y = y),
                     fill = palette(1), bins = bins)
  }

  quick_facet(g, scales = facet_scales, ncol = facet_ncol) +
    theme_ez() +
    scale_y_continuous(labels = ez_labels, expand = c(0, 0)) +
    xlab(names(nameifnot(x)))

}
