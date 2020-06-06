#' density_plot
#' @description creates a density plot
#' @inheritParams area_plot
#' @param adjust multiplicate bandwidth adjustment
#' @param alpha alpha
#' @export
#' @examples
#' library(tsibbledata)
#' density_plot(mtcars, "wt", "cyl")
#' density_plot(subset(tsibbledata::olympic_running, Length == 100 & Year >= 1980),
#'              "Time", "Year - Year %% 10", "Sex", facet_scales = "free", facet_ncol = 1, adjust = 2)
density_plot = function(data, x, group = NULL, facet_x = NULL,
                        facet_y = NULL,
                        palette = ez_col,
                        adjust = 1,
                        alpha = 0.5,
                        facet_scales = "fixed",
                        facet_ncol = NULL,
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
      geom_density(aes(x, fill = factor(group)),
                   #colour = NA,
                   outline.type = "full",
                   adjust = adjust,
                   alpha = alpha) +
      scale_fill_manual(names(group), values = palette(n_distinct(gdata[["group"]])),
                        labels = function(x) paste0(x, "   "))
  } else {
    g = ggplot(gdata) +
      geom_density(aes(x), fill = palette(1))

  }

  quick_facet(g, scales = facet_scales, ncol = facet_ncol) +
    theme_ez() +
    scale_y_continuous(labels = ez_labels, expand = c(0, 0)) +
    xlab(names(nameifnot(x)))

}
