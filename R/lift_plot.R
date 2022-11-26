#' lift_plot
#' @description precision-recall plot
#' @inheritParams area_plot
#' @inheritParams line_plot
#' @inheritParams roc_plot
#' @export
#' @examples
#' library(ggplot2)
#' n = 1000
#' df = data.frame(actual = sample(c(FALSE, TRUE), n, replace = TRUE),
#'                 runif = runif(n))
#' df[["fitted"]] = runif(n) ^ ifelse(df[["actual"]] == 1, 0.5, 2)
#'
#' density_plot(df, "fitted", "actual")
#'
#' lift_plot(df, "fitted", "actual")
#' lift_plot(df, "fitted", "actual") + scale_y_log10()
#' lift_plot(df, "runif", "actual", size_line = 0.5)
#'
#'\donttest{
#' library(dplyr, warn.conflicts = FALSE)
#' lift_plot(df, "fitted", "actual", "sample(c(1, 2), n(), TRUE)")
#'
#' lift_plot(df, "fitted", "actual",
#'         "sample(c(1, 2), n(), TRUE)",
#'         "sample(c(3, 4), n(), TRUE)")
#'
#' lift_plot(df, "fitted", "actual",
#'         "sample(c(1, 2), n(), TRUE)",
#'         "sample(c(3, 4), n(), TRUE)",
#'         "sample(c(5, 6), n(), TRUE)")
#'}
lift_plot = function(data,
                     fitted,
                     actual,
                     group = NULL,
                     facet_x = NULL,
                     facet_y = NULL,
                     size_line = 1,
                     size = 11,
                     env = parent.frame()) {

  cols = c(actual = unname(actual),
           fitted = unname(fitted),
           group = unname(group),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y))

  data = data %>%
    ungroup %>%
    transmute(!!!lapply(cols,
                        function(x) rlang::parse_quo(x, env = env)))

  gdata = data %>%
    group_by(!!!syms(intersect(names(cols),
                               c("group", "facet_x", "facet_y")))) %>%
    summarize(values = list(perf(fitted, actual,
                                 x_measure = "rpp",
                                 y_measure = "lift"))) %>%
    ungroup %>%
    tidyr::unnest(values)

  g = ggplot(gdata)

  if (exists("group", gdata)) {
    g = g +
      geom_path(aes(x = x,
                    y = y,
                    colour = factor(group)),
                linewidth = size_line) +
      scale_colour_manual(NULL, values = ez_col(n_distinct(gdata[["group"]])))
  } else {
    g = g +
      geom_path(aes(x = x,
                    y = y),
                linewidth = size_line)
  }

  g = quick_facet(g)

  g = g +
    geom_path(data = data.frame(x = c(0, 1), y = 1),
              aes(x, y),
              linewidth = size_line,
              linetype = 2) +
    theme_minimal(size) +
    xlab('Rate of positive predictions') +
    ylab('Lift') +
    scale_y_continuous(labels = ez_labels, limits = c(0, NA)) +
    scale_x_continuous(labels = ez_labels, limits = c(0, 1)) +
    theme(plot.title = element_text(hjust = 0.5),
          aspect.ratio = 1)

  g

}

globalVariables(c("values"))

