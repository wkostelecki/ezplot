#' @title distribution_plot
#' @inheritParams area_plot
#' @param nbins Number of bins for histogram. Default is 20.
#' @name distribution_plot
#' @export
#' @examples
#' n = 100
#' df = data.frame(residuals = rnorm(n),
#'                 group1 = sample(c("a", "b"), n, replace = TRUE))
#' distribution_plot(df, "residuals")
#' distribution_plot(df, "residuals", "group1")
distribution_plot = function (data,
                              x,
                              facet_x = NULL,
                              nbins = 20,
                              use_theme = theme_ez,
                              size = 11,
                              env = parent.frame()) {

  cols = c(x = unname(x),
           facet_x = unname(facet_x))

  data = data %>%
    transmute(!!!lapply(cols,
                        function(x) rlang::parse_quo(x, env = env)))

  binwidth = diff(range(data[["x"]])) / nbins

  # Durbin-Watson calculation
  dw = function (x) {
    x = x - mean(x, na.rm = TRUE)
    sum(diff(x) ^ 2, na.rm = TRUE) /
      sum(x ^ 2, na.rm = TRUE)
  }

  data = data %>%
    group_by(!!!syms(intersect("facet_x", names(cols)))) %>%
    mutate(facet_x_label = paste0(
      ifelse(facet_x == "", "",
             paste0(facet_x, "\n")),
      "N = ",
      ez_labels(sum(!is.na(x))),
      "; Mean = ",
      ez_labels(signif(mean(x), 3)),
      "; SD = ",
      ez_labels(signif(stats::sd(x), 3)),
      "\nDW = ",
      ez_labels(signif(dw(x), 3)),
      "; Skew = ",
      ez_labels(signif(psych::skew(x), 3)),
      "; Kurt = ",
      ez_labels(signif(psych::kurtosi(x), 3))
      # "; KS = ",
      # ez_labels(signif(ks.test(rnorm(100), "pnorm")[["statistic"]], 3))
    )) %>%
    ungroup()

  ggplot(data) +
    geom_histogram(aes(x, y = ..density..),
                   stat = "bin",
                   binwidth = binwidth) +
    scale_x_continuous(labels = ez_labels) +
    scale_y_continuous(labels = ez_labels,
                       expand = c(0, 0, 0.1, 0)) +
    use_theme(size) +
    facet_wrap(~ facet_x_label) +
    labs(y = "Density",
         x = names(x))

}


globalVariables(c("..density.."))
