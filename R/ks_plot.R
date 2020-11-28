#' ks_plot
#' @description ks plot
#' @inheritParams roc_plot
#' @export
#' @examples
#' ks_plot(mtcars, "-disp", "am")
#' x = c(rnorm(100), rnorm(100) + 2)
#' label = c(rep('low', 100), rep('high', 100))
#' ks_plot(data.frame(x, label), "x", "label")
#' ks_plot(data.frame(x, label = factor(label, c('low', 'high'))), "x", "label")
ks_plot = function(data,
                   fitted,
                   actual,
                   palette = ez_col,
                   size_line = 1,
                   size = 11,
                   env = parent.frame()) {

    cols = c(actual = unname(actual),
             fitted = unname(fitted))

    data = data %>%
      ungroup %>%
      transmute(!!!lapply(cols,
                          function(x) rlang::parse_quo(x, env = env)))

    actuals = as.character(sort(unique(data$actual)))
    stopifnot(length(actuals) == 2)

    colours = palette(2) %>% stats::setNames(actuals)

    data = perf_df(data$fitted, data$actual == actuals[2])

    ind = which.max(data$ks)

    g = ggplot(data) +
      geom_line(aes(cutoff, tnr, colour = actuals[1]),
                # colour = palette(2)[1],
                size = size_line) +
      geom_line(aes(cutoff, fnr, colour = actuals[2]),
                # colour = palette(2)[2],
                size = size_line) +
      geom_line(data = data.frame(x = data$cutoff[ind],
                                  y = c(data$fnr[ind], data$tnr[ind])),
                aes(x, y),
                size = size_line,
                linetype = 2) +
      theme_ez(base_size = size) +
      xlab(NULL) +
      ylab('Cumulative Probability') +
      scale_y_continuous(labels = ez_labels) +
      scale_x_continuous(labels = ez_labels) +
      theme(plot.title = element_text(hjust = 0.5),
            aspect.ratio = 1) +
      ggtitle(paste("KS =", ez_labels(data$ks[ind], signif = 3))) +
      scale_colour_manual(NULL, values = colours, breaks = actuals)

    g

  }

globalVariables(c("fnr", "tnr", "cutoff"))
