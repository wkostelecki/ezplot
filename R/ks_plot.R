#' ks_plot
#' @description ks plot
#' @inheritParams roc_plot
#' @export
#' @examples
#' ks_plot(mtcars, "-disp", "am")
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

    data = perf_df(data$fitted, data$actual)

    ind = which.max(data$ks)

    g = ggplot(data) +
      geom_line(aes(cutoffs, fnr), size = size_line,
                colour = palette(2)[2]) +
      geom_line(aes(cutoffs, tnr), size = size_line,
                colour = palette(2)[1]) +
      geom_line(data = data.frame(x = data$cutoffs[ind],
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
      ggtitle(paste("KS =", ez_labels(data$ks[ind], signif = 3)))

    g

  }

globalVariables(c("fnr", "tnr"))
