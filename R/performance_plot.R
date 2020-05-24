#' performance_plot
#' @description plots binary classification performance metrics
#' @inheritParams area_plot
#' @inheritParams line_plot
#' @param x ROCR::performance() measure
#' @param y ROCR::performance() measure
#' @export
#' @examples
#' performance_plot(mtcars, "-disp", "am")
#' performance_plot(mtcars, "-disp", "am", "cyl")
#' performance_plot(mtcars, "-disp", "am", "cyl", x = "rec", y = "prec")
#' performance_plot(mtcars, "-disp", "am", x = "rpp", y = "gain")
performance_plot = function(data,
                            fitted,
                            actual,
                            group = NULL,
                            facet_x = NULL,
                            facet_y = NULL,
                            x = "fpr",
                            y = "tpr",
                            auc = c("title", "group"),
                            size_line = 1,
                            size = 11,
                            env = parent.frame()) {

  x_measure = measure_standard(x)
  y_measure = measure_standard(y)

  cols = c(actual = unname(actual),
           fitted = unname(fitted),
           group = unname(group),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y))

  data = data %>%
    ungroup %>%
    transmute(!!!lapply(cols, function(x) rlang::parse_quo(x, env = env)))

  gdata = data %>%
    group_by(!!!syms(intersect(names(cols),
                               c("group", "facet_x", "facet_y")))) %>%
    summarize(values = list(perf(fitted,
                                 actual,
                                 x_measure = x_measure,
                                 y_measure = y_measure))) %>%
    ungroup %>%
    tidyr::unnest(values)

  for (this_name in names(c(group = group, facet_x = facet_x, facet_y = facet_y))) {
    gdata[[this_name]] = factor(gdata[[this_name]])
  }

  if (exists("auc", gdata)) {

    # title auc
    if (any("title" == auc)){
      if (any(c("group", "facet_x", "facet)y") %in% names(gdata))) {
        auc_title = (data %>%
                       summarize(values = list(perf(fitted,
                                                    actual,
                                                    x_measure = x_measure,
                                                    y_measure = y_measure))) %>%
                       pull(values))[[1]] %>%
          pull(auc) %>%
          head(1)
      } else {
        auc_title = gdata$auc[1]
      }

      auc_title = round(auc_title, 3)
    }

    # group auc
    if (exists("group", gdata) && any("group" == auc)) {

      if (any(c("facet_x", "facet_y") %in% names(gdata))) {
        auc_group = data %>%
          group_by(group) %>%
          summarize(values = perf(fitted,
                                  actual,
                                  x_measure = x_measure,
                                  y_measure = y_measure)$auc[1]) %>%
          ungroup
      } else {
        auc_group = gdata %>%
          dplyr::group_by(group) %>%
          dplyr::summarize(auc = auc[1])
      }

      auc_group = auc_group %>%
        dplyr::mutate(levels = paste0(group, " (AUC =", round(auc, 3), ")"))

      levels(gdata$group) = auc_group$levels

    }

  }

  x_base = measure_baseline(x_measure, data$actual)
  y_base = measure_baseline(y_measure, data$actual)

  g = ggplot(gdata) +
    geom_path(data = data.frame(x = x_base,
                                y = y_base),
              aes(x, y),
              size = size_line,
              linetype = 2)

  if (exists("group", gdata)) {
    g = g +
      geom_path(aes(x = x,
                    y = y,
                    colour = group),
                size = size_line) +
      scale_colour_manual(NULL, values = ez_col(n_distinct(gdata[["group"]])))
  } else {
    g = g +
      geom_path(aes(x = x,
                    y = y),
                size = size_line)
  }

  g = quick_facet(g)

  g = g +
    theme_minimal(size) +
    xlab(measure_label(x)) +
    ylab(measure_label(y)) +
    scale_y_continuous(labels = ez_labels, limits = c(0, NA)) +
    scale_x_continuous(labels = ez_labels, limits = c(0, 1)) +
    theme(plot.title = element_text(hjust = 0.5),
          aspect.ratio = 1)

  if (exists("auc", gdata) && any("title" == auc)) {
    g = g + ggtitle(paste("AUC =", auc_title))
  }

  g

}

measure_baseline = function(measure, actual) {
  if (any(measure == c("rpp", "tpr", "rec", "fpr", "fall", "sens", "rec", "gain"))) {
    return(c(0, 1))
  } else if (any(measure == c("fnr", "tnr", "spec"))) {
    return(c(1, 0))
  } else if (any(measure == c("lift"))) {
    return(c(1, 1))
  } else if (any(measure == c("prec", "ppv"))) {
    return(mean(actual))
  } else {
    stop("unknown measure range in measure_base()")
  }
}

measure_label = function(measure) {

  switch(measure,
         tpr = "True positive rate",
         fpr = "False positive rate",
         tnr = "True negative rate",
         fnr = "False negative rate",
         acc = "Accuracy",
         err = "Error rate",
         fall = "Fallout",
         rec = "Recall",
         sens = "Sensitivity",
         miss = "Miss",
         spec = "Specificity",
         ppv = "Positive predictive value",
         prec = "Precision",
         npv = "Negative predictive value",
         rpp = "Rate of positive predictions",
         rnp = "Rate of negative predictions",
         lift = "Lift",
         gain  = "Gain")

}

measure_standard = function(measure) {
  dplyr::case_when(measure == "rec" ~ "tpr",
                   measure == "sens" ~ "tpr",
                   measure == "gain" ~ "tpr",
                   measure == "fall" ~ "fpr",
                   measure == "spec" ~ "fpr",
                   measure == "ppv" ~ "prec",
                   TRUE ~ measure)

}
