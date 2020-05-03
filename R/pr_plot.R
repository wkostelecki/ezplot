#' pr_plot
#' @description precision-recall plot
#' @inheritParams area_plot
#' @inheritParams line_plot
#' @inheritParams roc_plot
#' @param actual Vector of actuals values
#' @param fitted Vector of fitted values
#' @export
#' @examples
#' library(ggplot2)
#' n = 1000
#' df = data.frame(actual = sample(c(FALSE, TRUE), n, replace = TRUE),
#'                 runif = runif(n))
#' df[["fitted"]] = runif(n) ^ ifelse(df[["actual"]] == 1, 0.5, 2)
#'
#' ggplot(df) +
#'   geom_density(aes(fitted, fill = actual), alpha = 0.5)
#'
#' pr_plot(df, "actual", "fitted")
#' pr_plot(df, "actual", "runif", size_line = 0.5)
#'
#'\donttest{
#' library(dplyr, warn.conflicts = FALSE)
#' pr_plot(df, "actual", "fitted", "sample(c(1, 2), n(), TRUE)")
#'
#' pr_plot(df, "actual", "fitted",
#'         "sample(c(1, 2), n(), TRUE)",
#'         "sample(c(3, 4), n(), TRUE)")
#'
#' pr_plot(df, "actual", "fitted",
#'         "sample(c(1, 2), n(), TRUE)",
#'         "sample(c(3, 4), n(), TRUE)",
#'         "sample(c(5, 6), n(), TRUE)")
#'}
pr_plot = function(data, actual, fitted,
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

  total = data %>%
    tibble::as_tibble() %>%
    summarize(prec_rec = list(prec_rec(actual, fitted)))

  gdata = data %>%
    group_by(!!!syms(intersect(names(cols),
                               c("group", "facet_x", "facet_y")))) %>%
    summarize(prec_rec = list(prec_rec(actual, fitted))) %>%
    ungroup %>%
    tidyr::unnest(prec_rec)

  g = ggplot(gdata)

  if (exists("group", gdata)) {
    g = g +
      geom_line(aes(x = recall,
                    y = precision,
                    colour = factor(group))) +
      scale_colour_manual(NULL, values = ez_col(n_distinct(gdata[["group"]])))
  } else {
    g = g +
      geom_line(aes(x = recall,
                    y = precision),
                size = size_line)
  }

  g = quick_facet(g)

  g = g +
    geom_line(data = data.frame(x = c(0, 1), y = c(0.5, 0.5)),
              aes(x, y),
              size = size_line,
              linetype = 2) +
    coord_equal() +
    theme_minimal(size) +
    xlab('Recall\nTrue Positive Rate\nSensitivity\nTP/P') +
    ylab('Precision\nPositive Predictive Value\nTP/(TP+FP)') +
    scale_y_continuous(labels = ez_labels, limits = c(0, 1)) +
    scale_x_continuous(labels = ez_labels, limits = c(0, 1)) +
    theme(plot.title = element_text(hjust = 0.5))

  g

}

globalVariables(c("false_positive", "true_positive", "x", "y"))



#' prec_rec
#' @description Precision recall calculation
#' @param actual Vector with two levels
#' @param fitted Vector with values between 0 and 1
#' @examples
#' ezplot:::prec_rec(sample(c(TRUE, FALSE), 1, replace = TRUE), runif(1))
#' ezplot:::prec_rec(sample(c(TRUE, FALSE), 5, replace = TRUE), runif(5))
prec_rec = function(actual, fitted) {

  ind = !is.na(actual) & !is.na(fitted)
  actual = actual[ind]
  fitted = fitted[ind]

  count = sum(actual == actual[1])
  if (sum(ind) == 0 | count == 0 | count == length(actual)) {
    return(
      data.frame(
        rec = NA,
        prec = NA
      )
    )
  }

  pred = ROCR::prediction(as.numeric(fitted), actual)
  perf = ROCR::performance(pred, "prec", "rec") # precision = ppv, recall = TPR

  x = perf@x.values[[1]]
  y = perf@y.values[[1]]


  data.frame(recall = x,
             precision = y) %>%
    filter(!is.na(precision))

}
