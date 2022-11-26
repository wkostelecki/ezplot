#' pr_plot
#' @description precision-recall plot
#' @inheritParams area_plot
#' @inheritParams line_plot
#' @inheritParams roc_plot
#' @param labs 'short' or 'long'
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
#' pr_plot(df, "fitted", "actual")
#' pr_plot(df, "runif", "actual", size_line = 0.5)
#'
#'\donttest{
#' library(dplyr, warn.conflicts = FALSE)
#' pr_plot(df, "fitted", "actual", "sample(c(1, 2), n(), TRUE)")
#'
#' pr_plot(df, "fitted", "actual",
#'         "sample(c(1, 2), n(), TRUE)",
#'         "sample(c(3, 4), n(), TRUE)")
#'
#' pr_plot(df, "fitted", "actual",
#'         "sample(c(1, 2), n(), TRUE)",
#'         "sample(c(3, 4), n(), TRUE)",
#'         "sample(c(5, 6), n(), TRUE)")
#'}
pr_plot = function(data,
                   fitted,
                   actual,
                   group = NULL,
                   facet_x = NULL,
                   facet_y = NULL,
                   palette = ez_col,
                   size_line = 1,
                   size = 11,
                   labs = "short",
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
    summarize(prec_rec = list(prec_rec(fitted, actual)))

  gdata = data %>%
    group_by(!!!syms(intersect(names(cols),
                               c("group", "facet_x", "facet_y")))) %>%
    summarize(prec_rec = list(prec_rec(fitted, actual))) %>%
    ungroup %>%
    tidyr::unnest(prec_rec)

  g = ggplot(gdata)

  if (exists("group", gdata)) {
    g = g +
      geom_path(aes(x = recall,
                    y = precision,
                    colour = factor(group)),
                linewidth = size_line) +
      scale_colour_manual(NULL, values = palette(n_distinct(gdata[["group"]])))
  } else {
    g = g +
      geom_path(aes(x = recall,
                    y = precision),
                linewidth = size_line)
  }

  g = quick_facet(g)

  if (labs == "long") {
    xlab = 'Recall\nTrue Positive Rate\nSensitivity\nTP/P'
    ylab = 'Precision\nPositive Predictive Value\nTP/(TP+FP)'
  } else {
    xlab = "Recall"
    ylab = "Precision"
  }

  g = g +
    geom_path(data = data.frame(x = c(0, 1)),
              y = mean(data$actual),
              aes(x, y),
              linewidth = size_line,
              linetype = 2) +
    coord_equal() +
    theme_minimal(size) +
    xlab(xlab) +
    ylab(ylab) +
    scale_y_continuous(labels = ez_labels, limits = c(0, 1)) +
    scale_x_continuous(labels = ez_labels, limits = c(0, 1)) +
    theme(plot.title = element_text(hjust = 0.5))

  g

}

globalVariables(c("precision", "recall"))



#' prec_rec
#' @description Precision recall calculation
#' @param fitted Vector with values between 0 and 1
#' @param actual Vector with two levels
#' @examples
#' ezplot:::prec_rec(runif(1), sample(c(TRUE, FALSE), 1, replace = TRUE))
#' ezplot:::prec_rec(runif(5), sample(c(TRUE, FALSE), 5, replace = TRUE))
prec_rec = function(fitted, actual) {

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
