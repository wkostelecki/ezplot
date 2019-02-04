#' roc_plot
#' @inheritParams area_plot
#' @param actual Vector of actuals values
#' @param fitted Vector of fitted values
#' @export
#' @examples
#' library(ggplot2)
#' n = 10000
#' df = data.frame(actual = sample(c(FALSE, TRUE), n, replace = TRUE),
#'                 runif = runif(n))
#' df[["fitted"]] = runif(n) ^ ifelse(df[["actual"]] == 1, 0.5, 2)
#'
#' ggplot(df) +
#'   geom_density(aes(fitted, fill = actual), alpha = 0.5)
#'
#' roc_plot(df, "actual", "actual")
#' roc_plot(df, "actual", "fitted")
#' roc_plot(df, "actual", "runif")
#'
#'\donttest{
#' roc_plot(df, "actual", "fitted", "sample(c(1, 2), n(), TRUE)")
#'
#' roc_plot(df, "actual", "fitted",
#'          "sample(c(1, 2), n(), TRUE)",
#'          "sample(c(3, 4), n(), TRUE)")
#'
#' roc_plot(df, "actual", "fitted",
#'          "sample(c(1, 2), n(), TRUE)",
#'          "sample(c(3, 4), n(), TRUE)",
#'          "sample(c(5, 6), n(), TRUE)")
#'}
roc_plot = function(data, actual, fitted,
                    group = NULL,
                    facet_x = NULL,
                    facet_y = NULL,
                    size = 14){

  cols = c(actual = unname(actual),
           fitted = unname(fitted),
           group = unname(group),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y))

  data = data %>%
    ungroup %>%
    transmute_(.dots = cols)

  total = data %>%
    tibble::as.tibble() %>%
    summarize(roc = list(roc(actual, fitted)))

  gdata = data %>%
    group_by_(.dots = intersect(names(cols),
                                c("group", "facet_x", "facet_y"))) %>%
    summarize(roc = list(roc(actual, fitted))) %>%
    ungroup %>%
    tidyr::unnest(roc)

  g = ggplot(gdata)

  if (exists("group", gdata)) {
    g = g +
      geom_line(aes(x = false_positive,
                    y = true_positive,
                    colour = factor(group))) +
      scale_colour_manual(NULL, values = ez_col(n_distinct(gdata[["group"]])))
  } else {
    g = g +
      geom_line(aes(x = false_positive,
                    y = true_positive))
  }

  g = quick_facet(g)

  g = g +
    geom_line(data = data.frame(x = c(0, 1), y = c(0, 1)),
              aes(x, y),
              linetype = 2) +
    coord_equal() +
    theme_minimal(size) +
    xlab('False Positive Rate') +
    ylab('True Positive Rate') +
    scale_y_continuous(labels = ez_labels) +
    scale_x_continuous(labels = ez_labels) +
    ggtitle(paste0("AUC = ", signif(total[["roc"]][[1]][["auc"]][1], 3))) +
    theme(plot.title = element_text(hjust = 0.5))

  g

}

globalVariables(c("false_positive", "true_positive", "x", "y"))

#' roc
#' @description Calculates ROC and AUC
#' @param actual Vector with two levels
#' @param fitted Vector with values between 0 and 1
#' @examples
#' ezplot:::roc(sample(c(TRUE, FALSE), 1, replace = TRUE), runif(1))
#' ezplot:::roc(sample(c(TRUE, FALSE), 3, replace = TRUE), runif(3))
roc = function(actual, fitted) {

  ind = !is.na(actual) & !is.na(fitted)
  actual = actual[ind]
  fitted = fitted[ind]

  count = sum(actual == actual[1])
  if (sum(ind) == 0 | count == 0 | count == length(actual)) {
    return(
      data.frame(
        true_positive = NA,
        false_positive = NA,
        auc = NA
      )
    )
  }

  pred = ROCR::prediction(as.numeric(fitted), actual)
  perf = ROCR::performance(pred, "tpr", "fpr")
  auc = methods::slot(ROCR::performance(pred, "auc"), "y.values")[[1]]

  x = perf@x.values[[1]]
  y = perf@y.values[[1]]


  data.frame(false_positive = x,
             true_positive = y,
             auc = auc)

}

