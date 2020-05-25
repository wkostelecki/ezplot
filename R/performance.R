
#' perf
#' @description Precision recall calculation
#' @param actual Vector with two levels
#' @param fitted Vector with values between 0 and 1
#' @param y_measure metric for ROCR::performance
#' @param x_measure metric for ROCR::performance
#' @examples
#' ezplot:::perf(runif(1), sample(c(TRUE, FALSE), 1, replace = TRUE), "rpp", "lift")
#' ezplot:::perf(runif(10), sample(c(TRUE, FALSE), 10, replace = TRUE), "rpp", "lift")
#' ezplot:::perf(runif(5), sample(c(TRUE, FALSE), 5, replace = TRUE), "rec", "prec")
#' ezplot:::perf(runif(5), sample(c(TRUE, FALSE), 5, replace = TRUE), "fpr", "tpr")
perf = function(fitted, actual, x_measure, y_measure) {

  pred = pred(fitted, actual)

  if (is.null(pred)) {
    return(data.frame(cutoff = NA, x = NA, y = NA))
  }

  perf = ROCR::performance(pred,
                           x.measure = x_measure,
                           measure = y_measure)

  if (x_measure == "fpr" && y_measure == "tpr") {
    auc = methods::slot(ROCR::performance(pred, "auc"), "y.values")[[1]]
  } else if (any(x_measure == c("rec", "tpr")) && y_measure == "prec") {
    auc = methods::slot(ROCR::performance(pred, "aucpr"), "y.values")[[1]]
  } else {
    auc = NULL
  }

  x = perf@x.values[[1]]
  y = perf@y.values[[1]]
  cutoff = perf@alpha.values[[1]]

  df = data.frame(cutoff = cutoff,
                  x = x,
                  y = y) %>%
    filter(is.finite(y))

  df$auc = auc

  df

}

pred = function(fitted, actual) {
  ind = !is.na(actual) & !is.na(fitted)
  actual = actual[ind]
  fitted = fitted[ind]

  count = sum(actual == actual[1])
  if (sum(ind) == 0 | count == 0 | count == length(actual)) {
    return(NULL)
  }
  ROCR::prediction(as.numeric(fitted), actual)
}

#' perf_df
#' @inheritParams performance_plot
#' @export
#' @examples
#' perf_df(mtcars$mpg, mtcars$am)
#' perf_df(mtcars$wt, mtcars$am==0)
perf_df = function(fitted, actual) {
  pred = pred(fitted, actual)
  # performance(pred, )
  df = data.frame(cutoffs = pred@cutoffs[[1]],
                  fp = pred@fp[[1]],
                  tp = pred@tp[[1]],
                  tn = pred@tn[[1]],
                  fn = pred@fn[[1]],
                  pp = pred@n.pos.pred[[1]],
                  np = pred@n.neg.pred[[1]])

  for (metric in c("rpp", "acc", "fpr", "tpr", "fnr", "tnr", "prec", "lift", "auc", "aucpr")) {

    df[[metric]] = ROCR::performance(pred, metric)@y.values[[1]]

  }
  df %>%
    dplyr::mutate(ks = abs(fpr - tpr)) %>%
    dplyr::arrange(desc(cutoffs))
}

globalVariables(c("tpr", "fpr", "cutoffs"))
