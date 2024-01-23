
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
#' ezplot:::perf(runif(5), sample(c(TRUE, FALSE), 5, replace = TRUE), "cutoff", "tpr")
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

  if (length(perf@alpha.values) == 1) {
    cutoff = perf@alpha.values[[1]]
  } else {
    if (x_measure == "cutoff") {
      cutoff = x
    } else if (y_measure == "cutoff") {
      cutoff = y
    } else {
      stop("cutoff not provided by ROCR::performance()")
    }
  }

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
#' @description shows classification performance statistics as a table
#' @inheritParams performance_plot
#' @param quantiles Number of quantiles to show. If \code{NULL}, uses distinct
#'   values of \code{fitted} for the cutoffs rather than showing quantiles.
#' @export
#' @returns A data.frame summarizing binary classification performance:
#' \itemize{
#'   \item{quantile:} {fitted value quantile (only if \code{!is.null(quantile)}})
#'   \item{cutoff:} {fitted value cutoff}
#'   \item{fp:} {false positives}
#'   \item{tp:} {true postives}
#'   \item{tn:} {true negatives}
#'   \item{fn:} {false negatives}
#'   \item{pp:} {positive predictions}
#'   \item{np:} {negative predictions}
#'   \item{ipp:} {group positive predictions}
#'   \item{ifp:} {group false positives}
#'   \item{itp:} {group true postives}
#'   \item{rpp:} {rate of positive predictions}
#'   \item{acc:} {accuracy}
#'   \item{fpr:} {false positive rate}
#'   \item{tpr:} {true positive rate}
#'   \item{fnr:} {false negative rate}
#'   \item{tnr:} {true negative rate}
#'   \item{prec:} {precision}
#'   \item{clift:} {lift}
#'   \item{ilift:} {group lift}
#'   \item{f1:} {f1 measure}
#'   \item{ks:} {Kolmogorov-Smirnov statistic}
#'   \item{auc:} {area under ROC curve}
#'   \item{aucpr:} {area under PR curve}
#' }
#' @examples
#' perf_df(mtcars$mpg, mtcars$am)
#' perf_df(mtcars$mpg, mtcars$am, quantiles = 4)
#' perf_df(mtcars$mpg, mtcars$am, quantiles = 8)
#' perf_df(mtcars$mpg, mtcars$am, quantiles = 10)
#' perf_df(mtcars$wt, mtcars$am==0)
perf_df = function(fitted, actual, quantiles = NULL) {
  if (!is.null(quantiles)) {
    quantiles = min(quantiles, length(fitted))
    bins = pmax(ceiling(seq(0, quantiles, length.out = length(fitted))), 1)
    r = rank(fitted, ties.method = "first")
    x = data.frame(r,
                   # o,
                   fitted,
                   c = bins[r]) %>%
      dplyr::group_by(c) %>%
      dplyr::mutate(qrank = min(r))

    fitted = x$qrank
    ocutoffs = x %>%
      dplyr::group_by(qrank) %>%
      dplyr::summarize(cutoff = min(fitted),
                       .groups = "drop")
  }
  pred = pred(fitted, actual)

  df = data.frame(cutoff = pred@cutoffs[[1]],
                  fp = pred@fp[[1]],
                  tp = pred@tp[[1]],
                  tn = pred@tn[[1]],
                  fn = pred@fn[[1]],
                  pp = pred@n.pos.pred[[1]],
                  np = pred@n.neg.pred[[1]],
                  ipp = c(0, diff(pred@n.pos.pred[[1]])),
                  ifp = c(0, diff(pred@fp[[1]])),
                  itp = c(0, diff(pred@tp[[1]])))

  df[["rpp"]] = ROCR::performance(pred, "rpp")@y.values[[1]]
  df[["acc"]] = ROCR::performance(pred, "acc")@y.values[[1]]
  df[["fpr"]] = ROCR::performance(pred, "fpr")@y.values[[1]]
  df[["tpr"]] = ROCR::performance(pred, "tpr")@y.values[[1]]
  df[["fnr"]] = ROCR::performance(pred, "fnr")@y.values[[1]]
  df[["tnr"]] = ROCR::performance(pred, "tnr")@y.values[[1]]
  df[["prec"]] = ROCR::performance(pred, "prec")@y.values[[1]]
  df[["clift"]] = ROCR::performance(pred, "lift")@y.values[[1]]

  if (!is.null(quantiles)) {
    df = df %>%
      utils::tail(quantiles) %>% # drop first row
      dplyr::mutate(cutoff = rev(ocutoffs$cutoff)) %>% # convert to original scale
      dplyr::bind_cols(quantile = seq_len(quantiles), .)
  }

  df = df %>%
    dplyr::mutate(
      ilift = (itp / ipp) / (tp[n()] / pp[n()]),
      f1 = 2 * prec * tpr / (prec + tpr),
      ks = abs(fpr - tpr)
    )

  df[["auc"]] = ROCR::performance(pred, "auc")@y.values[[1]]
  df[["aucpr"]] = ROCR::performance(pred, "aucpr")@y.values[[1]]

  df
}

globalVariables(c("pp", "tp", "tpr", "fpr", "cutoffs", "prec", "qrank"))
