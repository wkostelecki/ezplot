
#' @examples
#' actual = sample(c(0, 1), 100, replace = TRUE)
#' fitted = runif(100)
#' roc_plot(actual, fitted)
#'
#' @export
roc_plot = function(actual, fitted){

  pred = ROCR::prediction(fitted, actual)
  perf = ROCR::performance(pred, "tpr", "fpr")
  auc = slot(ROCR::performance(pred, "auc"), "y.values")[[1]]

  g = ggplot(data.frame(x = perf@x.values[[1]],
                        y = perf@y.values[[1]])) +
    geom_line(aes(x, y)) +
    coord_equal() +
    theme_minimal() +
    xlab('False Positive Rate') +
    ylab('True Positive Rate') +
    ggtitle(paste0("AUC = ", signif(auc, 3)))

  g

}


