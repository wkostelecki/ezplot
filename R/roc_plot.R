#' roc_plot
#' @examples
#' n = 500
#' actual = sample(c(FALSE, TRUE), n, replace = TRUE)
#' roc_plot(actual, actual)
#' roc_plot(actual, runif(n))
#' fitted = runif(n) ^ ifelse(actual == 1, 0.5, 2)
#' roc_plot(actual, fitted)
#'
#' @export
roc_plot = function(actual, fitted, size = 20){

  pred = ROCR::prediction(as.numeric(fitted), actual)
  perf = ROCR::performance(pred, "tpr", "fpr")
  auc = slot(ROCR::performance(pred, "auc"), "y.values")[[1]]

  g = ggplot(data.frame(x = perf@x.values[[1]],
                        y = perf@y.values[[1]])) +
    geom_line(aes(x, y)) +
    geom_line(data = data.frame(x = c(0, 1), y = c(0, 1)),
              aes(x, y),
              linetype = 2) +
    coord_equal() +
    theme_minimal(size) +
    xlab('False Positive Rate') +
    ylab('True Positive Rate') +
    ggtitle(paste0("AUC = ", signif(auc, 3))) +
    theme(plot.title = element_text(hjust = 0.5))

  g

}


