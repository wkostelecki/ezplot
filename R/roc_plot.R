#' roc_plot
#' @inheritParams area_plot
#' @inheritParams line_plot
#' @param fitted Vector of fitted values
#' @param actual Vector of actual values
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
#' roc_plot(df, "actual", "actual")
#' roc_plot(df, "fitted", "actual")
#' roc_plot(df, "runif", "actual", size_line = 0.5)
#'
#'\donttest{
#' library(dplyr, warn.conflicts = FALSE)
#' roc_plot(df, "fitted", "actual", "sample(c(1, 2), n(), TRUE)")
#'
#' roc_plot(df, "fitted", "actual",
#'          "sample(c(1, 2), n(), TRUE)",
#'          "sample(c(3, 4), n(), TRUE)")
#'
#' roc_plot(df, "fitted", "actual",
#'          "sample(c(1, 2), n(), TRUE)",
#'          "sample(c(3, 4), n(), TRUE)",
#'          "sample(c(5, 6), n(), TRUE)")
#'}
roc_plot = function(data,
                    fitted,
                    actual,
                    group = NULL,
                    facet_x = NULL,
                    facet_y = NULL,
                    palette = ez_col,
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
    summarize(roc = list(roc(fitted, actual)))

  gdata = data %>%
    group_by(!!!syms(intersect(names(cols),
                               c("group", "facet_x", "facet_y")))) %>%
    summarize(roc = list(roc(fitted, actual))) %>%
    ungroup %>%
    tidyr::unnest(roc)

  g = ggplot(gdata)

  if (exists("group", gdata)) {
    g = g +
      geom_path(aes(x = x,
                    y = y,
                    colour = factor(group)),
                size = size_line) +
      scale_colour_manual(NULL, values = palette(n_distinct(gdata[["group"]])))
  } else {
    g = g +
      geom_path(aes(x = x,
                    y = y),
                size = size_line)
  }

  g = quick_facet(g)

  g = g +
    geom_path(data = data.frame(x = c(0, 1), y = c(0, 1)),
              aes(x, y),
              size = size_line,
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

globalVariables(c("x", "y"))

#' roc
#' @description Calculates ROC and AUC
#' @param actual Vector with two levels
#' @param fitted Vector with values between 0 and 1
#' @examples
#' ezplot:::roc(runif(1), sample(c(TRUE, FALSE), 1, replace = TRUE))
#' ezplot:::roc(runif(3), sample(c(TRUE, FALSE), 3, replace = TRUE))
roc = function(fitted, actual) {

  ind = !is.na(actual) & !is.na(fitted)
  actual = actual[ind]
  fitted = fitted[ind]

  count = sum(actual == actual[1])
  if (sum(ind) == 0 | count == 0 | count == length(actual)) {
    return(
      data.frame(
        x = NA,
        y = NA,
        auc = NA
      )
    )
  }

  pred = ROCR::prediction(as.numeric(fitted), actual)
  perf = ROCR::performance(pred, "tpr", "fpr")
  auc = methods::slot(ROCR::performance(pred, "auc"), "y.values")[[1]]

  x = perf@x.values[[1]]
  y = perf@y.values[[1]]

  data.frame(x = x,
             y = y,
             auc = auc)

}

