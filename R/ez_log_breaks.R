#' ez_log_breaks
#' @description Creates break vector for a log axis
#' @param limits Limits of axis
#' @export
#' @examples
#' ez_log_breaks(limits = c(0.9, 11))
ez_log_breaks = function(limits) {

  breaks = 10 ^ (floor(log10(limits[1])):ceiling(log10(limits[2])))

  used_breaks = breaks[breaks > limits[1] & breaks < limits[2]]

  if (length(used_breaks) <= 2) {
    min_break = breaks[breaks > limits[1]][1]
    breaks = signif(min_break * 2 ^ (-2:ceiling(log2(limits[2]) / log2(limits[1] * 2))), 4)
  }

  breaks

}
