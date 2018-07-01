
#' @export

ez_log_breaks = function(limits) {

  breaks = 10 ^ (-10:100)

  used_breaks = breaks[breaks > limits[1] & breaks < limits[2]]

  if (length(used_breaks) <= 2 ) {
    breaks = signif(min(c(breaks[breaks > limits[1]])) * 2 ^ (-1:10), 4)
  }

  breaks

}
