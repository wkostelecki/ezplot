
#' get_incr
#' @description returns the minimum increment between sorted unique values of a
#'   vector
#' @param x A numeric or date vector
get_incr = function(x) {

  if (lubridate::is.POSIXct(x)) {
    y = sort(unique(x))
    y = as.numeric(min(difftime(y,
                                dplyr::lag(y),
                                units = "secs"),
                       na.rm = TRUE))
  } else if (is.factor(x) || is.character(x)) {
    y = 1
  } else {
    y = as.numeric(min(diff(sort(unique(x)))))
  }

  y

}
