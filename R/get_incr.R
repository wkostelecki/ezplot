
#' get_incr
#' @description returns the minimun increment between sorted unique values of a
#'   vector
#' @param x A numeric or date vector
get_incr = function(x){

  if (lubridate::is.POSIXct(x)) {
    y = sort(unique(x))
    y = as.numeric(min(difftime(y,
                                dplyr::lag(y),
                                units = "seconds"),
                       na.rm = TRUE))
  } else {
    y = as.numeric(min(diff(sort(unique(x)))))
  }

  y

}
