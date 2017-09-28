



get_incr = function(x){

  y = as.numeric(unique(diff(sort(unique(x))))[1])

  if (lubridate::is.POSIXct(x)){
    y = y * 86400
  }

  y

}
