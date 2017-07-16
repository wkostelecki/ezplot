
#' @title Quick facet
#' @name quick_facet
quick_facet = function(g, ...){
  if (all(c("facet_x", "facet_y") %in% names(g[["data"]]))){
    g = g +
      facet_grid(facet_y ~ facet_x, ...)
  } else if ("facet_x" %in% names(g[["data"]])){
    g = g +
      facet_wrap(~ facet_x, ...)
  } else if ("facet_y" %in% names(g[["data"]])){
    g = g +
      facet_wrap(~ facet_y, ...)
  }
  g
}


#' @title palette
#' @name palette
palette = function(n){
  rgb(runif(n), runif(n), runif(n))
}


