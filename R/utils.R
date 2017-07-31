
#' @title Quick facet
#' @param g A ggplot object.
#' @param ... Arguments to pass to \code{facet_grid} or \code{facet_wrap}
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


#' no_null
#' @description converts "NULL" to NULL
#' @param x A character vector.
#'
#' @return y
#' @export
#'
#' @examples
#' no_null(NULL)
#' no_null("NULL")
#' no_null("NOPE")
no_null = function(x){
  if (is.null(x) || (x == "NULL" & length(x) == 1)) return(NULL) else return(x)
}

#' not_numeric
#' @description returns names of non-numeric columns
#' @param x A data.frame.
#'
#' @return A character vector.
#'
not_numeric = function(x){
  ind = sapply(x, function(x) !is.numeric(x))
  names(ind)[ind]
}


#' nameifnot
#' @description if not
#' @param x A character vector.
#'
#' @return A named vector.
#'
nameifnot = function(x){
  stopifnot(is.character(x))
  if (is.null(names(x))){
    names(x) = x
  } else {
    ind = any(names(x) == "")
    if (any(ind)) names(x)[ind] = x[ind]
  }
  names(x) = make.names(names(x), unique = TRUE)
  x
}
