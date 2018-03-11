
#' @title Quick facet
#' @description Applies faceting to ggplot objects when g[["data"]] has a
#'   \code{facet_x} or \code{facet_y} column.
#' @param g A ggplot object.
#' @param ... Arguments to pass to \code{facet_grid} or \code{facet_wrap}
quick_facet = function(g, ...){
  if (all(c("facet_x", "facet_y") %in% names(g[["data"]]))){
    g = g + facet_grid(facet_y ~ facet_x, ...)
  } else if ("facet_x" %in% names(g[["data"]])){
    g = g + facet_wrap(~ facet_x, ...)
  } else if ("facet_y" %in% names(g[["data"]])){
    g = g + facet_wrap(~ facet_y, ...)
  }
  g
}


#' no_null
#' @description Converts "NULL" character to NULL.
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
#' @description Returns names of non-numeric columns.
#' @param x A data.frame.
#'
#' @return A character vector.
#'
not_numeric = function(x){
  ind = sapply(x, function(x) !is.numeric(x))
  names(ind)[ind]
}


#' nameifnot
#' @description Names unnamed elements of a character vector.
#' @param x A character vector.
#' @param make.names Logical. Whether to force names of x to be valid variablel
#'   names. Default is FALSE.
#'
#' @return A named vector.
#'
nameifnot = function(x, make.names = FALSE){
  stopifnot(is.character(x))
  if (is.null(names(x))){
    names(x) = x
  } else {
    ind = is.na(names(x)) | names(x) == ""
    if (any(ind)) names(x)[ind] = x[ind]
  }
  if (make.names) {
    names(x) = make.names(names(x), unique = TRUE)
  } else {
    names(x) = make.unique(names(x))
  }
  x
}



#' Unpack cols argument to agg_data
#'
#' @param x cols
#'
#' @return list
#' @importFrom stats as.formula
#' @examples
#' unpack_cols("x")
#' unpack_cols(c(x = "x", y = "x + y", expr = "~ x + y"))
unpack_cols = function(x) {

  stopifnot(is.character(x))
  x = gsub("^ *| *$", "", x)
  x = nameifnot(x)
  with_tilde = grepl("^~", x)

  list(x,
       x[!with_tilde],
       lapply(x[with_tilde], function(x) all.vars(as.formula(x))),
       setNames(sub(" *~ *", "", x[with_tilde]), x[with_tilde]))

}

