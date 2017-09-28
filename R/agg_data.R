#' Aggregates data
#'
#' @param data A data.frame.
#' @param cols Named character vector.
#' @param group_by A vector of column names.
#' @param agg_fun Function to use for aggregating.
#' @return An aggregated data.frame.
#' @importFrom rlang syms
#' @export
#' @examples
#' agg_data(fruit, c(Product = "Product", y = "Units"))
#' agg_data(fruit[, c("Product", "Units")])
#' agg_data(fruit[, "Product"])
agg_data = function(data,
                    cols = names(data),
                    group_by = not_numeric(x),
                    agg_fun = function(x) sum(x, na.rm = TRUE)){

  COLS = unpack_cols(cols)

  x = bind_cols(lapply(nameifnot(unique(unname(c(COLS[[2]],
                                                 unlist(COLS[[3]]))))),
                       function(x) eval(parse(text = x),
                                        envir = data)))

  x = x %>%
    group_by(!!!syms(group_by)) %>%
    summarize_all(agg_fun) %>%
    as.data.frame

  m = match(names(x), COLS[[1]])

  x = x[, !is.na(m), drop = FALSE]
  names(x) = names(COLS[[1]])[m[!is.na(m)]]
  x

}

