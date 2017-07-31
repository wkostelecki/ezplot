#' Aggregates data
#'
#' @param data A data.frame.
#' @param cols Named character vector.
#'
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

  x = bind_cols(lapply(nameifnot(cols),
                       function(x) eval(parse(text = x),
                                        envir = data)))

  x %>%
    group_by(!!!syms(group_by)) %>%
    summarize_all(agg_fun) %>%
    ungroup

}

