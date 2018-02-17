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
#' library(ukbabynames)
#' agg_data(ukbabynames, c(Sex = "sex", Total = "n"))
#' agg_data(ukbabynames["sex"])
#' agg_data(ukbabynames[c("sex", "n")])
#' agg_data(ukbabynames, "n", "year")
#' agg_data(ukbabynames, "n", c("sex", "year"))
#' agg_data(ukbabynames, c(x = "year", y = "n"), c(x = "year"))
agg_data = function(data,
                    cols = names(data),
                    group_by = NULL,
                    agg_fun = function(x) sum(x, na.rm = TRUE),
                    group_by2 = NULL){

  COLS = unpack_cols(cols)

  first_expr = nameifnot(unique(unname(c(COLS[[2]],
                                         group_by,
                                         unlist(COLS[[3]])))))

  x = as.data.frame(sapply(first_expr,
                           function(x) eval(parse(text = x),
                                            envir = data),
                           simplify = FALSE),
                    stringsAsFactors = FALSE,
                    check.names = FALSE)

  group_by = nameifnot(if (is.null(group_by)) not_numeric(x) else group_by)

  x = x %>%
    mutate(!!!syms(group_by)) %>%
    select(!!!syms(c(names(group_by), setdiff(names(x), unname(group_by))))) %>%
    group_by(!!!syms(names(group_by))) %>%
    summarize_all(agg_fun) %>%
    as.data.frame(check.names = FALSE)

  if (length(COLS[[3]]) > 0) {
    group_by2 = if (is.null(group_by2)) vector("character", 0) else group_by2

    x = x %>%
      group_by(!!!syms(group_by2)) %>%
      mutate_(.dots = COLS[[4]]) %>%
      as.data.frame
  }

  m = match(names(x), c(COLS[[1]], names(group_by)))

  x = x[, !is.na(m), drop = FALSE]
  names(x) = names(c(COLS[[1]], group_by))[m[!is.na(m)]]
  x

}

