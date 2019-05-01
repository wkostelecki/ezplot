#' Aggregates data
#' @param data A data.frame.
#' @param cols Named character vector of column names.
#' @param group_by Vector of grouping columns.
#' @param agg_fun Function to use for aggregating.
#' @param group_by2 Vector of grouping column names to use for delayed (post
#'   aggregation) calculation.
#' @param env Environment for extra variables.
#' @return An aggregated data.frame.
#' @importFrom rlang syms
#' @export
#' @examples
#' df = ez_data()
#' agg_data(df, c(Units = "units", Value = "value"))
#' agg_data(df["fct"])
#' agg_data(df[c("fct", "value")])
#' agg_data(df, "value", "fct")
#' agg_data(df, "value", c("fct", "year"))
#' agg_data(df, c(x = "year", y = "value"), c(x = "year"))
#' agg_data2(mtcars, c(x = "cyl", y = "1", group = "cyl"), c(x = "cyl", group = "cyl"))
agg_data = function(data,
                    cols = names(data),
                    group_by = NULL,
                    agg_fun = function(x) sum(x, na.rm = TRUE),
                    group_by2 = NULL,
                    env = parent.frame()){

  COLS = unpack_cols(cols)

  first_expr = nameifnot(unique(unname(c(COLS[["direct"]],
                                         group_by,
                                         unlist(COLS[["indirect_vars"]])))))
  x = transmute(data,
                !!!lapply(first_expr,
                          function(x) rlang::parse_quo(x, env = env)))

  if (is.null(group_by)) {
    group_by = not_numeric(x)
    ind = group_by %in% COLS[["cols"]]
    pos = match(group_by[ind], COLS[["cols"]])
    names(group_by)[ind] = names(COLS[["cols"]])[pos]
  }

  group_by = nameifnot(group_by)

  COLS[["groups"]] = group_by

  if (length(group_by) > 0) {
    names(group_by) = paste0(".agg_group.", names(group_by), ".agg_group.")
  }

  x = x %>%
    group_by(!!!syms(group_by[!(names(group_by) %in% names(x))])) %>%
    summarize_if(function(x) is.numeric(x) | is.logical(x), agg_fun) %>%
    as.data.frame(check.names = FALSE)

  if (length(COLS[["indirect_vars"]]) > 0) {

    group_by2 = if (length(group_by2) == 0) {
      vector("character", 0)
    } else {
      paste0(".agg_group.", names(group_by2), ".agg_group.")
    }

    x = x %>%
      group_by(!!!syms(group_by2)) %>%
      mutate(!!!lapply(COLS[["indirect_expr"]],
                       function(x) rlang::parse_quo(x, env = env))) %>%
      as.data.frame

  }

  group_by_ = gsub("`", "", group_by)
  names(group_by_) = gsub("\\.agg_group\\.", "", names(group_by_))
  take = COLS[["cols"]]
  ind = names(take) %in% names(COLS[["groups"]])
  if (any(ind)) {
    take[ind] = paste0(".agg_group.", names(take[ind]), ".agg_group.")
  }
  m = match(take, names(x))

  x = x[, m, drop = FALSE]
  names(x) = names(COLS[["cols"]])

  x

}

