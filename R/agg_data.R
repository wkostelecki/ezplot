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
#' library(tsibbledata)
#' agg_data(ansett, c("Passengers", count = "1"))
#' agg_data(ansett["Class"])
#' agg_data(ansett[c("Class", "Passengers")])
#' agg_data(ansett, "Passengers", "Class")
#' agg_data(ansett, "Passengers", c("Class", "Airports"))
#' agg_data(ansett, c(x = "Airports", y = "Passengers"), c(x = "Airports"))
#' agg_data(ansett, c(x = "Class", y = "1", group = "Airports"), c(x = "Class", group = "Airports"))
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
  x = data %>%
    as.data.frame %>%
    transmute(!!!lapply(first_expr,
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
    # summarize_all(agg_fun) %>%
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

