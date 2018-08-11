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
#' df = ez_data()
#' agg_data(df, c(Units = "units", Value = "value"))
#' agg_data(df["fct"])
#' agg_data(df[c("fct", "value")])
#' agg_data(df, "value", "fct")
#' agg_data(df, "value", c("fct", "year"))
#' agg_data(df, c(x = "year", y = "value"), c(x = "year"))
agg_data = function(data,
                    cols = names(data),
                    group_by = NULL,
                    agg_fun = function(x) sum(x, na.rm = TRUE),
                    group_by2 = NULL){

  # browser()

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

  if (is.null(group_by)) {
    group_by = not_numeric(x)
    ind = group_by %in% COLS[[1]]
    pos = match(group_by[ind], COLS[[1]])
    names(group_by)[ind] = names(COLS[[1]])[pos]
  }
  group_by = nameifnot(group_by)



  if (length(group_by) == 0) {
    group_by = c(".group." = "1")
  } else {
    group_by = setNames(paste0("`", group_by, "`"),
                        names(group_by))
  }

  names(group_by) = paste0(".agg_group.", names(group_by), ".agg_group.")

  x = x %>%
    mutate_(.dots = group_by[!(names(group_by) %in% names(x))])

  x = x %>%
    # mutate(!!!syms(COLS[[1]][!(names(COLS[[1]]) %in% names(x))])) %>%

    # select(!!!syms(c(names(group_by),
    #                  setdiff(names(x),
    #                          setdiff(unname(group_by),
    #                                  unlist(unname(COLS[[3]]))))))) %>%
    group_by(!!!syms(names(group_by))) %>%
    summarize_if(function(x) is.numeric(x) | is.logical(x), agg_fun) %>%
    as.data.frame(check.names = FALSE)

  if (length(COLS[[3]]) > 0) {

    group_by2 = if (length(group_by2) == 0) {
      vector("character", 0)
    } else {
      paste0(".agg_group.", names(group_by2), ".agg_group.")
    }

    x = x %>%
      group_by(!!!syms(group_by2)) %>%
      mutate_(.dots = COLS[[4]]) %>%
      as.data.frame

  }

  # names(x) = gsub("\\.agg_group\\.", "", names(x))

  group_by_ = gsub("`", "", group_by)
  names(group_by_) = gsub("\\.agg_group\\.", "", names(group_by_))

  # m = match(names(x), c(COLS[[1]], names(group_by)))
  # m = match(names(x), COLS[[1]])
  take = COLS[[1]]
  # ind = take %in% group_by_ & names(take) %in% names(group_by_)

  ind = which(match(take, group_by_) == match(names(take), names(group_by_)))


  pos = which(take[ind] %in% group_by_ & names(take)[ind] %in% names(group_by_))

  take[ind] = names(group_by)[pos]
  # take[ind] = names(group_by)[match(take, group_by_)[pos]]

  # take = take[]
  m = match(take, names(x))

  # x = x[, !is.na(m), drop = FALSE]

  x = x[, m, drop = FALSE]
  names(x) = names(COLS[[1]])

  x

}

