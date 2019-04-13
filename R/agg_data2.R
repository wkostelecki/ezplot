#' Aggregates data
#' @param data A data frame
#' @param cols Column names to retain
#' @param group_by Column names to use in grouping. All others are aggregated
#' @param agg_fun function to use for aggregation
#' @param group_by2 Column names to use in second grouping step (after
#'   aggregation)
#' @examples
#' agg_data2(mtcars, "cyl")
#' agg_data2(mtcars, c("factor(cyl)", "cyl"))
#' agg_data2(mtcars, c(x = "factor(cyl)", y = "cyl"))
#' agg_data2(mtcars, c(x = "cyl", y = "cyl"), 1)
#' @export
agg_data2 = function(data,
                    cols = names(data),
                    group_by = NULL,
                    agg_fun = function(x) sum(x, na.rm = TRUE),
                    group_by2 = NULL){

  COLS = unpack_cols(cols)

  first_expr = nameifnot(unique(unname(c(COLS[[2]],
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
    group_by = nameifnot(group_by)
  } else {
    group_by = cols[group_by]
  }

  if (length(group_by) == 0) {
    group_by = c(".group." = "1")
  } else {
    group_by = stats::setNames(paste0("`", group_by, "`"),
                               names(group_by))
  }

  names(group_by) = paste0(".agg_group.", names(group_by), ".agg_group.")

  x = x %>%
    mutate_(.dots = group_by[!(names(group_by) %in% names(x))])

  x = x %>%
    group_by(!!!syms(names(group_by))) %>%
    summarize_if(function(x) is.numeric(x) | is.logical(x), agg_fun) %>%
    as.data.frame(check.names = FALSE)

  if (length(COLS[[3]]) > 0) {
    group_by2 = if (is.null(group_by2)) vector("character", 0) else paste0(".agg_group.", names(group_by2), ".agg_group.")

    x = x %>%
      group_by(!!!syms(group_by2)) %>%
      mutate_(.dots = COLS[[4]]) %>%
      as.data.frame

  }

  group_by_ = gsub("`", "", group_by)
  names(group_by_) = gsub("\\.agg_group\\.", "", names(group_by_))
  take = COLS[[1]]

  ind = which(match(take, group_by_) == match(names(take), names(group_by_)))

  pos = which(take[ind] %in% group_by_ & names(take)[ind] %in% names(group_by_))

  take[ind] = names(group_by)[pos]
  m = match(take, names(x))

  x = x[, m, drop = FALSE]
  names(x) = names(COLS[[1]])

  x

}

