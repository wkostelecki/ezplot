

#' @import rlang dplyr
#'
#'


ez_agg = function(data, groups, dots) {

  data %>%
    transmute(!!!groups,
              !!!dots) %>%
    group_by(!!!rlang::syms(names(groups))) %>%
    summarize_all(function(x) sum(x, na.rm = TRUE)) %>%
    ungroup %>%
    rename()

}


agg_table = function(data, x, ...,
             group = NULL,
             facet_x = NULL,
             facet_y = NULL) {

  x = enquo(x)
  dots = enquos(...)
  group = enquo(group)
  facet_x = enquo(facet_x)
  facet_y = enquo(facet_y)

  groups = list(x = x,
                group = group,
                facet_x = facet_x,
                facet_y = facet_y)

  ind = vapply(groups, function(x) quo_text(x) != "NULL", FALSE)
  groups = groups[ind]

  ez_agg(data, groups, dots)

}

