



reorder_levels = function(data,
                          cols = c("group", "facet_x", "facet_x"),
                          y = "y"){

  reorder = forcats::fct_reorder
  for (this_col in cols){
    if (!is.null(data[[this_col]])){
      data[[this_col]] = reorder(data[[this_col]],
                                 data[[y]],
                                 function(x) sum(x, na.rm = TRUE),
                                 .desc = TRUE)
    }
  }

  data

}
