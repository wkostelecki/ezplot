
#' tile_plot
#' @inheritParams area_plot
#' @export
#' @examples
#' tile_plot(mtcars, "factor(cyl)", "factor(am)", "mpg")
#' tile_plot(ez_data(), "year", "char", "value", "fct", "num", reorder = NULL)
tile_plot = function(data,
                     x,
                     y,
                     z,
                     facet_x = NULL,
                     facet_y = NULL,
                     size = 12,
                     ncol = NULL,
                     labels_x = NULL,
                     labels_y = NULL,
                     labels_z = ez_labels,
                     palette = ez_jet,
                     reorder = c('facet_x', 'facet_y', "x", "y")){


  cols = c(x = unname(x),
           y = unname(y),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y),
           z = unname(z))

  gdata = agg_data(data,
                   cols,
                   group_by = cols[intersect(names(cols),
                                             c("x", "y", "facet_x", "facet_y"))])

  gdata = gdata %>%
    reorder_levels(cols = reorder,
                   y = 'z')

  if (!('y' %in% names(gdata))){
    gdata[["y"]] = ""
  }

  g = ggplot(gdata) +
    geom_tile(aes(x, y, fill = z)) +
    scale_fill_gradientn(names(z),
                         colours = palette(100),
                         labels = labels_z)

  g = quick_facet(g, ncol)

  g = g +
    theme_ez(size) +
    xlab(names(x)) +
    ylab(names(y)) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = "right")

  g = g + coord_cartesian(expand = FALSE)

  g

}
