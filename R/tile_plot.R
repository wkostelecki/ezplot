
#' tile_plot
#' @description Creates tile plots.
#' @inheritParams area_plot
#' @param z A named character. Evaluates to a column and is mapped to the fill
#'   colour of the tiles.
#' @param labels_z label formatting function
#' @param zlim argument for \code{scale_fill_grandientn(limits = zlim)}
#' @export
#' @examples
#' \dontrun{
#' library(tsibbledata)
#' library(dplyr)
#' nyc_bikes %>%
#'   mutate(duration = as.numeric(stop_time - start_time)) %>%
#'   filter(between(duration, 0, 16)) %>%
#'   tile_plot(c("Hour of Day" = "lubridate::hour(start_time) + 0.5"),
#'             c("Ride Duration (min)" = "duration - duration %% 2 + 1"))
#' }
tile_plot = function(data,
                     x,
                     y,
                     z = c(Count = "1"),
                     facet_x = NULL,
                     facet_y = NULL,
                     size = 14,
                     facet_ncol = NULL,
                     labels_x = NULL,
                     labels_y = NULL,
                     labels_z = ez_labels,
                     zlim = function(x) c(pmin(0, x[1]), pmax(0, x[2])),
                     palette = ez_jet,
                     reorder = c('facet_x', 'facet_y')){


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
                         labels = labels_z,
                         limits = zlim)

  g = quick_facet(g, ncol = facet_ncol)

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

