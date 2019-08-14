
#' Default theme
#'
#' @param base_size base font size
#' @param base_family base fond family
#'
#' @return theme
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars) + geom_point(aes(cyl, mpg)) + theme_ez()
theme_ez = function (base_size = 11, base_family = "") {
  theme_grey(base_size = base_size,
             base_family = base_family) %+replace%
    theme(axis.text = element_text(size = rel(0.8),
                                   colour = "grey30"),
          axis.line.x = element_line(color = "grey85",
                                     size = if (base_size > 16) 0.8 else 0.2),
          axis.ticks = element_line(colour = "grey85",
                                    size = if (base_size > 16) 0.8 else 0.2),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey85",
                                            size = if (base_size > 16) 0.8 else 0.2),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "white",
                                          colour = NA),
          strip.background = element_rect(fill = NA, colour = NA,
                                          size = 0.2),
          strip.placement = "outside",
          strip.text.x = element_text(colour = "black",
                                      face = "bold",
                                      margin = margin(b = base_size * 0.35,
                                                      t = base_size * 0.2)),
          strip.text.y = element_text(colour = "black",
                                      face = "bold",
                                      margin = margin(l = base_size * 0.35,
                                                      r = base_size * 0.2),
                                      angle = -90),
          axis.title.y = element_text(margin = margin(r = base_size / 2),
                                      angle = 90,
                                      colour = "grey30"),
          axis.title.x = element_text(margin = margin(t = base_size / 2),
                                      colour = "grey30"),
          legend.position = "top",
          legend.key = element_blank(),
          panel.spacing.y = grid::unit(1, "lines"))
}
