#' side_plot
#'
#' @param data A data.frame.
#' @param x A named character.
#' @param y A named character.
#' @param ylabels label formatting function
#' @param size theme size for \code{use_theme()}. Default is 20.
#' @param palette Colour function.
#'
#' @export
#' @examples
#' side_plot(mtcars, "gear", "1")
#' side_plot(mtcars, "cyl", "hp <= 120")
#' side_plot(mtcars, "cyl", c(count = "ifelse(cyl == 4, 1, -1)", "hp <= 120"))
side_plot = function(data,
                     x,
                     y,
                     ylabels = ez_labels,
                     size = 20,
                     palette = ez_col,
                     signif = 3){
  # browser()
  y = nameifnot(y)
  y_names = names(y)
  cols = c(x = unname(x),
           setNames(y, paste0("y", seq_along(y))))

  gdata = agg_data(data,
                   cols,
                   group_by = cols["x"])

  gdata[["x"]] = factor(gdata[["x"]])

  gdata = gdata %>%
    mutate(x = fct_reorder(x, y1, function(x) sum(x, na.rm = TRUE)))

  gdata = gdata %>%
    tidyr::gather(facet_x, y, -x) %>%
    mutate(facet_x = y_names[as.numeric(forcats::fct_inorder(facet_x))])

  gdata = gdata %>%
    group_by(facet_x) %>%
    mutate(label_offset = 0.02 * diff(range(c(y, 0), na.rm = TRUE)) * ifelse(y >= 0, 1, -1)) %>%
    ungroup

  expand = c(
    if (min(gdata[["y"]], na.rm = TRUE) < 0) 0.2 else 0,
    0,
    if (max(gdata[["y"]], na.rm = TRUE) > 0) 0.2 else 0,
    0
  )

  g = ggplot(gdata) +
    geom_col(aes(x, y),
             fill = palette(1)) +
    geom_text(aes(x, y + label_offset,
                  label = ez_labels(y, signif = signif),
                  hjust = ifelse(y >= 0, 0, 1)),
              vjust = 0.5,
              size = size / 3.5,
              colour = "grey30") +
    geom_text(aes(x, 1.2 * y, label = "")) +
    scale_y_continuous(labels = ez_labels) +
    coord_flip(expand = FALSE) +
    theme_ez(size) +
    xlab(NULL) +
    ylab(NULL) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "grey85",
                                            size = if (size > 16) 0.8 else 0.2),
          axis.line.y = element_line(color = "grey85",
                                     size = if (size > 16) 0.8 else 0.2),
          strip.placement = "outside")
  quick_facet(g, strip.position = "bottom", scales = "free_x")

}
