#' side_plot
#' @inheritParams bar_plot
#' @param signif Number of significant digits.
#' @export
#' @examples
#' side_plot(mtcars, "gear", "1")
#' side_plot(mtcars, "cyl", c("Cars with <120 HP" = "hp < 120"))
#' side_plot(mtcars, "cyl", c(count = "ifelse(cyl == 4, 1, -1)", "hp <= 120"))
#' side_plot(mtcars, "cyl", c("hp <= 120", "~ - wt / cyl"))
#' side_plot(mtcars, "cyl", c("1", "-1"))
side_plot = function(data,
                     x,
                     y,
                     labels_y = ez_labels,
                     size = 12,
                     palette = ez_col,
                     signif = 3,
                     reorder = TRUE,
                     y_rescale = 1.25){

  y = nameifnot(y)
  y_names = names(y)
  cols = c(x = unname(x),
           setNames(y, paste0("y", seq_along(y))))

  gdata = agg_data(data,
                   cols,
                   group_by = cols["x"])

  gdata[["x"]] = forcats::fct_rev(factor(gdata[["x"]]))

  if (reorder) {
    gdata = gdata %>%
      mutate(x = forcats::fct_reorder(x, y1, function(x) sum(x, na.rm = TRUE)))
  }

  gdata = gdata %>%
    tidyr::gather(facet_x, y, -x) %>%
    mutate(facet_x = y_names[as.numeric(forcats::fct_inorder(facet_x))],
           facet_x = forcats::fct_inorder(facet_x))

  gdata = gdata %>%
    group_by(facet_x) %>%
    mutate(y_offset = diff(range(c(y, 0), na.rm = TRUE)) * ifelse(y >= 0, 1, -1),
           sides = any(y >= 0) + any(y < 0)) %>%
    ungroup

  g = ggplot(gdata) +
    geom_col(aes(x, y),
             fill = palette(1)) +
    geom_text(aes(x, y + (y_rescale - 1) / 10 * y_offset,
                  label = labels_y(signif(y, signif)),
                  hjust = ifelse(y >= 0, 0, 1)),
              vjust = 0.5,
              size = size / 4,
              colour = "grey30") +
    geom_text(aes(x,
                  ifelse(sides < 2,
                         (y_rescale - 1) * y_offset + y,
                         (y_rescale - 1) / (2 - y_rescale) * y_offset + y),
                  label = "")) +
    scale_y_continuous(labels = labels_y,
                       expand = c(0, 0)) +
    coord_flip() +
    theme_ez(size) +
    xlab(NULL) +
    ylab(NULL) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "grey85",
                                            size = if (size > 16) 0.8 else 0.2),
          axis.line.y = element_line(color = "grey85",
                                     size = if (size > 16) 0.8 else 0.2),
          strip.placement = "outside")
  quick_facet(g, strip.position = "bottom", scales = "free_x", nrow = 1) +
    theme(panel.spacing.x = grid::unit(1.5, "lines"))

}
