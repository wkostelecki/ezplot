#' bar_plot
#'
#' @param data A data.frame.
#' @param x A named character.
#' @param y A named character.
#' @param group character
#' @param facet_x character
#' @param facet_y character
#' @param size theme size for \code{use_theme()}. Default is 20.
#' @param palette Colour function.
#' @param ylabels label formatting function
#' @param use_theme ggplot theme function
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' bar_plot(mtcars, "carb", "1", size = 12)
#' bar_plot(mtcars, "carb", "1", "cyl", use_theme = ggplot2::theme_bw)
#' bar_plot(mtcars, "carb", "1", "cyl", reorder = NULL)
#' bar_plot(mtcars, "carb", "1", "cyl", reorder = NULL, position = "fill")
#' bar_plot(mtcars, "carb", c(Count = "1"), size = 12)
#' bar_plot(mtcars, "carb", c(Count = "1"), "cyl", "gear")
#' bar_plot(mtcars, "carb", "1", "cyl", "gear", "am", position = "fill")
#' bar_plot(mtcars, "carb", "sample(c(1, -1), 32, replace = TRUE)", "cyl")
bar_plot = function(data,
                    x,
                    y,
                    group = NULL,
                    facet_x = NULL,
                    facet_y = NULL,
                    size = 12,
                    width = NULL,
                    reorder = c("group", "facet_x", "facet_y"),
                    palette = ez_col,
                    ylabels = if (position == "fill") {
                      function(x) ez_labels(100 * x, append = "%")
                    } else {
                      ez_labels
                    },
                    y_rescale = 1,
                    label_cutoff = 0.12,
                    use_theme = theme_ez,
                    position = "stack",
                    facet_scales = "fixed") {

  cols = c(x = unname(x),
           y = unname(y),
           group = unname(group),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y))

  group_vars = intersect(c("x", "group", "facet_x", "facet_y"), names(cols))

  gdata = agg_data(data,
                   cols,
                   group_by = cols[group_vars])

  if (any("group" == names(gdata))) gdata[["group"]] = factor(gdata[["group"]])

  gdata = reorder_levels(gdata, cols = reorder)

  if (any("group" == names(gdata))) {
    gdata[["group"]] = forcats::fct_rev(gdata[["group"]])
  }

  if (position == "fill") {
    gdata = gdata %>%
      group_by(!!!syms(setdiff(group_vars, "group"))) %>%
      mutate(y = y / sum(abs(y))) %>%
      ungroup
  }

  if (facet_scales == "fixed") {
    cutoff_groups = intersect(names(gdata), c("facet_x", "facet_y"))
  } else {
    cutoff_groups = NULL
  }

  gdata = gdata %>%
    mutate(sign = ifelse(y >= 0, 1, -1)) %>%
    group_by(!!!syms(c(setdiff(group_vars, "group"), "sign"))) %>%
    mutate(y_height = sum(y)) %>%
    group_by(!!!syms(setdiff(group_vars, c("group", "x", cutoff_groups)))) %>%
    mutate(y_range = diff(range(y_height, 0)) * (1 + (1 - y_rescale) * n_distinct(sign))) %>%
    ungroup

  gdata = gdata  %>%
    arrange(!!!syms(group_vars)) %>%
    group_by(!!!syms(setdiff(group_vars, "group"))) %>%
    mutate(ylabel_pos = rev(cumsum(rev(y))) - y / 2,
           ylabel_text = ifelse(abs(y) > label_cutoff * max(y_range),
                                ylabels(signif(y, 3)),
                                "")) %>%
    ungroup

  g = ggplot(gdata)

  if ("group" %in% names(gdata)){

    fill_pal = rev(palette(length(unique(gdata[["group"]]))))
    g = g +
      geom_col(aes(x, y,
                   fill = group),
               width = width) +
      scale_fill_manual(NULL,
                        values = fill_pal,
                        labels = function(x) paste0(x, "   "),
                        breaks = rev) +
      geom_text(aes(x, ylabel_pos,
                    label = ylabel_text,
                    colour = group),
                size = size / 3.5) +
      scale_colour_manual(NULL,
                          values = text_contrast(fill_pal),
                          guide = "none")

  } else {
    fill_pal = palette(1)
    g = g +
      geom_col(aes(x, y),
               fill = fill_pal,
               width = width) +
      geom_text(aes(x, ylabel_pos,
                    label = ylabel_text),
                colour = text_contrast(fill_pal),
                size = size / 3.5)
  }

  g = quick_facet(g, scales = facet_scales)

  expand = c(0.1 * any(gdata[["y"]] < 0), 0,
             0.1 * any(gdata[["y"]] >= 0), 0)

  g +
    xlab(names(x)) +
    ylab(names(y)) +
    scale_y_continuous(labels = ylabels,
                       expand = expand) +
    ylab(names(y)) +
    use_theme(size)

}




