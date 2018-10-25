#' bar_plot
#' @inheritParams area_plot
#' @param width Width of bar.
#' @param y_rescale Rescaling factor for y-axis limits
#' @param label_cutoff Cutoff size (proportion of limit range) for excluding labels
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' df = ez_data()
#' bar_plot(df, "year", "units")
#' bar_plot(df, "year", "units", "fct", use_theme = ggplot2::theme_bw)
#' bar_plot(df, "year", "units", "fct", reorder = NULL)
#' bar_plot(df, "year", "units", "fct", reorder = NULL, position = "fill")
#' bar_plot(df, "year", c(Units = "units"))
#' bar_plot(df, "year", c(Units = "units"), "fct", "char")
#' bar_plot(df, "year", "units", "fct", "char", "num", position = "fill")
#' bar_plot(df, "year", "ifelse(fct == 'X', units, -units)", "fct")
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
                    labels_y = if (position == "fill") {
                      function(x) ez_labels(100 * x, append = "%")
                    } else {
                      ez_labels
                    },
                    y_rescale = 1,
                    label_cutoff = 0.12,
                    use_theme = theme_ez,
                    position = "stack",
                    facet_scales = "fixed") {

  y = nameifnot(y)

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
    arrange(!!!syms(c(group_vars, "sign"))) %>%
    group_by(!!!syms(setdiff(c(group_vars, "sign"), "group"))) %>%
    mutate(ylabel_pos = rev(cumsum(rev(y))) - y / 2,
           ylabel_text = ifelse(abs(y) > label_cutoff * max(y_range),
                                labels_y(signif(y, 3)),
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
                size = size / 4) +
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
                size = size / 4)
  }

  g = quick_facet(g, scales = facet_scales)

  expand = c(0.1 * any(gdata[["y"]] < 0), 0,
             0.1 * any(gdata[["y"]] >= 0), 0)

  g +
    xlab(names(x)) +
    ylab(names(y)) +
    scale_y_continuous(labels = labels_y,
                       expand = expand) +
    ylab(names(y)) +
    use_theme(size)

}




