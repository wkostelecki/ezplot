#' bar_plot
#' @inheritParams area_plot
#' @param width Width of bar.
#' @param rescale_y Rescaling factor for y-axis limits
#' @param label_cutoff Cutoff size (proportion of y data range) for excluding
#'   labels
#' @param label_pos Position of labels. Can be "auto", "inside", "top", "both"
#'   or "none".
#' @param coord_flip logical (default is FALSE). If TRUE, flips the x and y
#'   coordinate using ggplot2::coord_flip()
#' @return A ggplot object.
#' @export
#'
#' @examples
#' library(tsibble)
#' library(tsibbledata)
#' library(lubridate)
#' bar_plot(ansett, "year(Week)", "Passengers", size = 20)
#' bar_plot(ansett, "year(Week)", "Passengers", "Class")
#' bar_plot(ansett, "Airports", c("Share of Passengers" = "Passengers"), "Class", position = "fill")
#' bar_plot(ansett, "Airports", "Passengers", "Class", reorder = NULL, label_pos = "both")
#' bar_plot(ansett, "Airports",
#'          c(Passengers = "ifelse(Class == 'Economy', Passengers, -Passengers)"),
#'          "Class", label_pos = "both")
#' bar_plot(ansett, "year(Week)", "Passengers", "Class", label_pos = "both", coord_flip = TRUE)
bar_plot = function(data,
                    x,
                    y = "1",
                    group = NULL,
                    facet_x = NULL,
                    facet_y = NULL,
                    size = 11,
                    width = NULL,
                    reorder = c("group", "facet_x", "facet_y"),
                    palette = ez_col,
                    labels_y = if (position == "fill") {
                      function(x) ez_labels(100 * x, append = "%")
                    } else {
                      ez_labels
                    },
                    labels_x = identity,
                    label_pos = c("auto", "inside", "top", "both", "none"),
                    rescale_y = 1.1,
                    label_cutoff = 0.12,
                    use_theme = theme_ez,
                    position = "stack",
                    facet_scales = "fixed",
                    coord_flip = FALSE) {

  label_pos = match.arg(label_pos)

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

  if (label_pos == "auto") {
    if (position == "fill") {
      label_pos = "inside"
    } else if (!exists("group", gdata) || length(unique(gdata[["group"]])) == 1) {
      label_pos = "top"
    } else {
      label_pos = "inside"
    }
  }

  if (position == "fill") {
    gdata = gdata %>%
      group_by(!!!syms(setdiff(group_vars, "group"))) %>%
      mutate(y = y / sum(abs(y))) %>%
      ungroup
  }

  if (facet_scales == "fixed") {
    facet_groups = intersect(names(gdata), c("facet_x", "facet_y"))
  } else {
    facet_groups = NULL
  }

  gdata = gdata %>%
    mutate(sign = ifelse(y >= 0, 1, -1)) %>%
    group_by(!!!syms(c(setdiff(group_vars, "group"), "sign"))) %>%
    mutate(y_height = sum(y)) %>%
    group_by(!!!syms(setdiff(group_vars, c("group", "x", facet_groups)))) %>%
    mutate(y_span = diff(range(y_height, 0)),
           y_range = y_span * (1 + (1 - rescale_y) * n_distinct(sign))) %>%
    ungroup

  gdata = gdata  %>%
    arrange(!!!syms(c(group_vars, "sign"))) %>%
    group_by(!!!syms(setdiff(c(group_vars, "sign"), "group"))) %>%
    mutate(ylabel_pos = rev(cumsum(rev(y))) - y / 2,
           ylabel_text = ifelse(abs(y) > label_cutoff * max(y_span),
                                labels_y(signif(y, 3)),
                                "")) %>%
    ungroup

  if (coord_flip && (is.factor(gdata[["x"]]) | is.character(gdata[["x"]]))) {
    gdata[["x"]] = forcats::fct_rev(factor(gdata[["x"]]))
  }

  g = ggplot(gdata)



  if (exists("group", gdata)) {

    fill_pal = rev(palette(length(unique(gdata[["group"]]))))
    g = g +
      geom_col(aes(x, y,
                   fill = group),
               width = width,
               orientation = "x") +
      scale_fill_manual(NULL,
                        values = fill_pal,
                        labels = function(x) paste0(x, "   "),
                        breaks = rev)

  } else {
    fill_pal = palette(1)
    g = g +
      geom_col(aes(x, y),
               fill = fill_pal,
               orientation = "x",
               width = width)
  }

  if (label_pos %in% c("inside", "both")) {
    if (exists("group", gdata)) {
      g = g +
        geom_text(aes(x, ylabel_pos,
                      label = ylabel_text,
                      colour = group),
                  size = size / 4,
                  vjust = 0.38) +
        scale_colour_manual(NULL,
                            values = text_contrast(fill_pal),
                            guide = "none")
    } else {
      g = g +
        geom_text(aes(x, ylabel_pos,
                      label = ylabel_text),
                  colour = text_contrast(fill_pal),
                  size = size / 4)
    }
  }

  if (label_pos %in% c("top", "both")) {
    top_labels = gdata %>%
      group_by(!!!syms(intersect(names(gdata),
                                 c("x", "facet_x", "facet_y")))) %>%
      summarize(y_range = y_range[1],
                top_y = sum(y[y > 0], na.rm = TRUE),
                y = sum(y, na.rm = TRUE)) %>%
      ungroup %>%
      mutate(top_ylabel_text = labels_y(signif(y, 3)))

    g = g +
      geom_text(data = top_labels,
                aes(x,
                    top_y + y_range / 100,
                    label = top_ylabel_text),
                size = size / 4,
                vjust = if (coord_flip) 0.38 else -0.2,
                hjust = if (coord_flip) 0 else 0.5)
  }

  g = quick_facet(g, scales = facet_scales)

  expand = c((rescale_y - 1) * any(gdata[["y"]] < 0) * (position == "stack"),
             0,
             (rescale_y - 1) * any(gdata[["y"]] >= 0) * (position == "stack"),
             0)

  g = g +
    xlab(names(x)) +
    ylab(names(y)) +
    scale_y_continuous(labels = labels_y,
                       expand = expand) +
    ylab(names(y)) +
    use_theme(size)

  if (coord_flip) {
    g = g +
      coord_flip() +
      theme(axis.text.y = element_text(angle = 0))

    if (is.numeric(gdata[["x"]])) {
      g = g + scale_x_reverse(labels = labels_x)
    }

  }

  g

}

globalVariables(c("y_height", "y_range", "y_span",
                  "ylabel_pos", "ylabel_text",
                  "top_y", "top_ylabel_text"))
