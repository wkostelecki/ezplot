#' bar_plot
#' @inheritParams area_plot
#' @param width Width of bar.
#' @param rescale_y Rescaling factor for y-axis limits
#' @param label_cutoff Cutoff size (proportion of y data range) for excluding
#'   labels
#' @param position Either \code{"stack"} (default), \code{"fill"} or \code{"dodge"}
#' @param label_pos Position of labels. Can be "auto", "inside", "top", "both"
#'   or "none".
#' @param label_inside Value to display inside bar segments. Optiosn are "y", "absolute", "percent", "share" or "both".
#' @param coord_flip logical (default is FALSE). If TRUE, flips the x and y
#'   coordinate using ggplot2::coord_flip()
#' @param angle angle for geom_text(_repel)
#' @param repel logical (default if FALSE). If TRUE, uses ggrepel for geom_text
#' @return A ggplot object.
#' @export
#'
#' @examples
#' library(tsibble)
#' library(tsibbledata)
#' library(lubridate)
#'
#' bar_plot(ansett, "year(Week)", "Passengers", size = 16)
#' bar_plot(ansett, "year(Week)", "Passengers", size = 16, label_pos = "both")
#' bar_plot(ansett, "year(Week)", "Passengers", size = 16, label_pos = "both", repel = TRUE)
#' bar_plot(ansett, "year(Week)", "Passengers", size = 16, rescale_y = 1.5, label_pos = "both")
#' bar_plot(ansett, "year(Week)", "Passengers", "Class")
#' bar_plot(ansett, "year(Week)", "Passengers", "Class", "Airports")
#' bar_plot(ansett, "year(Week)", "Passengers", "Class", "Airports",
#'          facet_scales = "free_y")
#' bar_plot(ansett, "year(Week)", "Passengers", "Class", "Airports",
#'          facet_scales = "free_y", repel = TRUE)
#' bar_plot(ansett, "year(Week)", "Passengers", "Class", label_pos = "both")
#' bar_plot(ansett, "year(Week)", "Passengers", "Class", label_pos = "both", label_inside = "share")
#' bar_plot(ansett, "year(Week)", "Passengers", "Class", label_pos = "both", label_inside = "both")
#' bar_plot(ansett, "year(Week)", "Passengers", "Class", label_pos = "both", label_inside = "both",
#'          coord_flip = TRUE)
#'
#'\dontrun{
#' bar_plot(ansett, "Airports", c("Share of Passengers" = "Passengers"), "Class", position = "fill")
#' bar_plot(ansett, "Airports", "Passengers", "Class", label_pos = "both")
#' bar_plot(ansett, "Airports", "Passengers", "Class", label_pos = "both", repel = TRUE)
#' bar_plot(ansett, "Airports", "Passengers", "Class", label_pos = "both", repel = TRUE, angle = 90)
#' bar_plot(ansett, "Airports", "Passengers", "Class", label_pos = "both", repel = TRUE, angle = -90)
#'
#' bar_plot(mtcars, "factor(cyl)", "1", "am", position = "dodge")
#' bar_plot(mtcars, "factor(cyl)", "1", "am", position = "dodge", coord_flip = TRUE)
#' bar_plot(mtcars, "factor(cyl)", "1", "am", position = "dodge", coord_flip = TRUE, rescale_y = 2)
#' bar_plot(mtcars, "factor(cyl)", "1", "am", position = "dodge", coord_flip = TRUE, angle = -90)
#' bar_plot(mtcars, "factor(cyl)", "1", "am", position = "dodge", coord_flip = TRUE, angle = 90)
#'
#' bar_plot(ansett, "Airports",
#'          c(Passengers = "ifelse(Class == 'Economy', Passengers, -Passengers)"),
#'          "Class", label_pos = "both")
#'}
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
                    label_inside = c("y", "absolute", "share", "percent", "both"),
                    rescale_y = 1.1,
                    label_cutoff = 0.12,
                    use_theme = theme_ez,
                    position = "stack",
                    facet_scales = "fixed",
                    legend_ncol = NULL,
                    coord_flip = FALSE,
                    angle = 0,
                    repel = FALSE) {

  label_pos = match.arg(label_pos)
  label_inside = match.arg(label_inside)

  y = nameifnot(y)

  cols = c(x = unname(x),
           y = unname(y),
           group = unname(group),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y))

  group_vars = intersect(c("x", "group", "facet_x", "facet_y"), names(cols))

  gdata = agg_data(data,
                   cols,
                   group_by = cols[group_vars]) %>%
    mutate(abs = y)%>%
    group_by(!!!syms(setdiff(group_vars, "group"))) %>%
    mutate(p = coalesce(y / sum(abs(y)), 0)) %>%
    ungroup()

  if (any("group" == names(gdata))) gdata[["group"]] = factor(gdata[["group"]])

  gdata = reorder_levels(gdata, cols = reorder)

  if ((exists("group", gdata) & (position != "dodge")) | (exists("group", gdata) & (position == "dodge") & coord_flip)) {
    gdata[["group"]] = forcats::fct_rev(gdata[["group"]])
  }

  if (label_pos == "auto") {
    if (position == "fill") {
      label_pos = "inside"
    } else if (!exists("group", gdata) || length(unique(gdata[["group"]])) == 1) {
      label_pos = "top"
    } else if (position == "dodge") {
      label_pos = "top"
    } else {
      label_pos = "inside"
    }
  }

  if (position == "fill") gdata = gdata  %>% mutate(y = p)

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
    mutate(y_span = diff(range(y_height, 0, na.rm = TRUE)),
           y_range = y_span * (1 + (rescale_y - 1) * n_distinct(sign))) %>%
    ungroup

  gdata = gdata  %>%
    arrange(!!!syms(c(group_vars, "sign"))) %>%
    group_by(!!!syms(setdiff(c(group_vars, "sign"), "group"))) %>%
    mutate(ylabel_pos = rev(cumsum(rev(y))) - y / 2,
           ylabel_cutoff = label_cutoff * max(y_span)) %>%
    ungroup()

  if (label_inside == "y") {
    gdata[["ylabel_text"]] = labels_y(signif(gdata[["y"]], 3))
  } else if (label_inside == "absolute") {
    gdata[["ylabel_text"]] = ez_labels(gdata[["abs"]], signif = 3)
  } else if (label_inside %in% c("p", "share")) {
    gdata[["ylabel_text"]] = ez_labels(100 * gdata[["p"]], signif = 3, append = "%")
  } else if (label_inside == "both") {
    gdata[["ylabel_text"]] = paste0(ez_labels(gdata[["abs"]], signif = 3),
                                    "\n",
                                    ez_labels(100 * gdata[["p"]], signif = 3, append = "%"))
  }
  gdata[["ylabel_text"]] = ifelse(abs(gdata[["y"]]) > gdata[["ylabel_cutoff"]], gdata[["ylabel_text"]], "")

  if (coord_flip && (is.factor(gdata[["x"]]) | is.character(gdata[["x"]]))) {
    gdata[["x"]] = forcats::fct_rev(factor(gdata[["x"]]))
  }

  g = ggplot(gdata)


  if (position == "dodge" & exists("group", gdata)) {
    fill_pal = palette(length(unique(gdata[["group"]])))
    g = g +
      geom_col(aes(x, y,
                   fill = group),
               width = width,
               position = position_dodge(0.9),
               orientation = "x") +
      scale_fill_manual(NULL,
                        values = if (coord_flip) rev(fill_pal) else fill_pal,
                        labels = function(x) paste0(x, "   "),
                        breaks = if (coord_flip) rev else identity,
                        guide = guide_legend(ncol = legend_ncol))
  } else if (exists("group", gdata)) {

    fill_pal = rev(palette(length(unique(gdata[["group"]]))))
    g = g +
      geom_col(aes(x, y,
                   fill = group),
               width = width,
               orientation = "x") +
      scale_fill_manual(NULL,
                        values = fill_pal,
                        labels = function(x) paste0(x, "   "),
                        breaks = rev,
                        guide = guide_legend(ncol = legend_ncol))

  } else {
    fill_pal = palette(1)
    g = g +
      geom_col(aes(x, y),
               fill = fill_pal,
               orientation = "x",
               width = width)
  }

  if (repel) {
    g_text = function(...) ggrepel::geom_text_repel(...,
                                                    point.size = NA,
                                                    box.padding = 0,
                                                    point.padding = 0,
                                                    ylim = c(-Inf, Inf),
                                                    xlim = c(-Inf, Inf),
                                                    angle = angle,
                                                    size = size * 0.8 / ggplot2::.pt,
                                                    direction = if (coord_flip) "x" else "y",
                                                    position = if(position == "dodge") position_dodge(0.9) else "identity")
  } else {
    g_text = function(...) geom_text(...,
                                     angle = angle,
                                     size = size * 0.8 / ggplot2::.pt,
                                     position = if(position == "dodge") position_dodge(0.9) else "identity")
  }

  if (label_pos != "none") {

    if (label_pos %in% c("inside", "both") && position != "dodge") {
      if (!exists("group", g[["data"]])) g[["data"]][["group"]] = factor("")
      inside_text = g[["data"]] %>%
        mutate(placement = "inside",
               vjust = if (angle == 0) 0.38 else 0.33,
               hjust = 0.5,
               colour = text_contrast(fill_pal[as.numeric(group)])) %>%
        select(-sign, -y_height, -y_span, -y_range, -group)
    } else {inside_text = data.frame()}

    if (label_pos %in% c("top", "both") & position != "fill") {
      top_vjust = case_when(coord_flip & angle > 0 ~ 1,
                            coord_flip & angle < 0 ~ -0.38,
                            coord_flip ~ 0.38,
                            !coord_flip & angle > 0 ~ 0.33,
                            !coord_flip & angle < 0 ~ 0.33,
                            TRUE ~ -0.2)
      top_hjust = case_when(coord_flip  & angle != 0 ~ 0.5,
                            coord_flip ~ 0,
                            !coord_flip & angle > 0 ~ 0,
                            !coord_flip & angle < 0 ~ 1,
                            TRUE ~ 0.5)
      top_text = gdata %>%
        group_by(!!!syms(intersect(names(gdata),
                                   c("x", "facet_x", "facet_y",
                                     if(position == "dodge") "group" else NULL)))) %>%
        summarize(y_range = y_range[1],
                  ylabel_pos = sum(y[y > 0], na.rm = TRUE) + y_range / 200,
                  y = sum(y, na.rm = TRUE)) %>%
        ungroup %>%
        mutate(ylabel_text = labels_y(signif(y, 3)),
               colour = "black",
               placement = "top",
               vjust = top_vjust,
               hjust = top_hjust) %>%
        select(-y_range)
    } else {top_text = data.frame()}

    all_text = bind_rows(top_text, inside_text)
    # print(all_text)

    g = g + g_text(data = all_text,
                   aes(x, ylabel_pos, label = ylabel_text, group = group),
                   vjust = all_text[["vjust"]],
                   hjust = all_text[["hjust"]],
                   colour = all_text[["colour"]])

  }

  g = quick_facet(g, scales = facet_scales)

  expand = c((rescale_y - 1) * any(gdata[["y"]] < 0) * (position %in% c("stack", "dodge")),
             0,
             (rescale_y - 1) * any(gdata[["y"]] >= 0) * (position %in% c("stack", "dodge")),
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
      coord_flip(clip = "off") +
      theme(axis.line.y = element_line(color = "grey85",
                                       linewidth = if (size > 16) 0.8 else 0.2),
            axis.line.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(colour = "grey85",
                                              linewidth = if (size > 16) 0.8 else 0.2))

    if (is.numeric(gdata[["x"]])) {
      g = g + scale_x_reverse(labels = labels_x)
    }

  } else {
    g = g + coord_cartesian(clip = "off")
  }

  g

}

globalVariables(c("y_height", "y_range", "y_span", "p",
                  "ylabel_pos", "ylabel_text", "ylabel_cutoff"))
