#' waterfall_plot
#' @description function for creating waterfall charts
#' @inheritParams area_plot
#' @param labels Function for formatting labels.
#' @param label_rescale Scaling factor for chart labels (relative to axis
#'   labels).
#' @param y_min Minimum limit of y axis.
#' @param rescale_y Scaling factor to extend y_max.
#' @param n_signif Number of significant figures in labels.
#' @param rotate_xlabel Logical.
#' @param bottom_label Logical.
#' @param ingroup_label Logical. Shows in-group percentage change.
#' @param n_x Number of x levels to show in chart.
#' @export
#' @examples
#' df = ez_data()
#' waterfall_plot(df, "year", "units", "fct")
#' waterfall_plot(df, "year", "units", "char")
#' waterfall_plot(df, "year", "units", "fct", n_x = 3)
#' waterfall_plot(df, "year", "units", "fct",
#'                label_rescale = 0.5,
#'                ingroup_label = TRUE,
#'                bottom_label = FALSE,
#'                n_x = 3,
#'                size = 20,
#'                y_min = 0,
#'                rotate_xlabel = TRUE)
waterfall_plot = function(data,
                          x,
                          y,
                          group,
                          size = 14,
                          labels = ez_labels,
                          label_rescale = 1,
                          y_min = 'auto',
                          rescale_y = 1.1,
                          n_signif = 3,
                          rotate_xlabel = FALSE,
                          bottom_label = TRUE,
                          ingroup_label = FALSE,
                          n_x = 2){

  y = nameifnot(y)

  data = data %>%
    mutate_(..y.. = y) %>%
    # agg_data(cols = c(x = x, group = group, y),
    #          group_by = "x", group_by2 = "x") %>%
    group_by_(x = x,
              group = group) %>%
    summarize(y = sum(..y.., na.rm = TRUE)) %>%
    ungroup %>%
    mutate(x = factor(x),
           group = factor(group)) %>%
    filter(x %in% tail(levels(x), n_x)) %>%
    mutate(x = droplevels(x),
           group = droplevels(group))

  x_levels = levels(data[["x"]])

  if (length(x_levels) < 2){
    stop('x column must have at least two levels for a waterfall chart')
  }

  x_totals = data %>%
    group_by(x) %>%
    summarize(end = sum(y)) %>%
    ungroup %>%
    mutate(col = 0,
           order = 1,
           start = 0,
           percent_change = end / lag(end) - 1)

  group_totals = expand.grid(group = levels(data$group),
                             x = levels(data$x)) %>%
    select(x, group)

  group_totals = data %>%
    group_by(x, group) %>%
    summarize(y = sum(y)) %>%
    ungroup %>%
    left_join(group_totals, ., c("x", "group")) %>%
    mutate(y = ifelse(is.na(y), 0, y)) %>%
    group_by(group) %>%
    mutate(y_next = lead(y),
           change = y_next - y) %>%
    filter(y != 0 | y_next != 0, !is.na(change)) %>%
    arrange(x, desc(change)) %>%
    left_join(x_totals[c("x", "end")], "x") %>%
    group_by(x) %>%
    mutate(end = end + cumsum(change),
           start = end - change,
           order = 2) %>%
    ungroup %>%
    mutate(col = ifelse(change >= 0, 1, -1))

  if (y_min == 'auto'){
    y_min = group_totals %>%
      summarize(y_max = max(start, end),
                y_min = min(start, end)) %>%
      mutate(y_range = y_max - y_min,
             yaxis_min = min(y_min - 0.25 * y_range,
                             0.95 * y_min),
             yaxis_min = pmax(yaxis_min, 0)) %>%
      pull(yaxis_min)
  }

  gdata = bind_rows(x_totals %>%
                      transmute(x, order,
                                start = y_min,
                                end, col,
                                percent_change),
                    group_totals[c("x", "order", "group", "start", "end", "col",
                                   "y", "y_next", "change")]) %>%
    arrange(x, order, desc(change)) %>%
    mutate(label = ifelse(order == 1, as.character(x), as.character(group)),
           x_pos = row_number(),
           p_change = ifelse(y <= 0, NA, change / y),
           col = factor(col))

  gdata = gdata %>%
    mutate(value = ifelse(col == 0, end, change))

  gdata = gdata %>%
    mutate(value_label = ifelse(abs(value) < 1000,
                                labels(signif(round(value, 2), 3)),
                                labels(signif(value, n_signif))))

  if (ingroup_label){
    gdata = gdata %>%
      mutate(value_label2 = ifelse(
        is.na(p_change),
        NA,
        paste0('(',
               ifelse(p_change > 0, '+', ''),
               ez_labels(100 * signif(round(p_change, 4), 3),
                         append = "%"),
               ')')
      ))
  } else (
    gdata[['value_label2']] = NA_character_
  )

  gdata = gdata %>%
    group_by(x) %>%
    mutate(percent_change = case_when(
      x_pos == 1 ~ NA_real_,
      is.na(percent_change) ~ signif(round(value / value[1], 3), 2),
      TRUE ~ signif(round(percent_change, 3), 2)
    )) %>%
    ungroup

  gdata = gdata %>%
    mutate(percent_label = ez_labels(100 * percent_change, append = "%"),
           percent_label = ifelse(percent_change > 0,
                                  paste0('+', percent_label),
                                  percent_label),
           percent_label_0 = ifelse(order == 2 | is.na(percent_label),
                                    "",
                                    percent_label),
           percent_label = ifelse(order == 1, "", percent_label))

  limits = c(y_min,
             max(c(gdata$end, gdata$start), na.rm = TRUE))

  limits[2] = (limits[2] - limits[1]) * rescale_y + limits[1]

  cols = c("0" = 'dodgerblue4',
           "-1" = 'brown',
           "1" = 'forestgreen')

  g = ggplot(gdata, aes(x_pos)) +
    geom_blank() +
    geom_rect(aes(xmin = x_pos - 0.5,
                  xmax = x_pos + 0.5,
                  ymin = start,
                  ymax = end,
                  colour = col,
                  fill = col)) +
    geom_text(aes(x_pos,
                  end,
                  label = value_label,
                  vjust = case_when(col == -1 ~ 1.2,
                                    is.na(value_label2) ~ -0.5,
                                    TRUE ~ -1.9)),
              size = label_rescale * size / 4) +
    geom_text(aes(x_pos,
                  end,
                  label = value_label2,
                  vjust = ifelse(col == -1, 2.6, -0.5)),
              size = label_rescale * size / 4,
              na.rm = TRUE) +
    scale_colour_manual(values = cols,
                        guide = 'none') +
    scale_fill_manual(values = cols,
                      guide = 'none') +
    xlab(NULL) +
    theme_ez(size) +
    scale_y_continuous(labels = labels,
                       limits = limits) +
    scale_x_continuous(breaks = gdata[["x_pos"]],
                       labels = gdata[["label"]]) +
    ylab(names(y))

  if (rotate_xlabel){
    g = g + theme(axis.text.x = element_text(angle = 90,
                                             vjust = 0.5,
                                             hjust = 1))
  }

  if (bottom_label){
    g = g +
      geom_text(aes(x_pos, y_min,
                    label = percent_label),
                vjust = -0.5,
                size = label_rescale * size / 4) +
      geom_text(aes(x_pos, y_min,
                    label = percent_label_0),
                vjust = -0.5,
                colour = 'white',
                size = label_rescale * size / 4)

  }

  g = g +
    coord_cartesian(xlim = c(0, nrow(gdata) + 1),
                    ylim = limits,
                    expand = FALSE)

  g

}

globalVariables(c("end", ".", "..y..", "y_next", "change", "start",
                  "y_max", "yaxis_min",
                  "percent_change", "p_change", "percent_label", "x_pos",
                  "value_label", "value_label2", "percent_label_0"))
