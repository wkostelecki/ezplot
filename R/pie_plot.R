#' @title pie_plot
#' @name pie_plot
#' @description Creates pie charts.
#' @inheritParams bar_plot
#' @param round Option for rounding label.
#' @param signif Option for retaining significant figures in label.
#' @param label_x Position of label from centre of pie.  0 is the centre of the
#'   pie and 1 is the outer edge.
#' @return ggplot object
#' @export
#' @importFrom forcats fct_reorder
#' @examples
#' \donttest{
#' library(tsibbledata)
#' pie_plot(ansett, "Class", "Passengers")
#' pie_plot(ansett, "Class", "Passengers", reorder = NULL, label_x = 0.5)
#' pie_plot(ansett, "Class", "Passengers", "Airports", reorder = NULL, label_x = 0.5)
#' }
pie_plot = function (data,
                     x,
                     y = "1",
                     facet_x = NULL,
                     facet_y = NULL,
                     labels_y = function(x) ez_labels(x * 100,
                                                     append = "%",
                                                     round = round,
                                                     signif = signif),
                     size = 11,
                     label_cutoff = 0.04,
                     round = Inf,
                     signif = 3,
                     palette = ez_col,
                     reorder = c("x", "facet_x", "facet_y"),
                     label_x = 0.8) {

  stopifnot(label_x >= 0 & label_x <=1)

  cols = c(x = unname(x),
           y = unname(y),
           facet_x = unname(facet_x),
           facet_y = unname(facet_y))

  gdata = agg_data(data,
                   cols,
                   group_by = cols[intersect(names(cols),
                                             c("x", "facet_x", "facet_y"))])

  gdata[["x"]] = factor(gdata[["x"]])

  gdata = reorder_levels(gdata, reorder)

  gdata = gdata %>%
    mutate(x = forcats::fct_rev(x)) %>%
    arrange(desc(x)) %>%
    group_by(!!!syms(intersect(names(cols), c("facet_x", "facet_y")))) %>%
    mutate(share = y / sum(y, na.rm = TRUE),
           share_label = ifelse(share > label_cutoff,
                                labels_y(share),
                                ""),
           share_pos = cumsum(share) - share / 2) %>%
    ungroup

  fill_col = rev(palette(length(levels(gdata[["x"]]))))

  g = ggplot(gdata) +
    geom_col(aes(x = factor(1),
                 y = share,
                 fill = x),
             width = 1) +
    scale_fill_manual(NULL,
                      values = fill_col,
                      breaks = rev(levels(gdata[["x"]])),
                      labels = function(x) paste0(x, "   ")) +
    geom_text(aes(label_x + 0.5,
                  share_pos,
                  label = share_label,
                  colour = x),
              size = size / 4) +
    scale_colour_manual(values = text_contrast(fill_col),
                        guide = "none") +
    coord_polar(theta = "y") +
    theme_ez(size) +
    theme(axis.line.x = element_blank()) +
    scale_y_continuous(breaks = NULL) +
    scale_x_discrete(breaks = NULL) +
    ylab(NULL) +
    xlab(NULL)

  quick_facet(g)

}

globalVariables(c("share", "share_pos", "share_label"))
