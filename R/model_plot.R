

#' model_plot
#' @inheritParams area_plot
#' @param actual A character value. Evaluates to a column.
#' @param fitted A character value. Evaluates to a column.
#' @param res_bins Number of bins in the residual distribution. Default value
#'   (NA) doesn't show the distribution.
#' @param point_size Numeric. Default is 2.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' y = rnorm(26)
#' df = data.frame(ID = 1:26, actual = y + rnorm(26), fitted = y, id = letters)
#' model_plot(df, "ID", "actual", "fitted")
#' model_plot(df, "id", "actual", "fitted")
#' model_plot(df, "ID", "actual", "fitted", res_bins = 10)
#' model_plot(df, "id", "actual", "fitted", res_bins = 10)
model_plot = function(data,
                      x,
                      actual,
                      fitted,
                      facet_x = NULL,
                      point_size = 2,
                      res_bins = NA_real_,
                      size = 14){

  gdata = data.frame(ID = eval(parse(text = x), data),
                     Actual = eval(parse(text = actual), data),
                     Fitted = eval(parse(text = fitted), data),
                     stringsAsFactors = FALSE)

  gdata[["Residual"]] = gdata[["Actual"]] - gdata[["Fitted"]]


  if (!is.null(facet_x)) {
    gdata[["facet_x"]] = eval(parse(text = facet_x), data)
  }

  gdata = gdata %>%
    group_by_at(vars(matches("facet_x"))) %>%
    mutate(min_af = min(Actual, Fitted, na.rm = TRUE),
           max_res = max(Residual, na.rm = TRUE)) %>%
    ungroup

  if (is.character(gdata[["ID"]]) | is.factor(gdata[["ID"]])) {
    gdata[["id"]] = gdata[["ID"]]
    gdata[["ID"]] = seq_len(nrow(gdata))
  }


  g = ggplot(gdata) +
    suppressWarnings(geom_line(
      aes(ID, Actual,
          color = "Actual  ",
          linetype = "Actual  ",
          shape = "Actual  ",
          size = "Actual  "),
      na.rm = TRUE
    )) +
    suppressWarnings(geom_point(
      aes(ID, Fitted,
          color = "Fitted  ",
          linetype = "Fitted  ",
          shape = "Fitted  ",
          size = "Fitted  "),
      na.rm = TRUE
    )) +
    suppressWarnings(geom_segment(
      aes(ID,
          Residual + min_af - max_res,
          xend = ID,
          yend = min_af - max_res,
          color = "Residual  ",
          linetype = "Residual  ",
          shape = "Residual  ",
          size = "Residual  "),
      na.rm = TRUE
    )) +
    scale_colour_manual(NULL,
                        labels = c("Actual  ", "Fitted  ", "Residual  "),
                        values = c("dodgerblue4",
                                   "tomato2",
                                   "mediumorchid4")) +
    scale_linetype_manual(NULL,
                          labels = c("Actual  ", "Fitted  ", "Residual  "),
                          values = c(1, 0, 1)) +
    scale_shape_manual(NULL,
                       labels = c("Actual  ", "Fitted  ", "Residual  "),
                       values = c(NA, 16, 0)) +
    scale_size_manual(NULL,
                      labels = c("Actual  ", "Fitted  ", "Residual  "),
                      values = point_size ^ c(1, 0.5, 1) * c(0.75, 2, 0.75)) +
    scale_y_continuous(labels = ezplot::ez_labels) +
    ezplot::theme_ez(size) +
    theme(
      legend.position = "top",
      panel.grid.major.x = element_line(colour = "grey85",
                                        size = if (size > 16) 0.8 else 0.2)
    ) +
    guides(shape = guide_legend(override.aes=list(shape = c(NA, 16, NA)))) +
    ylab(NULL) +
    xlab(NULL)

  if (!is.null(gdata[["id"]])) {
    g = g +
      scale_x_continuous(breaks = gdata[["ID"]],
                         labels = gdata[["id"]],
                         expand = c(0, 0.6)) +
      theme(axis.text.x = element_text(angle = 90,
                                       vjust = 0.38,
                                       hjust = 1),
            panel.grid.major.x = element_blank())
  }

  if (!is.na(res_bins)) {
    bin_data = gdata %>%
      mutate(bins = cut(Residual + min_af - max_res,
                        breaks = res_bins)) %>%
      group_by_at(vars(matches("facet_x"))) %>%
      mutate(x_start = max(ID)) %>%
      group_by_(.dots = intersect(c("facet_x", "bins"), names(.))) %>%
      summarize(n = n(),
                x_start = x_start[1],
                x_range = diff(range(as.numeric(.$ID)))) %>%
      ungroup %>%
      mutate(xmin = as.numeric(x_start) + 0.02 * x_range,
             xmax = xmin + 0.05 * x_range * n / max(n),
             ymin = as.numeric(gsub("\\(|,.*", "", bins)),
             ymax = as.numeric(gsub(".*,|]", "", bins)))
    g = g +
      geom_rect(data = bin_data,
                aes(xmin = xmin,
                    xmax = xmax,
                    ymin = ymin,
                    ymax = ymax),
                fill = "mediumorchid4",
                colour = "mediumorchid4")
  }

  quick_facet(g, scales = "free_y")

}

globalVariables(c("Actual", "Fitted", "Residual", "ID", "min_af", "max_res",
                  "x_start", "x_range", "xmin", "xmax", "ymin", "ymax", "bins"))
