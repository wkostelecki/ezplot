

#' model_plot
#'
#' @param data A data.frame.
#' @param x A quoted expression.
#' @param actual A quoted expression.
#' @param fitted A quoted expression.
#' @param point_size Numeric. Default is 2.
#' @param size Theme base size. Default is 20.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' y = rnorm(26)
#' df = data.frame(ID = 1:26, Actual = y + rnorm(26), Fitted = y, id = letters)
#' model_plot(df)
#' model_plot(df, "id")
model_plot = function(data,
                      x = "ID",
                      actual = "Actual",
                      fitted = "Fitted",
                      point_size = 2,
                      size = 20){

  gdata = data.frame(ID = eval(parse(text = x), data),
                     Actual = eval(parse(text = actual), data),
                     Fitted = eval(parse(text = fitted), data),
                     stringsAsFactors = FALSE)

  if (is.character(gdata[["ID"]]) | is.factor(gdata[["ID"]])) {
    gdata[["id"]] = gdata[["ID"]]
    gdata[["ID"]] = seq_len(nrow(gdata))
  }

  gdata[["Residual"]] = gdata[["Actual"]] - gdata[["Fitted"]]

  g = ggplot(gdata) +
    suppressWarnings(geom_line(aes(ID, Actual,
                                   color = 'Actual  ',
                                   linetype = 'Actual  ',
                                   shape = 'Actual  ',
                                   size = 'Actual  '),
                               na.rm = TRUE)) +
    suppressWarnings(geom_point(aes(ID, Fitted,
                                    color = 'Fitted  ',
                                    linetype = 'Fitted  ',
                                    shape = 'Fitted  ',
                                    size = 'Fitted  '),
                                na.rm = TRUE)) +
    suppressWarnings(geom_segment(aes(ID,
                                      Residual + min(c(Actual, Fitted), na.rm = TRUE) -
                                        max(Residual, na.rm = TRUE),
                                      xend = ID,
                                      yend = min(c(Actual, Fitted), na.rm = TRUE) -
                                        max(Residual, na.rm = TRUE),
                                      color = 'Residual  ',
                                      linetype = 'Residual  ',
                                      shape = 'Residual  ',
                                      size = 'Residual  '),
                                  na.rm = TRUE)) +
    scale_colour_manual(NULL,
                        labels = c('Actual  ', 'Fitted  ', 'Residual  '),
                        values = c('dodgerblue4',
                                   'tomato2',
                                   'mediumorchid4')) +
    scale_linetype_manual(NULL,
                          labels = c('Actual  ', 'Fitted  ', 'Residual  '),
                          values = c(1, 0, 1)) +
    scale_shape_manual(NULL,
                       labels = c('Actual  ', 'Fitted  ', 'Residual  '),
                       values = c(NA, 16, 0)) +
    scale_size_manual(NULL,
                      labels = c('Actual  ', 'Fitted  ', 'Residual  '),
                      values = point_size ^ c(1, 0.5, 1) * c(0.5, 2, 0.5)) +
    scale_y_continuous(labels = ezplot::ez_labels) +
    ezplot::theme_ez(size) +
    theme(legend.position = 'top',
          panel.grid.major.x = element_line(colour = "grey85",
                                            size = if (size > 16) 0.8 else 0.2)) +
    guides(shape = guide_legend(override.aes=list(shape = c(NA, 16, NA)))) +
    ylab(NULL) +
    xlab(NULL)

  if (!is.null(gdata[["id"]])) {
    g = g +
      scale_x_continuous(breaks = gdata[["ID"]],
                         labels = gdata[["id"]]) +
      theme(axis.text.x = element_text(angle = 90,
                                       vjust = 0.38,
                                       hjust = 1))
  }

  g

}
