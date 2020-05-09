#' variable_plot
#' @description Plots variables (multiple "y" values) broken out as vertical
#'   facets.
#' @inheritParams line_plot
#' @inheritParams bar_plot
#' @param ylab y label text
#' @param switch Option to switch location of variable (facet) labels. Default
#'   is 'y' (yes) which shows facet strips on left side of panels.
#' @param geom Either "line", "col" or "bar". Default is "line"
#' @export
#' @examples
#' library(tsibbledata)
#' variable_plot(ansett, "Week", "Passengers", facet_x = "Class")
#' variable_plot(ansett, "Week", "Passengers", facet_x = "Class", yoy = TRUE)
#' variable_plot(pelt, "Year", c("Lynx", "Hare"), "round(Year, -1)")
#' variable_plot(hh_budget, "Year", c("Debt", "Expenditure"), "Country")
#' variable_plot(subset(hh_budget, Year > 2013), "Year",
#'               c("Debt\n(% of disposable income)" = "Debt",
#'                 "Expenditure\nGrowth (%)" = "Expenditure",
#'                 "Unemployment (%)" = "Unemployment"),
#'                group = "Country", geom = "bar")
#' variable_plot(subset(hh_budget, Year > 2013), "Year",
#'               c("Debt\n(% of disposable income)" = "Debt",
#'                 "Expenditure\nGrowth (%)" = "Expenditure",
#'                 "Unemployment (%)" = "Unemployment"),
#'                facet_x = "Country", geom = "bar")
#' variable_plot(PBS, "Type", "Scripts", "Concession", switch = "y", geom = "col")
variable_plot = function(data,
                         x, y,
                         group = NULL,
                         facet_x = NULL,
                         size = 14,
                         labels_y = ez_labels,
                         geom = "line",
                         size_line = 1,
                         ylab = NULL,
                         yoy = FALSE,
                         switch = "y",
                         rescale_y = 1) {

  if (yoy & !is.null(group)) {
    stop("Can't use both the \"group\" and \"yoy\" arguments in variable_plot.")
  }

  cols = c(x = unname(x),
           facet_x = unname(facet_x),
           group = unname(group),
           nameifnot(y))

  gdata = agg_data(
    data,
    cols,
    group_by = cols[intersect(names(cols),
                              c("x", "group",
                                "facet_x", "facet_y"))],
    group_by2 = cols[intersect(names(cols),
                               c("group", "facet_x", "facet_y"))]
  )

  gdata = tidyr::gather_(gdata, "variable", "value",
                         setdiff(names(gdata), c("x", "facet_x", "group")),
                         factor_key = TRUE)

  if (inherits(gdata[["x"]], c("numeric", "integer", "Date", "POSIXt"))) {
    if (inherits(gdata[["x"]], "Date") && length(class(gdata[["x"]])) > 1) {
      gdata[["x"]] = as.Date(gdata[["x"]])
    }
    incr = get_incr(gdata[["x"]])
    gdata = gdata %>%
      group_by(!!!syms(setdiff(names(gdata), c("x", "value")))) %>%
      summarize(x = list(seq(min(x), max(x), by = incr))) %>%
      tidyr::unnest(x) %>%
      left_join(gdata, by = setdiff(names(gdata), c("value")))
  } else {
    unique_vals = unique(gdata[["x"]])
    gdata = gdata %>%
      group_by(!!!syms(setdiff(names(gdata), c("x", "value")))) %>%
      summarize(x = list(unique_vals)) %>%
      tidyr::unnest(x) %>%
      left_join(gdata, by = setdiff(names(gdata), c("value")))
  }

  if (geom == "line") {
    if ("group" %in% names(gdata) | yoy) {

      if (yoy) {
        gdata[["group"]] = factor(lubridate::year(gdata[["x"]]))
        gdata[["x"]] = lubridate::yday(gdata[["x"]])
      }

      g = ggplot(gdata) +
        geom_line(aes(x, value, colour = factor(group)), size = size_line, na.rm = FALSE) +
        scale_colour_manual(NULL,
                            values = ez_col(length(unique(gdata[["group"]]))),
                            labels = function(x) paste0(x, "   "))

      if (yoy) {
        g = g +
          scale_x_continuous(breaks = c(1, 91, 182, 274, 366),
                             labels = c("Jan", "Apr", "Jul", "Oct", "Jan"))
      }

    } else {
      g = ggplot(gdata) +
        geom_line(aes(x, value), size = size_line, colour = ez_col(1), na.rm = FALSE)
    }
  } else if (geom %in% c("bar", "col")) {

    label_rescale = 1
    gdata = gdata %>%
      mutate(value = ifelse(is.finite(value), value, NA_real_),
             ylabel_text = labels_y(signif(value, 3))) %>%
      group_by(variable) %>%
      mutate(y_space = (1.1 * rescale_y - 1) * diff(range(c(0, value), na.rm = TRUE)) * sign(value)) %>%
      ungroup()

    if ("group" %in% names(gdata)) {
      g = ggplot(gdata) +
        geom_col(aes(x, value, fill = group),
                 position = "dodge",
                 orientation = "x",
                 na.rm = TRUE) +
        scale_fill_manual(NULL,
                          values = ez_col(length(unique(gdata[["group"]]))),
                          labels = function(x) paste0(x, "   "))

      g = g +
        geom_text(aes(x, value,
                      vjust = ifelse(value >= 0, -0.2, 1),
                      group = group,
                      label = ylabel_text),
                  position = position_dodge(width = 0.9),
                  size = size / 4 * label_rescale,
                  na.rm = TRUE) +
        geom_text(aes(x, value + y_space),
                  label = "",
                  na.rm = TRUE)

    } else {

      g = ggplot(gdata) +
        geom_col(aes(x, value),
                 fill = ez_col(1),
                 orientation = "x",
                 na.rm = TRUE)

      g = g +
        geom_text(aes(x, value, vjust = ifelse(value >= 0, -0.2, 1.4),
                      label = ylabel_text),
                  size = size / 4 * label_rescale,
                  na.rm = TRUE) +
        geom_text(aes(x, value + y_space),
                  label = "",
                  na.rm = TRUE)

    }


    if (inherits(gdata[["x"]], c("character", "factor"))) {
      xlim = c(0.25, length(unique(gdata[["x"]])) + 0.75)
    } else {
      xlim = range(gdata[["x"]]) + c(-0.75, 0.75)
    }

    g = g + coord_cartesian(expand = FALSE,
                            xlim = xlim)

  }

  if (switch != "y") {
    switch = NULL
  }

  if ("facet_x" %in% names(gdata)) {
    g = g + facet_grid(variable ~ facet_x, scales = "free_y", switch = switch)
  } else {
    g = g + facet_grid(variable ~ ., scales = "free_y", switch = switch)
  }

  g = g +
    theme_ez(size) +
    xlab(names(x)) +
    ylab(ylab) +
    scale_y_continuous(labels = labels_y) +
    theme(legend.position = "top")

  if (length(switch) == 1 && switch == "y") {
    g = g + theme(strip.text.y = element_text(colour = "black",
                                              face = "bold",
                                              margin = margin(r = size * 0.35,
                                                              l = size * 0.2)),
                  strip.text.x = element_text(colour = "black",
                                              face = "bold",
                                              margin = margin(b = size * 0.35,
                                                              t = size * 0.2)),
                  strip.background = element_rect(fill = NA,
                                                  colour = NA,
                                                  size = NA),
                  strip.placement = "outside")
  }

  g

}

globalVariables(c("variable", "ylabel_text", "y_space"))
