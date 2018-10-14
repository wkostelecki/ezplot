ez_png(line_plot(df, "year", "value", "fct"),
       height = 150,
       width = 250,
       resx = 1.5,
       # check = FALSE,
       "man/figures/README-line_plot.png",
       dir.create = TRUE)


ez_png(area_plot(df, "year2", "value", "fct"),
       height = 150,
       width = 250,
       resx = 1.5,
       # check = FALSE,
       "man/figures/README-area_plot.png",
       dir.create = TRUE)

ez_png(waterfall_plot(ez_data(), "year", "units", "fct", size = 10),
       height = 150,
       width = 250,
       resx = 1.5,
       # check = FALSE,
       "man/figures/README-waterfall_plot.png",
       dir.create = TRUE)

ez_png(side_plot(df, "fct", c("units", "value", price = "~ value / units"), size = 10) +
         ggplot2::theme(panel.spacing.x = grid::unit(1.5, "lines")),
       height = 150,
       width = 450,
       resx = 1.5,
       # check = FALSE,
       "man/figures/README-side_plot.png",
       dir.create = TRUE)
