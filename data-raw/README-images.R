library(ezplot)
library(ggplot2)
df = ez_data()

ez_png(line_plot(df, "year2", "value", "fct", size = 10),
       height = 200,
       width = 350,
       resx = 1.5,
       # check = FALSE,
       "man/figures/README-line_plot.png",
       dir.create = TRUE)

ez_png(area_plot(df, "year2", "value", "fct", size = 10),
       height = 150,
       width = 250,
       resx = 1.5,
       # check = FALSE,
       "man/figures/README-area_plot.png",
       dir.create = TRUE)

ez_png(bar_plot(df, x = "year", y = "value", group = "fct", size = 10),
       height = 150,
       width = 250,
       resx = 1.5,
       # check = FALSE,
       "man/figures/README-bar_plot.png",
       dir.create = TRUE)

ez_png(waterfall_plot(df, "year", "units", "fct", size = 10),
       height = 150,
       width = 250,
       resx = 1.5,
       # check = FALSE,
       "man/figures/README-waterfall_plot.png",
       dir.create = TRUE)

ez_png(side_plot(df, "fct", c("units", "value", price = "~ value / units"), size = 10),
       height = 150,
       width = 450,
       resx = 1.5,
       # check = FALSE,
       "man/figures/README-side_plot.png",
       dir.create = TRUE)

ez_png(secondary_plot(mtcars, "row.names(mtcars)",
                      c("Miles Per Gallon" = "mpg"), c("Horse Power" = "hp"),
                      ylim1 = c(0, 35),
                      ylim2 = c(0, 350),
                      size = 10),
       height = 200,
       width = 350,
       resx = 1.5,
       "man/figures/README-secondary_plot.png",
       dir.create = TRUE)

