library(ezplot)
library(ggplot2)
library(dplyr)
library(tsibbledata)


# line_plot 1 -------------------------------------------------------------

line_plot(ansett, x = "Week", y = "Passengers") %>%
        ez_png(height = 200,
               width = 350,
               resx = 1.5,
               # check = FALSE,
               "man/figures/README-line_plot_1.png",
               dir.create = TRUE)


# line_plot 2 -------------------------------------------------------------

line_plot(ansett, x = "Week",
          y = c("Yearly Passengers" = "Passengers"),
          group = "substr(Airports, 5, 7)",
          facet_x = "substr(Airports, 1, 3)", facet_y = "Class",
          facet_scales = "free_y") %>%
        ez_png("man/figures/README-line_plot_2.png",
               height = 350,
               width = 750,
               resx = 1.5)



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

ez_png(secondary_plot(pelt, "Year",
                      c("Hare Population" = "Hare"), c("Lynx Population" = "Lynx"),
                      size = 10,
                      ylim1 = c(0, 160e3),
                      ylim2 = c(0, 80e3)),
       height = 200,
       width = 350,
       resx = 1.5,
       "man/figures/README-secondary_plot.png",
       dir.create = TRUE)

