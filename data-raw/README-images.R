
# packages ----------------------------------------------------------------


library(ezplot)
library(ggplot2)
library(dplyr)
library(tsibbledata)
library(lubridate)


# data overview -----------------------------------------------------------

lapply(objects("package:tsibbledata"),
       function(x) head(eval(parse(text = x)))) %>%
  setNames(objects("package:tsibbledata"))


# line_plot 1 -------------------------------------------------------------

line_plot(ansett, x = "Week", y = "Passengers") %>%
  ez_png("man/figures/README-line_plot_1.png",
         height = 200,
         width = 350,
         resx = 1.5,
         # check = FALSE,
         dir.create = TRUE)


# line_plot 2 -------------------------------------------------------------

line_plot(ansett, x = "Week",
          y = c("Weekly Passengers" = "Passengers"),
          group = "substr(Airports, 5, 7)",
          facet_x = "substr(Airports, 1, 3)", facet_y = "Class",
          facet_scales = "free_y") %>%
  ez_png("man/figures/README-line_plot_2.png",
         height = 350,
         width = 750,
         resx = 1.5)

# area_plot ---------------------------------------------------------------



area_plot(ansett, x = "Week",
          y = c("Weekly Passengers" = "Passengers"), "Class") %>%
  ez_png("man/figures/README-area_plot.png",
         height = 300,
         width = 500,
         resx = 1.5,
         dir.create = TRUE)


# bar_plot ----------------------------------------------------------------

bar_plot(ansett, x = "lubridate::year(Week)",
         y = c("Yearly Passengers" = "Passengers"), "Class") %>%
  ez_png("man/figures/README-bar_plot.png",
         height = 300,
         width = 500,
         resx = 1.5,
         dir.create = TRUE)


# tile_plot ---------------------------------------------------------------

nyc_bikes %>%
  mutate(duration = as.numeric(stop_time - start_time)) %>%
  filter(between(duration, 0, 16)) %>%
  tile_plot(c("Trip Start (Hour of Day)" = "lubridate::hour(start_time) + 0.5"),
            c("Ride Duration (min)" = "duration - duration %% 2 + 1")) %>%
  ez_png("man/figures/README-tile_plot.png",
         height = 160,
         width = 400,
         resx = 1.5,
         dir.create = TRUE)

# waterfall ---------------------------------------------------------------

waterfall_plot(aus_retail,
               "lubridate::year(Month)",
               "Turnover",
               "sub(' Territory', '\nTerritory', State)",
               rotate_xlabel = TRUE) %>%
  ez_png("man/figures/README-waterfall_plot.png",
         height = 300,
         width = 450,
         resx = 1.5,
         dir.create = TRUE)

# side_plot ---------------------------------------------------------------

side_plot(PBS, "paste(Concession, Type, sep = ' - ')",
          c("Scripts", "Cost", "Average Cost" = "~ Cost / Scripts")) %>%
  ez_png(height = 150,
         width = 600,
         resx = 1.5,
         # check = FALSE,
         "man/figures/README-side_plot.png",
         dir.create = TRUE)

# secondary_plot ----------------------------------------------------------


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




line_plot(gafa_stock, "Date", c("Closing Stock Price" = "Close"),
          yoy = TRUE, facet_y = "Symbol",
          facet_scales = "free_y",
          labels = function(x) ez_labels(x, prepend = "$")) %>%
  ez_png("man/figures/README-line_plot_3.png",
         width = 500, height = 300)


