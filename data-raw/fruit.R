library(dplyr)

# Generate Data -----------------------------------------------------------
set.seed(1)

fruit = expand.grid(list(OBS = seq(as.Date("2014-01-05"),
                                   as.Date("2016-12-25"),
                                   by = "week"),
                         Product = c("Apples", "Bananas", "Oranges"),
                         Size = c("Small", "Large"),
                         Store = c("Grocery", "Variety")))

fruit = fruit %>%
  group_by(Product, Store) %>%
  mutate(Units = sample(2:5, 1) + # size
           sin((1:n() - sample(n(), 1)) * 2 * pi / n() * sample(c(6, 12), 1)) + # seasonality
           (1:n()) * 2 * (runif(1) - 0.5) / n(), # trend
         Units = round(10000 * Units),
         Value = Units *
           case_when(Product == "Bananas" ~ 0.5,
                     Product == "Apples" ~ 1,
                     Product == "Oranges" ~ 1.5) *
           case_when(Size == "Small" ~ 0.75,
                     Size == "Large" ~ 1.25) *
           case_when(Store == "Grocery" ~ 0.8,
                     Store == "Variety" ~ 1))



devtools:::use_data(fruit, overwrite = TRUE)

# Chart -------------------------------------------------------------------

# area_plot(fruit, "OBS", "Units", "paste(Product, Store, Size)")
# line_plot(fruit, "OBS", "Units", "paste(Product, Store, Size)")
# line_plot(fruit, "OBS", "Value", "paste(Product, Store, Size)")


