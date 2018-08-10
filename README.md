## Overview
ezplot provides high-level wrapper functions for common chart types.

## Intallation
devtools::install_github("wkostelecki/ezplot")

## example data
df = ez_data()
summary(df)

## line_plot
line_plot(df, "year2", "units")
line_plot(df, "year2", "units", "num")
line_plot(df, "year2", "units", "num", "fct")
line_plot(df, "year2", "units", "num", "fct", "char")
