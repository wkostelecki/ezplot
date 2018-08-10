## Overview
ezplot provides high-level wrapper functions for common chart types.

## Intallation
devtools::install_github("wkostelecki/ezplot")

## example data
df = ez_data()

summary(df)

## line_plot
- `line_plot(df, "year2", "units")` plots unit sales with "year2" aggregation along x-axis.
- `line_plot(df, "year2", "units", "num")` adds "num" grouping.
- `line_plot(df, "year2", "units", "num", "fct")` adds "fct" faceting with facet_wrap().
- `line_plot(df, "year2", "units", "num", "fct", "char")` adds "fct" and "char" faceting with facet_grid().
