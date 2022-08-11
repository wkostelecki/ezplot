# ezplot 0.7.3
- updated to use tidyr::gather instead of gather_
- fixed bar_plot labels edge case
- message cleanup

# ezplot 0.7.2
- updated histogram_plot to pass bare y to geom_histogram
- added limits_y argument to line_plot

# ezplot 0.7.0
- added instantaneous lift to perf_df

# ezplot 0.6.6
- added quantiles option to perf_df
- fixed precision-recall plots when categorical target is used

# ezplot 0.6.5
- added legend to ks_plot
- added legend_ncol argument where applicable
- added reorder argument to line_plot to match other plot functions
- added tests

# ezplot 0.6.2
- fixed variable_plot
- updates to ez_jet for n < 5
- fixed issues with tsibble/tsibbledata
- added ks_plot

# ezplot 0.6.0
- added performance_plot
- general clean up

# ezplot 0.5.1
- minor fixes and clean up

# ezplot 0.5.0
- added density_plot
- added histogram_plot
- added pr_plot
- added lift_plot
- fixes for new dplyr release

# ezplot 0.4.1
- fixed problems where geom_col(orientation = "x") was no longer used by default

# ezplot 0.4.0
- prep for dplyr 1.0.0
- fixed "don't test" examples
- added ez_app and variable_plot
- coord_flip option for bar_plot
- deleted mean_plot

# ezplot 0.3.1
- bug fixes for tsibbles
- clean up
- deleted ez_data and ez_log_breaks

# ezplot 0.3.0
- added functions: distribution_plot, secondary_plot, calendar_plot
- aesthetic updates
- updated examples to use tsibbledata package
- general cleanup

# ezplot 0.2.2
First CRAN release.
