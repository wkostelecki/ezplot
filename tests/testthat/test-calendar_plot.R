
context("calendar_plot")

test_that("runs", {

  g = calendar_plot(vic_elec, "Time", "Demand", zlim = c(NA, NA))

  expect_true(all(c("x", "y", "facet_x", "facet_y", "z") %in% names(g[["data"]])))

})

