test_that("both established plot views build from a parsed WALZ file", {
  skip_if_not_installed("plotly")
  parsed <- read_walz_csv(fixture_path("walz_sample.csv"))

  expect_s3_class(make_timeseries_plot(parsed, show_grid = TRUE), "plotly")
  expect_s3_class(make_state_plot(parsed), "plotly")
})
