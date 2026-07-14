test_that("both established plot views build from a parsed WALZ file", {
  skip_if_not_installed("plotly")
  parsed <- read_walz_csv(fixture_path("walz_sample.csv"))

  expect_s3_class(make_timeseries_plot(parsed, show_grid = TRUE), "plotly")
  expect_s3_class(make_state_plot(parsed), "plotly")
})

test_that("numeric variables are grouped in physiological order", {
  parsed <- read_walz_csv(fixture_path("walz_sample.csv"))
  choices <- plot_variable_choices(list(parsed))
  groups <- group_plot_variable_choices(choices)

  expect_setequal(unname(choices), plottable_variables(parsed))
  expect_true(all(c("Area", "Tcuv", "Tleaf", "A", "PARtop") %in% unname(choices)))
  expect_true(any(grepl("Area", names(choices), fixed = TRUE)))
  expect_equal(head(unname(choices), 3), c("A", "GH2O", "E"))
  expect_equal(tail(unname(choices), 1), "Area")
  expect_equal(unname(groups$response), c("A", "GH2O", "E"))
  expect_setequal(
    unname(groups$environmental),
    setdiff(plottable_variables(parsed), c("A", "GH2O", "E", "Area"))
  )
  expect_equal(unname(groups$physiological_constant), "Area")
  expect_match(names(groups$response)[[1]], "^Net CO2")
  expect_true("GH2O" %in% WALZ_PLOT_VARIABLES)
  expect_true("Tcuv" %in% WALZ_PLOT_VARIABLES)
  expect_true("Tamb" %in% WALZ_PLOT_VARIABLES)
  expect_false("Tleaf" %in% WALZ_PLOT_VARIABLES)
})

test_that("two runs overlay from elapsed minute zero in both views", {
  skip_if_not_installed("plotly")
  primary <- read_walz_csv(fixture_path("walz_sample.csv"))
  comparison <- primary
  comparison$data$Datetime <- comparison$data$Datetime + (24 * 60 * 60)
  comparison$data$A <- comparison$data$A + 0.5

  primary_long <- measurement_long_data(
    primary,
    variables = c("A", "Tcuv"),
    run_label = "primary.csv"
  )
  comparison_long <- measurement_long_data(
    comparison,
    variables = c("A", "Tcuv"),
    run_label = "overlay.csv"
  )

  expect_equal(min(primary_long$ElapsedMinutes), 0)
  expect_equal(min(comparison_long$ElapsedMinutes), 0)
  expect_s3_class(
    make_timeseries_plot(
      primary,
      show_grid = TRUE,
      variables = c("A", "Tcuv", "PARtop"),
      comparison = comparison,
      run_labels = c("primary.csv", "overlay.csv")
    ),
    "plotly"
  )
  expect_s3_class(
    make_state_plot(
      primary,
      variables = c("A", "Tcuv", "PARtop"),
      comparison = comparison,
      run_labels = c("primary.csv", "overlay.csv")
    ),
    "plotly"
  )
})
