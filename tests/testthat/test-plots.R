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

test_that("any number of runs overlay from elapsed minute zero with unique colors", {
  skip_if_not_installed("plotly")
  primary <- read_walz_csv(fixture_path("walz_sample.csv"))
  comparison <- primary
  third <- primary
  comparison$data$Datetime <- comparison$data$Datetime + (24 * 60 * 60)
  comparison$data$A <- comparison$data$A + 0.5
  third$data$Datetime <- third$data$Datetime + (2 * 24 * 60 * 60)
  third$data$A <- third$data$A + 1

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
  labels <- c("primary.csv", "comparison.csv", "third.csv")
  colors <- c("#28754D", "#BD5D38", "#426A8C")
  timeseries <- make_timeseries_plot(
    list(primary, comparison, third),
    show_grid = TRUE,
    variables = c("A", "Tcuv", "PARtop"),
    run_labels = labels,
    run_colors = colors
  )
  state <- make_state_plot(
    list(primary, comparison, third),
    variables = c("A", "Tcuv", "PARtop"),
    run_labels = labels,
    run_colors = colors
  )

  expect_s3_class(timeseries, "plotly")
  expect_s3_class(state, "plotly")

  built <- plotly::plotly_build(timeseries)
  axis_names <- grep("^[xy]axis[0-9]*$", names(built$x$layout), value = TRUE)
  expect_true(length(axis_names) > 0L)
  expect_true(all(vapply(
    built$x$layout[axis_names],
    function(axis) isTRUE(axis$showspikes),
    logical(1)
  )))
  expect_true(all(vapply(
    built$x$layout[axis_names],
    function(axis) identical(axis$spikemode, "across+toaxis"),
    logical(1)
  )))
  expect_length(unique(run_palette(12)), 12L)
})
