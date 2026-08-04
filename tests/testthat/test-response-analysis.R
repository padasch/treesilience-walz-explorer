make_response_steps <- function(run_count = 5L, light_count = 6L) {
  lights <- seq(0, 1200, length.out = light_count)
  rows <- lapply(seq_len(run_count), function(index) {
    temperature <- 16 + index * 3
    data.frame(
      run_id = sprintf("run-%02d", index), step_id = seq_along(lights),
      A_mean = 13 * (1 - exp(-lights / 300)) - .08 * (temperature - 25)^2 - .5,
      A_sd = .1, A_slope = 0, Tleaf_mean = temperature,
      Tleaf_sd = .05, PPFD_mean = lights, PPFD_sd = 2,
      n_window = 181L, expected_n = 181L, coverage = 1,
      window_complete = TRUE, include_model = TRUE, warning = "",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

test_that("response coverage enforces runs, light levels, and observations", {
  ready <- response_model_coverage(make_response_steps())
  expect_true(ready$ready)
  expect_equal(ready$summary$distinct_runs, 5L)

  too_few <- response_model_coverage(make_response_steps(run_count = 3L, light_count = 4L))
  expect_false(too_few$ready)
  expect_match(too_few$summary$message, "temperature runs")
})

test_that("GAM predictions are masked outside interpolation support", {
  steps <- make_response_steps()
  steps <- steps[!(steps$Tleaf_mean > 27 & steps$PPFD_mean > 700), , drop = FALSE]
  result <- fit_response_gam(steps, grid_size = 30L)
  expect_equal(result$status, "success")
  expect_true(any(!result$grid$inside_support))
  expect_true(all(is.na(result$grid$predicted_A[!result$grid$inside_support])))
  expect_true(all(c(
    "deviance_explained", "rmse", "predictive_r_squared",
    "boundary_optimum_proportion"
  ) %in% names(result$diagnostics)))
  expect_true(all(result$optima$all$status %in% c(
    "boundary optimum", "interior optimum", "no positive optimum"
  )))
})

test_that("insufficient data retain raw workflow but suppress modeling", {
  result <- fit_response_gam(make_response_steps(run_count = 2L, light_count = 3L))
  expect_equal(result$status, "insufficient")
  expect_match(result$message, "suppressed")
})

test_that("local-analysis presets resolve exact Drive runs and report missing files", {
  catalog <- data.frame(
    id = paste0("id-", seq_along(WALZ_CURATED_OAK_RUNS)),
    run_id = WALZ_CURATED_OAK_RUNS,
    species = "oak", plant_id = "oak-1", quality = "unassessed",
    stringsAsFactors = FALSE
  )
  resolved <- resolve_response_preset("curated_oak", catalog)
  expect_equal(resolved$matched, WALZ_CURATED_OAK_RUNS)
  expect_length(resolved$ids, 10L)
  expect_length(resolved$missing, 0L)

  partial <- resolve_response_preset("curated_all", catalog)
  expect_length(partial$ids, 10L)
  expect_length(partial$missing, 11L)
})

test_that("manual response selection keeps user order and can include unmatched runs", {
  catalog <- data.frame(
    id = c("a", "b", "c"), run_id = c("run-a", "run-b", "run-c"),
    species = c("oak", "", "beech"), plant_id = c("1", "", "2"),
    quality = c("good", "unassessed", "bad"), metadata_matches = c(1L, 0L, 1L),
    stringsAsFactors = FALSE
  )
  selected <- manual_response_catalog(catalog, c("b", "a"))
  expect_equal(selected$id, c("b", "a"))
  choices <- response_run_choices(catalog)
  expect_equal(unname(choices), catalog$id)
  expect_true(any(grepl("species unavailable", names(choices), fixed = TRUE)))
})

test_that("response line plots expose continuous color legends", {
  steps <- make_response_steps()
  observed <- suppressMessages(plotly::plotly_build(make_observed_response_plot(steps)))
  expect_true(isTRUE(observed$x$data[[1]]$marker$showscale))
  expect_equal(observed$x$data[[1]]$marker$colorbar$title$text, "Mean Tleaf (°C)")
  expect_true(all(vapply(observed$x$data, function(trace) !isTRUE(trace$showlegend), logical(1))))

  model <- fit_response_gam(steps, grid_size = 30L)
  model$supported <- TRUE
  slices <- suppressMessages(plotly::plotly_build(make_temperature_slice_plot(model)))
  expect_true(isTRUE(slices$x$data[[1]]$marker$showscale))
  expect_equal(slices$x$data[[1]]$marker$colorbar$title$text, "Measured PPFD")
})
