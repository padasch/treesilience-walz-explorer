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

make_response_audit_runs <- function() {
  start <- as.POSIXct("2026-07-15 09:00:00", tz = "Europe/Zurich")
  positive_raw <- data.frame(
    Datetime = start + seq(0, 6) * 60,
    .elapsed_minutes = seq(0, 6),
    A = c(-1, 0, 1, 2, 4, 3, 2),
    stringsAsFactors = FALSE
  )
  positive_summary <- data.frame(
    step_id = 1:2,
    window_start = start + c(0, 3) * 60,
    window_end = start + c(2, 6) * 60,
    A_mean = c(2, 4), A_sd = c(0.4, 0.8),
    PPFD_mean = c(100, 500), coverage = c(1, 0.95),
    stringsAsFactors = FALSE
  )
  negative_raw <- data.frame(
    Datetime = start + seq(0, 3) * 60,
    .elapsed_minutes = seq(0, 3),
    A = c(-4, -3, -2, -1),
    stringsAsFactors = FALSE
  )
  list(
    `run-positive` = list(raw = positive_raw, summary = positive_summary),
    `run-negative` = list(raw = negative_raw, summary = positive_summary[1, , drop = FALSE])
  )
}

if (!exists("alert_ui", envir = environment(response_model_notice_ui), inherits = FALSE)) {
  assign(
    "alert_ui",
    function(message, type = "info") {
      shiny::div(class = paste0("alert alert-", type), message)
    },
    envir = environment(response_model_notice_ui)
  )
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

test_that("first-analysis presets stay species-specific and resolve exact Drive runs", {
  expect_equal(names(WALZ_RESPONSE_PRESETS), c("curated_oak", "curated_beech"))
  expect_equal(
    unname(WALZ_RESPONSE_PRESET_LABELS),
    c(
      "First Analysis — Oak temperature series (10)",
      "First Analysis — Beech temperature series (10)"
    )
  )
  expect_false(any(grepl("oak + beech|full curated|prunus", WALZ_RESPONSE_PRESET_LABELS, ignore.case = TRUE)))

  catalog <- data.frame(
    id = googledrive::as_id(paste0("id-", seq_along(WALZ_CURATED_OAK_RUNS))),
    run_id = WALZ_CURATED_OAK_RUNS,
    species = "oak", plant_id = "oak-1", quality = "unassessed",
    stringsAsFactors = FALSE
  )
  resolved <- resolve_response_preset("curated_oak", catalog)
  expect_equal(resolved$matched, WALZ_CURATED_OAK_RUNS)
  expect_type(resolved$ids, "character")
  expect_length(resolved$ids, 10L)
  expect_length(resolved$missing, 0L)
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
  run_temperatures <- vapply(
    observed$x$data,
    function(trace) unique(as.numeric(trace$marker$color))[[1]],
    numeric(1)
  )
  limits <- range(run_temperatures)
  expected_line_colours <- vapply(
    run_temperatures, walz_tleaf_colour, character(1), limits = limits
  )
  actual_line_colours <- vapply(observed$x$data, function(trace) trace$line$color, character(1))
  expect_equal(toupper(actual_line_colours), toupper(expected_line_colours))
  expect_gt(length(unique(actual_line_colours)), 1L)
  expect_equal(toupper(walz_tleaf_colour(min(limits), limits)), "#2166AC")
  expect_equal(toupper(walz_tleaf_colour(max(limits), limits)), "#B2182B")

  observed_temperature <- suppressMessages(plotly::plotly_build(
    make_observed_temperature_slice_plot(steps)
  ))
  expect_equal(length(observed_temperature$x$data), 6L)
  expect_true(isTRUE(observed_temperature$x$data[[1]]$marker$showscale))
  expect_equal(
    observed_temperature$x$data[[1]]$marker$colorbar$title$text,
    "PPFD slice"
  )
  expect_true(all(vapply(
    observed_temperature$x$data,
    function(trace) !isTRUE(trace$showlegend), logical(1)
  )))

  model <- fit_response_gam(steps, grid_size = 30L)
  model$supported <- TRUE
  modeled_light <- suppressMessages(plotly::plotly_build(
    make_modeled_light_slice_plot(model)
  ))
  expect_equal(length(modeled_light$x$data), 5L)
  expect_true(isTRUE(modeled_light$x$data[[1]]$marker$showscale))
  expect_equal(
    modeled_light$x$data[[1]]$marker$colorbar$title$text,
    "Mean Tleaf (°C)"
  )
  expect_true(all(grepl("Modeled A", vapply(
    modeled_light$x$data,
    function(trace) trace$text[[1]], character(1)
  ), fixed = TRUE)))

  slices <- suppressMessages(plotly::plotly_build(make_temperature_slice_plot(model)))
  expect_true(isTRUE(slices$x$data[[1]]$marker$showscale))
  expect_equal(slices$x$data[[1]]$marker$colorbar$title$text, "Measured PPFD")
})

test_that("observed temperature slices use measured values in bounded PPFD bands", {
  steps <- make_response_steps(run_count = 5L, light_count = 14L)
  targets <- response_ppfd_slice_targets(steps)
  prepared <- prepare_observed_temperature_slices(steps)

  expect_lte(length(targets), 10L)
  expect_setequal(unique(prepared$PPFD_slice), targets)
  expect_true(all(prepared$n_points >= 1L))
  expect_true(all(prepared$run_id %in% unique(steps$run_id)))
  expect_true(all(prepared$A_mean >= min(steps$A_mean)))
  expect_true(all(prepared$A_mean <= max(steps$A_mean)))
})

test_that("response analysis ends with a concise explanation of the fitted model", {
  html <- htmltools::renderTags(response_main_ui("analysis"))$html
  expect_match(html, "Model theory", fixed = TRUE)
  expect_match(html, "tensor-product smooth", fixed = TRUE)
  expect_match(html, "REML", fixed = TRUE)
  expect_match(html, "log(1 + PPFD_mean)", fixed = TRUE)
  expect_match(html, "outside the sampled temperature–PPFD support", fixed = TRUE)
  expect_match(html, "without a separate run effect", fixed = TRUE)
  expect_match(html, "Actual mode connects the extracted three-minute means", fixed = TRUE)
  expect_match(html, "same fitted two-dimensional surface", fixed = TRUE)
  expect_lt(
    regexpr("Model diagnostics", html, fixed = TRUE)[[1]],
    regexpr("Model theory", html, fixed = TRUE)[[1]]
  )
})

test_that("response slices are arranged above the raw and fitted landscapes", {
  html <- htmltools::renderTags(response_main_ui("analysis"))$html
  positions <- vapply(c(
    "A versus measured PPFD by Tleaf",
    "A versus Tleaf by measured PPFD",
    "Raw extracted landscape",
    "GAM surface within observed support"
  ), function(label) regexpr(label, html, fixed = TRUE)[[1]], integer(1))
  expect_true(all(positions > 0L))
  expect_true(all(diff(positions) > 0L))
  expect_equal(lengths(regmatches(html, gregexpr("response-two-column", html, fixed = TRUE))), 2L)
  expect_lt(
    regexpr("GAM surface within observed support", html, fixed = TRUE)[[1]],
    regexpr("Extraction audit", html, fixed = TRUE)[[1]]
  )
  expect_lt(
    regexpr("analysis-normalized_audit_section", html, fixed = TRUE)[[1]],
    regexpr("Extraction audit", html, fixed = TRUE)[[1]]
  )
  expect_lt(
    regexpr("Extraction audit", html, fixed = TRUE)[[1]],
    regexpr("Model theory", html, fixed = TRUE)[[1]]
  )
})

test_that("response slice display defaults to actual measurements", {
  sidebar_html <- htmltools::renderTags(response_sidebar_ui("analysis"))$html
  expect_match(sidebar_html, "Values to display", fixed = TRUE)
  expect_match(sidebar_html, "Actual measurements", fixed = TRUE)
  expect_match(sidebar_html, "GAM fits", fixed = TRUE)
  expect_match(
    sidebar_html,
    'value="observed" checked="checked"',
    fixed = TRUE
  )
  expect_match(sidebar_html, "without rerunning the analysis", fixed = TRUE)
})

test_that("raw response audit is max-normalized with window means and SD", {
  prepared <- prepare_normalized_response_audit(make_response_audit_runs())
  expect_equal(prepared$run_ids, "run-positive")
  expect_equal(max(prepared$raw$A_normalized, na.rm = TRUE), 1)
  expect_equal(unique(prepared$raw$positive_max), 4)
  expect_equal(prepared$windows$A_mean_normalized, c(0.5, 1))
  expect_equal(prepared$windows$A_sd_normalized, c(0.1, 0.2))
  expect_equal(prepared$windows$window_start, c(0, 3))
  expect_equal(prepared$windows$window_end, c(2, 6))
  expect_equal(
    prepared$windows$window_type,
    c("three minutes before next light step", "final three minutes of terminal step")
  )
  expect_true(any(grepl("no positive A maximum", prepared$warnings, fixed = TRUE)))

  widget <- suppressWarnings(make_normalized_response_audit_plot(prepared))
  expect_s3_class(widget, "plotly")
  expect_gt(length(plotly::plotly_build(widget)$x$data), 0L)
  section_html <- htmltools::renderTags(
    response_normalized_audit_section_ui(shiny::NS("analysis"), prepared)
  )$html
  expect_match(section_html, "Max-normalized raw A and extraction windows", fixed = TRUE)
  expect_match(section_html, "thick green lines", fixed = TRUE)
  expect_match(section_html, "mean ± 1 SD", fixed = TRUE)
  expect_match(section_html, "analysis-normalized_audit_plot", fixed = TRUE)
})

test_that("poor model fits show one metric warning and retain fitted outputs", {
  bad_model <- list(
    status = "success", supported = FALSE,
    diagnostics = data.frame(
      deviance_explained = 0.42,
      adjusted_r_squared = 0.35,
      predictive_r_squared = 0.18,
      k_p_value = 0.012,
      boundary_optimum_proportion = 0.7
    )
  )
  message <- model_fit_warning_message(bad_model)
  expect_match(message, "deviance explained = 0.42", fixed = TRUE)
  expect_match(message, "adjusted R² = 0.35", fixed = TRUE)
  expect_match(message, "predictive R² = 0.18", fixed = TRUE)
  expect_match(message, "basis-check p-value = 0.012", fixed = TRUE)
  expect_match(message, "boundary optima = 70%", fixed = TRUE)
  expect_match(message, "still shown", fixed = TRUE)

  poor_html <- htmltools::renderTags(response_model_notice_ui(bad_model))$html
  expect_match(poor_html, "Model quality is low", fixed = TRUE)
  layout_html <- htmltools::renderTags(response_main_ui("analysis"))$html
  expect_match(layout_html, "analysis-temperature_plot", fixed = TRUE)
  expect_match(layout_html, "analysis-surface_3d", fixed = TRUE)
  optima_html <- htmltools::renderTags(
    response_optima_section_ui(shiny::NS("analysis"), bad_model)
  )$html
  expect_match(optima_html, "analysis-optima_table", fixed = TRUE)
  poor_download_html <- htmltools::renderTags(
    response_download_buttons_ui(shiny::NS("analysis"), bad_model)
  )$html
  expect_match(poor_download_html, "Extracted steps", fixed = TRUE)
  expect_match(poor_download_html, "Diagnostics", fixed = TRUE)
  expect_match(poor_download_html, "Model predictions", fixed = TRUE)
  expect_match(poor_download_html, "analysis-download_optima", fixed = TRUE)

  good_model <- bad_model
  good_model$supported <- TRUE
  expect_null(response_model_notice_ui(good_model))
  good_html <- htmltools::renderTags(
    response_optima_section_ui(shiny::NS("analysis"), good_model)
  )$html
  expect_match(good_html, "analysis-optima_table", fixed = TRUE)
  good_download_html <- htmltools::renderTags(
    response_download_buttons_ui(shiny::NS("analysis"), good_model)
  )$html
  expect_match(good_download_html, "Model predictions", fixed = TRUE)
  expect_match(good_download_html, "analysis-download_optima", fixed = TRUE)
})

test_that("low diagnostics do not blank fitted response plots", {
  model <- fit_response_gam(make_response_steps(), grid_size = 30L)
  expect_equal(model$status, "success")
  model$supported <- FALSE
  light <- suppressMessages(plotly::plotly_build(make_modeled_light_slice_plot(model)))
  temperature <- suppressMessages(plotly::plotly_build(make_temperature_slice_plot(model)))
  surface <- suppressWarnings(suppressMessages(
    plotly::plotly_build(make_surface_response_3d(model))
  ))
  expect_gt(length(light$x$data), 0L)
  expect_gt(length(temperature$x$data), 0L)
  expect_gt(length(surface$x$data), 0L)
})
