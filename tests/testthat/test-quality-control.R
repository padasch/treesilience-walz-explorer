test_that("catalog keeps unmatched runs and canonicalizes quality", {
  modified <- as.POSIXct("2026-08-01", tz = "UTC")
  files <- data.frame(
    id = c("a", "b", "c"),
    name = c("20260801_1000_a.csv", "20260801_1100_b.csv", "20260801_1200_c.csv"),
    modified_iso = rep("2026-08-01T00:00:00Z", 3),
    modified_time = rep(modified, 3), stringsAsFactors = FALSE
  )
  metadata <- clean_run_metadata(data.frame(
    timestamp = c("20260801_1000_a", "20260801_1100_b"),
    `TREE species` = c("oak", "oak"),
    `plant id` = c("A", "B"),
    `quality assessment` = c("GOOD", "unexpected"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  catalog <- build_run_catalog(files, metadata)
  expect_equal(catalog$quality, c("good", "unassessed", "unassessed"))
  expect_false(catalog$quality_invalid[[1]])
  expect_true(catalog$quality_invalid[[2]])
  expect_equal(catalog$metadata_matches, c(1L, 1L, 0L))
  expect_equal(catalog$metadata_status[[3]], "No metadata row")
  expect_equal(stable_run_colour(catalog$run_id), catalog$colour)
})
make_qc_timeseries_fixture <- function() {
  start <- as.POSIXct("2026-08-01 10:00:00", tz = WALZ_TIMEZONE)
  make_entry <- function(values) {
    raw <- data.frame(
      Datetime = start + 0:6 * 60,
      .elapsed_minutes = 0:6,
      A = values,
      stringsAsFactors = FALSE
    )
    summary <- data.frame(
      step_id = 1:2,
      window_start = start + c(0, 3) * 60,
      window_end = start + c(2, 6) * 60,
      A_mean = c(mean(values[1:3]), mean(values[4:7])),
      A_sd = c(stats::sd(values[1:3]), stats::sd(values[4:7])),
      PPFD_mean = c(100, 500), coverage = 1,
      stringsAsFactors = FALSE
    )
    list(error = NULL, extraction = list(raw = raw, summary = summary))
  }
  list(
    prepared = list(
      a = make_entry(c(-1, 0, 1, 2, 4, 3, 2)),
      b = make_entry(c(-4, -3, -2, -2, -1, -1, -0.5))
    ),
    catalog = data.frame(
      id = c("a", "b"), run_id = c("run-positive", "run-negative"),
      quality = c("good", "bad"), stringsAsFactors = FALSE
    )
  )
}

test_that("QC timeseries preparation retains raw A and entirely negative runs", {
  fixture <- make_qc_timeseries_fixture()
  prepared <- prepare_qc_timeseries_data(fixture$prepared, fixture$catalog)

  expect_equal(prepared$run_ids, c("run-positive", "run-negative"))
  expect_equal(nrow(prepared$raw), 14L)
  expect_equal(range(prepared$raw$A_raw), c(-4, 4))
  expect_equal(nrow(prepared$windows), 4L)
  expect_equal(unique(as.character(prepared$windows$run_id)), prepared$run_ids)
  expect_length(prepared$warnings, 0L)
  expect_equal(qc_display_run_ids(prepared, FALSE), prepared$run_ids)
  expect_equal(qc_display_run_ids(prepared, TRUE), "run-positive")
  expect_match(prepared$normalization_warnings, "run-negative has no positive A maximum")
})

test_that("QC timeseries facets link run titles to the metadata sheet", {
  fixture <- make_qc_timeseries_fixture()
  prepared <- prepare_qc_timeseries_data(fixture$prepared, fixture$catalog)
  widget <- make_qc_timeseries_plot(
    prepared, columns = 2L, metadata_sheet_id = "sheet-test"
  )
  annotations <- widget$x$layout$annotations
  linked <- Filter(function(annotation) {
    isTRUE(annotation$captureevents) && grepl("<a href=", annotation$text, fixed = TRUE)
  }, annotations)

  expect_s3_class(widget, "plotly")
  expect_length(linked, 2L)
  expect_setequal(vapply(linked, `[[`, character(1), "name"), prepared$run_ids)
  expect_true(all(vapply(linked, function(annotation) {
    grepl("docs.google.com/spreadsheets/d/sheet-test/edit", annotation$text, fixed = TRUE)
  }, logical(1))))
  expect_true(all(c("plotly_click", "plotly_clickannotation") %in% widget$x$shinyEvents))

  built <- suppressMessages(plotly::plotly_build(widget))
  trace_keys <- unique(unlist(lapply(built$x$data, `[[`, "key")))
  expect_setequal(as.character(trace_keys), prepared$run_ids)
})

test_that("QC normalization uses each run's positive maximum and defaults remain raw", {
  fixture <- make_qc_timeseries_fixture()
  prepared <- prepare_qc_timeseries_data(fixture$prepared, fixture$catalog)
  normalized <- make_qc_timeseries_plot(
    prepared, columns = 2L, normalized = TRUE, metadata_sheet_id = "sheet-test"
  )
  built <- suppressMessages(plotly::plotly_build(normalized))
  trace_keys <- unique(unlist(lapply(built$x$data, `[[`, "key")))
  normalized_values <- unlist(lapply(built$x$data, function(trace) {
    if (is.null(trace$key)) numeric() else as.numeric(trace$y)
  }))

  expect_setequal(as.character(trace_keys), "run-positive")
  expect_equal(max(normalized_values, na.rm = TRUE), 1)
  expect_equal(qc_a_axis_label(TRUE), "A / positive run maximum")
  expect_match(qc_a_axis_label(FALSE), "µmol m⁻² s⁻¹", fixed = TRUE)
})

test_that("QC facet columns are bounded and determine section height", {
  expect_equal(qc_facet_columns(NULL), 2L)
  expect_equal(qc_facet_columns(0), 1L)
  expect_equal(qc_facet_columns(5), 4L)
  expect_equal(qc_timeseries_plot_height(8, 2), 1085L)
  expect_lt(qc_timeseries_plot_height(8, 4), qc_timeseries_plot_height(8, 2))
})

test_that("QC manual selection preserves the requested run order", {
  catalog <- data.frame(
    id = c("a", "b", "c"), run_id = c("run-a", "run-b", "run-c"),
    species = c("oak", "", "beech"), plant_id = c("1", "", "2"),
    quality = c("good", "unassessed", "bad"), stringsAsFactors = FALSE
  )
  selected <- manual_qc_catalog(catalog, c("c", "a", "missing"))
  expect_equal(selected$id, c("c", "a"))
  choices <- qc_run_choices(catalog)
  expect_equal(unname(choices), catalog$id)
  expect_true(any(grepl("species unavailable", names(choices), fixed = TRUE)))
})

test_that("QC selected-run control updates only when choices or selection change", {
  catalog <- data.frame(run_id = c("run-a", "run-b"), stringsAsFactors = FALSE)
  initial <- qc_selected_run_control_state(catalog, NULL, character())
  expect_true(initial$update)
  expect_equal(initial$selected, "run-a")

  stable <- qc_selected_run_control_state(catalog, "run-a", initial$values)
  expect_false(stable$update)

  changed_selection <- qc_selected_run_control_state(catalog, "run-b", initial$values)
  expect_false(changed_selection$update)
  expect_equal(changed_selection$selected, "run-b")

  reduced <- qc_selected_run_control_state(catalog[2, , drop = FALSE], "run-a", initial$values)
  expect_true(reduced$update)
  expect_equal(reduced$selected, "run-b")
})

test_that("QC raw-audit choices can contain the complete measurement catalog", {
  complete <- data.frame(
    run_id = c("run-new", "run-filtered-out", "run-unmatched"),
    stringsAsFactors = FALSE
  )
  overview_subset <- complete[1, , drop = FALSE]

  state <- qc_selected_run_control_state(complete, "run-filtered-out", character())

  expect_equal(state$values, complete$run_id)
  expect_equal(state$selected, "run-filtered-out")
  expect_false("run-unmatched" %in% overview_subset$run_id)
  expect_true("run-unmatched" %in% state$values)
})

test_that("QC raw audit shows three-minute means and SD bars for A and PPFD", {
  start <- as.POSIXct("2026-08-01 10:00:00", tz = WALZ_TIMEZONE)
  raw <- data.frame(
    Datetime = start + 0:9 * 60,
    .elapsed_minutes = 0:9,
    A = seq(1, 2.8, length.out = 10),
    PARtop = rep(c(100, 400), each = 5)
  )
  summary <- data.frame(
    step_id = 1:2,
    window_start = start + c(2, 7) * 60,
    window_end = start + c(5, 10) * 60,
    A_mean = c(1.6, 2.5), A_sd = c(0.12, 0.18),
    A_slope = c(0.01, -0.02),
    PPFD_mean = c(100, 400), PPFD_sd = c(2, 5),
    coverage = c(1, 0.9), window_complete = c(TRUE, FALSE),
    warning = c("", "Short window"), stringsAsFactors = FALSE
  )
  entry <- list(error = NULL, extraction = list(raw = raw, summary = summary))

  built <- suppressMessages(plotly::plotly_build(make_qc_audit_plot(entry)))
  trace_names <- vapply(built$x$data, function(trace) {
    if (is.null(trace$name)) "" else as.character(trace$name)
  }, character(1))
  mean_traces <- Filter(function(trace) identical(trace$name, "3-minute mean"), built$x$data)
  error_traces <- Filter(function(trace) identical(trace$name, "Mean ± 1 SD"), built$x$data)

  expect_equal(sum(trace_names == "3-minute mean"), 2L)
  expect_equal(sum(trace_names == "Mean ± 1 SD"), 2L)
  expect_true(all(vapply(mean_traces, function(trace) trace$line$width == 6, logical(1))))
  expect_equal(as.numeric(error_traces[[1]]$error_y$array), summary$A_sd)
  expect_equal(as.numeric(error_traces[[2]]$error_y$array), summary$PPFD_sd)
})
