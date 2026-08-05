if (!exists("alert_ui", envir = environment(quality_control_server), inherits = FALSE)) {
  assign(
    "alert_ui",
    function(message, level = "info") {
      shiny::div(class = paste0("alert alert-", level), message)
    },
    envir = environment(quality_control_server)
  )
}
if (!exists(
  "metadata_sheet_link_ui",
  envir = environment(quality_control_server),
  inherits = FALSE
)) {
  assign(
    "metadata_sheet_link_ui",
    function(class = NULL) shiny::a(class = class, "Open metadata sheet"),
    envir = environment(quality_control_server)
  )
}

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

test_that("QC date defaults start on 1 July 2026 and end today in Zurich", {
  now <- as.POSIXct("2026-08-05 22:30:00", tz = "UTC")
  expect_equal(
    qc_default_date_range(now, "Europe/Zurich"),
    as.Date(c("2026-07-01", "2026-08-06"))
  )

  html <- htmltools::renderTags(qc_sidebar_ui("qc"))$html
  expect_match(html, 'data-initial-date="2026-07-01"', fixed = TRUE)
  expect_match(
    html,
    format(qc_default_date_range()[[2]], 'data-initial-date="%Y-%m-%d"'),
    fixed = TRUE
  )
})

test_that("QC selection and source signatures are deterministic", {
  first <- qc_selection_signature(
    "filters", species = c("oak", "beech"), plant_ids = c("2", "1"),
    date_range = as.Date(c("2026-07-01", "2026-08-05")),
    qualities = c("bad", "good")
  )
  second <- qc_selection_signature(
    "filters", species = c("beech", "oak"), plant_ids = c("1", "2"),
    date_range = as.Date(c("2026-07-01", "2026-08-05")),
    qualities = c("good", "bad")
  )
  expect_identical(first, second)

  catalog <- data.frame(
    id = c("b", "a"), modified_iso = c("2", "1"),
    quality = c("good", "bad"), species = c("oak", "beech"),
    plant_id = c("2", "1"), metadata_matches = 1L,
    metadata_status = "Exact ID", stringsAsFactors = FALSE
  )
  expect_identical(
    qc_catalog_signature(catalog),
    qc_catalog_signature(catalog[2:1, , drop = FALSE])
  )
  changed <- catalog
  changed$quality[[1]] <- "medium"
  expect_false(identical(
    qc_catalog_signature(catalog), qc_catalog_signature(changed)
  ))
})

test_that("QC prepares only the requested scope and never starts automatically", {
  clear_remote_cache()
  withr::defer(clear_remote_cache())

  modified <- as.POSIXct("2026-08-05 08:00:00", tz = "UTC")
  records <- data.frame(
    id = c("before", "july", "august"),
    name = c(
      "20260630_1000_before.csv",
      "20260702_1000_july.csv",
      "20260801_1000_august.csv"
    ),
    modified_time = modified - c(120, 60, 0),
    modified_iso = c(
      "2026-08-05T07:58:00Z",
      "2026-08-05T07:59:00Z",
      "2026-08-05T08:00:00Z"
    ),
    mime_type = "text/csv",
    size = 1,
    stringsAsFactors = FALSE
  )
  metadata <- clean_run_metadata(data.frame(
    timestamp = tools::file_path_sans_ext(records$name),
    `TREE species` = "oak",
    `plant id` = c("before", "july", "august"),
    `quality assessment` = "",
    check.names = FALSE,
    stringsAsFactors = FALSE
  ))
  parsed <- dew_point_fixture()
  for (index in seq_len(nrow(records))) {
    cache_remote_value(
      "measurement", records[index, , drop = FALSE], parsed
    )
  }

  shiny::testServer(
    quality_control_server,
    args = list(
      active = shiny::reactive(FALSE),
      measurements = shiny::reactive(records),
      metadata = shiny::reactive(metadata),
      config = list(
        background_workers = 2L,
        metadata_sheet_id = "sheet-test"
      )
    ),
    {
      session$flushReact()
      expect_null(analyzed_catalog())
      expect_equal(progress()$total, 0L)
      expect_false(progress()$loading)
      expect_match(output$progress$html, "Ready.", fixed = TRUE)

      session$setInputs(
        selection_mode = "filters",
        plant_ids = character(),
        qualities = c("good", "medium", "bad", "unassessed"),
        date_range = as.Date(c("2026-07-01", "2026-08-05"))
      )
      session$flushReact()
      session$setInputs(analyze_filtered = 1L)
      session$flushReact()

      expect_setequal(analyzed_catalog()$id, c("july", "august"))
      expect_equal(progress()$total, 2L)
      expect_equal(progress()$done, 2L)
      expect_false(progress()$loading)
      expect_match(output$progress$html, "2 runs", fixed = TRUE)

      session$setInputs(
        date_range = as.Date(c("2026-08-01", "2026-08-05"))
      )
      session$flushReact()
      expect_setequal(analyzed_catalog()$id, c("july", "august"))
      expect_match(output$scope_notice$html, "filters have changed", fixed = TRUE)

      session$setInputs(analyze_filtered = 2L)
      session$flushReact()
      expect_identical(as.character(analyzed_catalog()$id), "august")
      expect_equal(progress()$total, 1L)
      expect_equal(progress()$done, 1L)

      session$setInputs(analyze_all = 1L)
      session$flushReact()
      expect_setequal(analyzed_catalog()$id, records$id)
      expect_equal(progress()$total, 3L)
      expect_equal(progress()$done, 3L)
      expect_false(progress()$loading)
    }
  )
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
      quality = c("good", "bad"),
      walz_number = c("1", "2"),
      target_tcuv = c("24", "18"),
      xibox_temperature = c("26", "20"),
      xibox_light = c("9999", "875"),
      stringsAsFactors = FALSE
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
  expect_identical(
    unname(prepared$facet_labels[["run-positive"]]),
    paste0(
      "run-positive\n",
      "(W#: 1 (dimmer), WTemp: 24°C, XTemp: 26°C, XLight: 9999 PPFD)"
    )
  )
})

test_that("QC facet titles show settings without links and use a fixed 180-minute axis", {
  expect_identical(qc_walz_number_label("1.0"), "1 (dimmer)")
  expect_identical(qc_walz_number_label("2"), "2 (brighter)")
  fixture <- make_qc_timeseries_fixture()
  prepared <- prepare_qc_timeseries_data(fixture$prepared, fixture$catalog)
  widget <- make_qc_timeseries_plot(prepared, columns = 2L)
  annotations <- widget$x$layout$annotations
  annotation_text <- vapply(annotations, function(annotation) {
    if (is.null(annotation$text)) "" else as.character(annotation$text)
  }, character(1))

  expect_s3_class(widget, "plotly")
  expect_true(any(grepl("run-positive", annotation_text, fixed = TRUE)))
  expect_true(any(grepl("W#: 1 (dimmer)", annotation_text, fixed = TRUE)))
  expect_true(any(grepl("W#: 2 (brighter)", annotation_text, fixed = TRUE)))
  expect_false(any(grepl("<a href=", annotation_text, fixed = TRUE)))
  expect_true("plotly_click" %in% widget$x$shinyEvents)

  built <- suppressMessages(plotly::plotly_build(widget))
  trace_keys <- unique(unlist(lapply(built$x$data, `[[`, "key")))
  expect_setequal(as.character(trace_keys), prepared$run_ids)
  x_axes <- built$x$layout[grepl("^xaxis", names(built$x$layout))]
  expect_true(length(x_axes) >= 2L)
  expect_true(all(vapply(x_axes, function(axis) {
    isTRUE(all.equal(as.numeric(axis$range), c(0, 180)))
  }, logical(1))))
  line_traces <- Filter(function(trace) {
    identical(trace$type, "scatter") && grepl("lines", trace$mode, fixed = TRUE)
  }, built$x$data)
  raw_widths <- vapply(Filter(function(trace) {
    identical(trace$line$color, "rgba(86,97,107,1)")
  }, line_traces), function(trace) trace$line$width, numeric(1))
  mean_widths <- vapply(Filter(function(trace) {
    identical(trace$line$color, "rgba(27,158,119,1)")
  }, line_traces), function(trace) trace$line$width, numeric(1))
  expect_true(length(raw_widths) > 0L)
  expect_true(length(mean_widths) > 0L)
  expect_true(all(mean_widths %in% raw_widths))
})

test_that("QC normalization uses each run's positive maximum and defaults remain raw", {
  fixture <- make_qc_timeseries_fixture()
  prepared <- prepare_qc_timeseries_data(fixture$prepared, fixture$catalog)
  normalized <- make_qc_timeseries_plot(
    prepared, columns = 2L, normalized = TRUE
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

test_that("QC preparation invalidates only changed Drive file versions", {
  records <- data.frame(
    id = c("a", "b"),
    modified_iso = c("2026-08-01T10:00:00Z", "2026-08-01T11:00:00Z"),
    stringsAsFactors = FALSE
  )
  prepared <- list(
    a = list(record = records[1, , drop = FALSE], error = NULL),
    b = list(record = records[2, , drop = FALSE], error = "parse failure")
  )

  expect_equal(qc_prepared_count(prepared, records), 2L)
  expect_equal(qc_failed_count(prepared, records), 1L)
  expect_true(qc_entry_matches_record(prepared$a, records[1, , drop = FALSE]))

  changed <- records
  changed$modified_iso[[1]] <- "2026-08-01T12:00:00Z"
  expect_false(qc_entry_matches_record(prepared$a, changed[1, , drop = FALSE]))
  expect_equal(qc_prepared_count(prepared, changed), 1L)
  expect_equal(qc_failed_count(prepared, changed), 1L)
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

test_that("QC audit values are compact and missing values are explicit", {
  summary <- data.frame(
    step_id = c(1L, 2L), PPFD_mean = c(100.123, NA),
    A_mean = c(1.23456, 2), A_sd = c(0.0503393326297748, NA),
    A_slope = c(-0.012345, 0), n_window = c(181L, NA),
    coverage = c(1, NA), window_complete = c(TRUE, NA),
    warning = c("", NA), stringsAsFactors = FALSE
  )
  display <- format_qc_audit_table(summary)

  expect_equal(display$PPFD_mean, c("100.1", "—"))
  expect_equal(display$A_mean, c("1.235", "2.000"))
  expect_equal(display$A_sd, c("0.050", "—"))
  expect_equal(display$A_slope, c("-0.0123", "0.0000"))
  expect_equal(display$coverage, c("100%", "—"))
  expect_equal(display$window_complete, c("Yes", "—"))
  expect_equal(display$warning, c("—", "—"))
})

test_that("QC metadata stays collapsed until requested", {
  html <- htmltools::renderTags(qc_main_ui("qc"))$html
  position <- regexpr("Metadata for plotted runs", html, fixed = TRUE)[[1]]
  panel <- substr(html, position, position + 700L)
  expect_match(panel, "accordion-button collapsed", fixed = TRUE)
  expect_match(panel, "aria-expanded=\"false\"", fixed = TRUE)
})
