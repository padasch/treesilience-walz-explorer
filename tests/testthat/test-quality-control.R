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
test_that("QC normalization keeps negative A and omits non-positive runs", {
  data <- data.frame(
    run_id = rep(c("positive", "negative"), each = 3),
    PPFD_mean = rep(c(0, 100, 500), 2), PPFD_sd = 1,
    A_mean = c(-2, 2, 4, -4, -2, 0), A_sd = .1, A_slope = 0,
    coverage = 1, window_complete = TRUE, quality = "good",
    species = "oak", plant_id = "1", metadata_status = "Exact ID",
    colour = rep(c("#28754D", "#BD5D38"), each = 3),
    stringsAsFactors = FALSE
  )
  result <- make_qc_overview_plot(data)
  expect_s3_class(result$widget, "plotly")
  expect_true(any(grepl("negative has no positive", result$warnings)))
  built <- suppressMessages(plotly::plotly_build(result$widget))
  normalized_positive <- Filter(function(trace) {
    identical(trace$name, "positive") && any(abs(unlist(trace$y) - c(-.5, .5, 1)) < 1e-8)
  }, built$x$data)
  expect_true(length(normalized_positive) >= 1L)
  expect_false(any(vapply(
    built$x$data,
    function(trace) identical(trace$name, "negative") && identical(trace$y, c(-.5, -.25, 0)),
    logical(1)
  )))
})

test_that("QC overview pairs each quality row with original left and normalized right", {
  qualities <- c("good", "medium", "bad", "unassessed")
  data <- do.call(rbind, lapply(seq_along(qualities), function(index) {
    data.frame(
      run_id = rep(paste0("run-", qualities[[index]]), 3),
      PPFD_mean = c(0, 100, 500), PPFD_sd = 1,
      A_mean = c(-index, index, 2 * index), A_sd = .1, A_slope = 0,
      coverage = 1, window_complete = TRUE, quality = qualities[[index]],
      species = "oak", plant_id = as.character(index), metadata_status = "Exact ID",
      colour = rep(WALZ_DARK2[[index]], 3), stringsAsFactors = FALSE
    )
  }))

  result <- make_qc_overview_plot(data)
  built <- suppressMessages(plotly::plotly_build(result$widget))
  annotations <- built$x$layout$annotations
  annotation_text <- vapply(annotations, `[[`, character(1), "text")
  annotation_x <- vapply(annotations, `[[`, numeric(1), "x")
  annotation_y <- vapply(annotations, `[[`, numeric(1), "y")

  expect_equal(annotation_text, c(
    "Good — original scale", "Good — normalized",
    "Medium — original scale", "Medium — normalized",
    "Bad — original scale", "Bad — normalized",
    "Unassessed — original scale", "Unassessed — normalized"
  ))
  expect_equal(annotation_x, rep(c(0.23, 0.77), 4))
  expect_equal(annotation_y, rep(c(1, 0.75, 0.5, 0.25), each = 2))

  plotted_traces <- Filter(function(trace) !is.null(trace$name), built$x$data)
  trace_maxima <- vapply(plotted_traces, function(trace) {
    max(as.numeric(unlist(trace$y)), na.rm = TRUE)
  }, numeric(1))
  expect_equal(trace_maxima, as.vector(rbind(2 * seq_along(qualities), rep(1, 4))))
  expect_equal(
    vapply(plotted_traces, `[[`, character(1), "yaxis"),
    c("y", paste0("y", 2:8))
  )
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
