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
