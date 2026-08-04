test_that("public metadata URL targets the requested visible sheet", {
  url <- metadata_sheet_url("sheet-id", "Walz Measurement Metadata")

  expect_match(url, "/sheet-id/gviz/tq", fixed = TRUE)
  expect_match(url, "tqx=out:csv", fixed = TRUE)
  expect_match(url, "sheet=Walz%20Measurement%20Metadata", fixed = TRUE)
})

test_that("metadata cleaning removes blank rows and columns but preserves headers", {
  raw <- data.frame(
    timestamp = c(" run-one ", "", "run-two"),
    `TREE species` = c("beech", "", "oak"),
    blank = c("", "", ""),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  metadata <- clean_run_metadata(raw)

  expect_equal(metadata$timestamp, c("run-one", "run-two"))
  expect_equal(metadata$`TREE species`, c("beech", "oak"))
  expect_true("blank" %in% names(metadata))
  expect_true(all(metadata$blank == ""))
  expect_equal(metadata$.run_id, c("run-one", "run-two"))
})

test_that("measurement metadata matching is exact apart from case and whitespace", {
  metadata <- clean_run_metadata(data.frame(
    timestamp = c("20260727_0834_B2", "20260727_0834_B20"),
    species = c("beech", "oak"),
    stringsAsFactors = FALSE
  ))

  exact <- match_run_metadata(metadata, "20260727_0834_b2.csv")
  missing <- match_run_metadata(metadata, "20260727_0834_B.csv")

  expect_equal(nrow(exact), 1L)
  expect_equal(exact$species, "beech")
  expect_equal(nrow(missing), 0L)
  expect_equal(measurement_run_id("/tmp/run-one.csv"), "run-one")
})

test_that("full metadata can be sorted newest first by run timestamp", {
  metadata <- clean_run_metadata(data.frame(
    timestamp = c(
      "20260728_0815_B1",
      "manual-note",
      "20260730_1410_O2",
      "20260729_0930_P1",
      "20260730_0900_B2"
    ),
    species = c("beech", "note", "oak", "prunus", "beech"),
    stringsAsFactors = FALSE
  ))

  sorted <- sort_run_metadata_newest(metadata)

  expect_equal(
    sorted$timestamp,
    c(
      "20260730_1410_O2",
      "20260730_0900_B2",
      "20260729_0930_P1",
      "20260728_0815_B1",
      "manual-note"
    )
  )
  expect_equal(rownames(sorted), as.character(seq_len(nrow(sorted))))
})

test_that("run IDs produce deterministic relative and absolute date labels", {
  reference_time <- as.POSIXct(
    "2026-08-03 12:00:00",
    tz = WALZ_TIMEZONE
  )
  run_ids <- c(
    "20260803_1123_B2",
    "20260802_0915_O1",
    "20260801_1844_P1",
    "20260730_0705_B1",
    "20260804_0810_O2",
    "not-a-run-id"
  )

  expect_equal(
    relative_run_date(run_ids, reference_time),
    c(
      "Today (Mon 3 Aug, 11:23)",
      "Yesterday (Sun 2 Aug, 09:15)",
      "Two days ago (Sat 1 Aug, 18:44)",
      "4 days ago (Thu 30 Jul, 07:05)",
      "Tomorrow (Tue 4 Aug, 08:10)",
      NA_character_
    )
  )
  expect_true(is.na(run_datetime_from_id("20260230_1200_B1")))

  display <- add_run_date_display(
    clean_run_metadata(data.frame(
      timestamp = run_ids[1:2],
      species = c("beech", "oak"),
      stringsAsFactors = FALSE
    )),
    reference_time
  )
  expect_equal(
    names(display)[1:3],
    c("timestamp", ".display_date", "species")
  )
  expect_equal(metadata_column_label(".display_date"), "Date")
})

test_that("duplicate exact metadata rows are retained and colors use Dark2", {
  metadata <- clean_run_metadata(data.frame(
    timestamp = c("run-one", "run-one"),
    species = c("beech", "oak"),
    stringsAsFactors = FALSE
  ))

  expect_equal(nrow(match_run_metadata(metadata, "run-one.csv")), 2L)
  expect_equal(run_palette(8), WALZ_DARK2)
  expect_length(unique(run_palette(20)), 20L)
  expect_true(all(stable_run_colour(c("run-one", "run-two")) %in% WALZ_DARK2))
  expect_match(hex_to_rgba(WALZ_DARK2[[1]], 0.1), "^rgba\\(")
})

test_that("metadata loader validates the required run ID column", {
  expect_error(
    clean_run_metadata(data.frame(other = "run-one")),
    "must contain a 'timestamp' column"
  )
  expect_equal(nrow(match_run_metadata(NULL, "run-one.csv")), 0L)
})
