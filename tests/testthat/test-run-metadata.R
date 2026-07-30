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
  expect_false("blank" %in% names(metadata))
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

test_that("duplicate exact metadata rows are retained and colors are distinct", {
  metadata <- clean_run_metadata(data.frame(
    timestamp = c("run-one", "run-one"),
    species = c("beech", "oak"),
    stringsAsFactors = FALSE
  ))

  expect_equal(nrow(match_run_metadata(metadata, "run-one.csv")), 2L)
  expect_length(unique(run_palette(20)), 20L)
  expect_equal(length(run_line_types(12)), 12L)
  expect_match(hex_to_rgba("#28754D", 0.1), "^rgba\\(")
})

test_that("metadata loader validates the required run ID column", {
  expect_error(
    clean_run_metadata(data.frame(other = "run-one")),
    "must contain a 'timestamp' column"
  )
  expect_equal(nrow(match_run_metadata(NULL, "run-one.csv")), 0L)
})
