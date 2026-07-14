test_that("WALZ CSVs are parsed with units and Zurich timestamps", {
  parsed <- read_walz_csv(fixture_path("walz_sample.csv"))

  expect_equal(parsed$row_count, 3L)
  expect_equal(parsed$column_count, 19L)
  expect_equal(parsed$missing_variables, character())
  expect_true(inherits(parsed$data$Datetime, "POSIXct"))
  expect_equal(attr(parsed$data$Datetime, "tzone"), WALZ_TIMEZONE)
  expect_match(parsed$units[["Tleaf"]], "C$")
  expect_type(parsed$data$A, "double")
})

test_that("the parser reports missing plot variables without crashing", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(
    c(
      "Date;Time;Tleaf",
      "yyyy-mm-dd;hh:mm:ss;°C",
      "2026-07-13;15:28:09;21.98"
    ),
    path,
    useBytes = TRUE
  )

  parsed <- read_walz_csv(path)
  expect_true("A" %in% parsed$missing_variables)
  expect_match(parsed$issues, "Missing plotted variable")
})

test_that("malformed clocks and empty data produce explicit errors", {
  missing_time <- tempfile(fileext = ".csv")
  empty_data <- tempfile(fileext = ".csv")
  on.exit(unlink(c(missing_time, empty_data)), add = TRUE)

  writeLines(
    c("Date;A", "yyyy-mm-dd;µmol m-2 s-1", "2026-07-13;1.2"),
    missing_time,
    useBytes = TRUE
  )
  writeLines(c("Date;Time;A", "yyyy-mm-dd;hh:mm:ss;µmol m-2 s-1"), empty_data)

  expect_error(read_walz_csv(missing_time), "missing clock column")
  expect_error(read_walz_csv(empty_data), "contains no data rows")
})

test_that("invalid timestamp rows are surfaced as issues", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(
    c(
      "Date;Time;A",
      "yyyy-mm-dd;hh:mm:ss;µmol m-2 s-1",
      "2026-07-13;15:28:09;1.2",
      "not-a-date;not-a-time;1.3"
    ),
    path,
    useBytes = TRUE
  )

  parsed <- read_walz_csv(path)
  expect_true(any(grepl("invalid timestamps", parsed$issues)))
})
