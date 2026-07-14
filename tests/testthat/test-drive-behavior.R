test_that("remote cache keys include file identity and modification time", {
  clear_remote_cache()
  record <- data.frame(
    id = "one",
    modified_iso = "2026-07-13T16:27:42.000Z",
    stringsAsFactors = FALSE
  )
  calls <- 0L
  loader <- function() {
    calls <<- calls + 1L
    "content"
  }

  expect_equal(with_remote_cache("measurement", record, loader), "content")
  expect_equal(with_remote_cache("measurement", record, loader), "content")
  expect_equal(calls, 1L)

  record$modified_iso <- "2026-07-14T09:00:00.000Z"
  expect_equal(with_remote_cache("measurement", record, loader), "content")
  expect_equal(calls, 2L)
})

test_that("temporary loader failures are not cached", {
  clear_remote_cache()
  record <- data.frame(id = "failure", modified_iso = "now")
  calls <- 0L
  loader <- function() {
    calls <<- calls + 1L
    stop("temporary Drive failure")
  }

  expect_error(with_remote_cache("measurement", record, loader), "temporary Drive failure")
  expect_error(with_remote_cache("measurement", record, loader), "temporary Drive failure")
  expect_equal(calls, 2L)
})

test_that("public Drive URL is used when the package download fails", {
  record <- data.frame(id = "public-file-id", stringsAsFactors = FALSE)
  destination <- tempfile()
  on.exit(unlink(destination), add = TRUE)
  direct_url <- NULL

  result <- download_drive_record(
    record,
    destination,
    drive_downloader = function(...) stop("shared API key unavailable"),
    direct_downloader = function(url, destfile, mode, quiet) {
      direct_url <<- url
      writeBin(charToRaw("public content"), destfile)
      0L
    }
  )

  expect_identical(result, destination)
  expect_match(direct_url, "id=public-file-id", fixed = TRUE)
  expect_equal(readLines(destination, warn = FALSE), "public content")
})

test_that("Drive download reports both package and public URL failures", {
  record <- data.frame(id = "unavailable-file-id", stringsAsFactors = FALSE)
  destination <- tempfile()
  on.exit(unlink(destination), add = TRUE)

  expect_error(
    download_drive_record(
      record,
      destination,
      drive_downloader = function(...) stop("package failure"),
      direct_downloader = function(...) stop("public URL failure")
    ),
    "package failure.*public URL failure"
  )
})

test_that("empty folders and deleted selections resolve safely", {
  empty_index <- list(measurements = data.frame(id = character()))
  populated_index <- list(
    measurements = data.frame(
      id = c("one", "two"),
      name = c("one.csv", "two.csv"),
      stringsAsFactors = FALSE
    )
  )

  expect_null(resolve_selected_measurement(empty_index, "one"))
  expect_null(resolve_selected_measurement(populated_index, "deleted-id"))
  expect_null(resolve_selected_measurement(populated_index, NULL))
  expect_equal(resolve_selected_measurement(populated_index, "two")$name, "two.csv")
})
