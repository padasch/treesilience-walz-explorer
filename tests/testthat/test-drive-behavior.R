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

test_that("Drive listings expose plain character IDs to background workflows", {
  files <- data.frame(
    id = googledrive::as_id(c("b5-drive-id", "o3-drive-id")),
    name = c("20260806_1243_B5.csv", "20260806_1251_O3.csv"),
    stringsAsFactors = FALSE
  )
  files$drive_resource <- list(
    list(
      id = "b5-drive-id", name = files$name[[1]],
      modifiedTime = "2026-08-06T12:43:00.000Z",
      mimeType = "text/csv", size = "100"
    ),
    list(
      id = "o3-drive-id", name = files$name[[2]],
      modifiedTime = "2026-08-06T12:51:00.000Z",
      mimeType = "text/csv", size = "100"
    )
  )

  result <- drive_metadata_table(files)

  expect_type(result$id, "character")
  expect_false(inherits(result$id, "drive_id"))
  expect_identical(result$id, c("b5-drive-id", "o3-drive-id"))
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

test_that("source listings use a process-wide TTL cache", {
  clear_source_cache()
  expect_null(source_cache_get("measurements::test", ttl_seconds = 60))
  value <- list(measurements = data.frame(id = "one"))
  source_cache_set("measurements::test", value)
  expect_identical(
    source_cache_get("measurements::test", ttl_seconds = 60),
    value
  )
  expect_null(source_cache_get("measurements::test", ttl_seconds = -1))
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
  selected <- resolve_selected_measurements(
    populated_index,
    c("two", "deleted-id", "one")
  )
  expect_equal(selected$id, c("two", "one"))
})
