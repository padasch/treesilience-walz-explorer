fake_sheet_client <- function(initial) {
  state <- new.env(parent = emptyenv())
  state$data <- initial
  state$writes <- list()
  list(
    state = state,
    client = list(
      read = function() state$data,
      write = function(cell, value) {
        state$writes[[length(state$writes) + 1L]] <- list(cell = cell, value = value)
        column <- match(WALZ_QUALITY_COLUMN, names(state$data))
        row <- as.integer(sub("^[A-Z]+", "", cell)) - 1L
        state$data[[column]][[row]] <- value
      }
    )
  )
}

test_that("Sheet write-back only requires the service-account credential", {
  expect_true(sheet_writeback_configured(list(service_account_json_b64 = "configured")))
  expect_false(sheet_writeback_configured(list(service_account_json_b64 = "")))
})

test_that("quality write-back changes exactly one quality cell and verifies it", {
  fake <- fake_sheet_client(data.frame(
    timestamp = c("run-a", "run-b"),
    notes = c("keep", "keep"),
    `quality assessment` = c("", "medium"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  result <- write_quality_assessment(
    "run-a", "good", "unassessed", fake$client
  )
  expect_equal(length(fake$state$writes), 1L)
  expect_equal(fake$state$writes[[1]]$cell, "C2")
  expect_equal(fake$state$writes[[1]]$value, "good")
  expect_equal(fake$state$data$notes, c("keep", "keep"))
  expect_equal(result$quality, "good")
})
test_that("quality write-back rejects conflicts, duplicate rows, and invalid values", {
  conflict <- fake_sheet_client(data.frame(
    timestamp = "run-a", `quality assessment` = "bad",
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  expect_error(
    write_quality_assessment("run-a", "good", "unassessed", conflict$client),
    "changed in Google Sheets"
  )
  duplicate <- fake_sheet_client(data.frame(
    timestamp = c("run-a", "run-a"), `quality assessment` = c("", ""),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  expect_error(
    write_quality_assessment("run-a", "good", "unassessed", duplicate$client),
    "exactly one exact timestamp"
  )
  expect_error(
    write_quality_assessment("run-a", "excellent", "unassessed", conflict$client),
    "Quality must be"
  )
})

test_that("Sheet column letters cover columns beyond Z", {
  expect_equal(sheet_column_letter(c(1L)), "A")
  expect_equal(sheet_column_letter(26L), "Z")
  expect_equal(sheet_column_letter(27L), "AA")
  expect_equal(sheet_column_letter(52L), "AZ")
})
