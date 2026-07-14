test_that("the public Drive source has the validated WALZ structure and files", {
  skip_if(Sys.getenv("RUN_LIVE_DRIVE_TESTS") != "true")
  skip_if_not_installed("googledrive")

  configure_drive_access("")
  root_files <- googledrive::drive_ls(googledrive::as_id(WALZ_DEFAULT_DRIVE_FOLDER_ID))
  folder_names <- root_files$name[vapply(
    root_files$drive_resource,
    function(item) identical(item$mimeType, "application/vnd.google-apps.folder"),
    logical(1)
  )]
  expect_setequal(folder_names, c("measurements", "protocols"))

  index <- list_walz_drive(WALZ_DEFAULT_DRIVE_FOLDER_ID)

  expect_equal(nrow(index$measurements), 5L)
  expect_equal(nrow(index$protocols), 4L)
  expect_equal(
    index$measurements$name[[1]],
    "20260713_1023_chamber_oak(area10)_postblackout.csv"
  )
  expect_false(any(grepl("\\.txt$", index$measurements$name, ignore.case = TRUE)))
  expect_false(any(grepl("lightFlucScript_oak\\(witharea10\\)$", index$measurements$name)))

  for (row in seq_len(nrow(index$measurements))) {
    record <- index$measurements[row, , drop = FALSE]
    parsed <- load_remote_measurement(record)
    expect_equal(parsed$row_count, 537L, info = record$name)
    expect_equal(parsed$column_count, 40L, info = record$name)
    expect_false(anyNA(parsed$data$Datetime), info = record$name)
    expect_equal(parsed$missing_variables, character(), info = record$name)

    matched <- match_protocol(record$name[[1]], index$protocols)
    expect_equal(matched$status, "matched", info = record$name)
  }
})
