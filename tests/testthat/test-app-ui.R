test_that("source status, filename heading, and match notice render", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  withr::local_dir(project_root)
  app_environment <- new.env(parent = globalenv())
  source("app.R", local = app_environment)

  parsed <- read_walz_csv(fixture_path("walz_sample.csv"))
  modified <- as.POSIXct("2026-07-13 16:27:42", tz = "UTC")
  fake_index <- list(
    measurements = data.frame(
      id = "measurement-id",
      name = "20260713_1023_chamber_oak(area10)_postblackout.csv",
      modified_time = modified,
      modified_iso = "2026-07-13T16:27:42.000Z",
      mime_type = "text/csv",
      size = 1,
      stringsAsFactors = FALSE
    ),
    protocols = data.frame(
      id = "protocol-id",
      name = "20260713_1023_chamber_oak(10).txt",
      modified_time = modified,
      modified_iso = "2026-07-13T16:27:42.000Z",
      mime_type = "text/plain",
      size = 1,
      stringsAsFactors = FALSE
    ),
    refreshed_at = modified
  )

  app_environment$list_walz_drive <- function(root_id) fake_index
  app_environment$load_remote_measurement <- function(record) parsed
  app_environment$load_remote_protocol <- function(record) "Set CO2 = 440"

  shiny::testServer(app_environment$server, {
    session$flushReact()
    session$setInputs(
      measurement_id = "measurement-id",
      show_grid = FALSE
    )
    session$flushReact()

    expect_match(output$source_status$html, "<dl", fixed = TRUE)
    expect_match(
      output$selected_file_heading$html,
      "20260713_1023_chamber_oak(area10)_postblackout.csv",
      fixed = TRUE
    )
    expect_match(
      output$protocol_panel$html,
      "No fuzzy matching was used",
      fixed = TRUE
    )
    expect_match(output$protocol_panel$html, "Protocol match:", fixed = TRUE)
  })
})
