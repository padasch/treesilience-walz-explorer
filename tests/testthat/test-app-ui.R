test_that("comparison controls, status, variables, and protocols render", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  withr::local_dir(project_root)
  app_environment <- new.env(parent = globalenv())
  source("app.R", local = app_environment)

  page_html <- htmltools::renderTags(app_environment$ui)$html
  expect_lt(
    regexpr("source_status", page_html, fixed = TRUE)[[1]],
    regexpr("measurement_id", page_html, fixed = TRUE)[[1]]
  )
  expect_match(page_html, "Dew-Point Calculation", fixed = TRUE)
  expect_match(page_html, "dew_humidity_mode", fixed = TRUE)
  expect_match(page_html, "dew_h2o_ppm", fixed = TRUE)
  expect_match(page_html, "dew_relative_humidity", fixed = TRUE)
  expect_match(page_html, "dew_couple_temperatures", fixed = TRUE)
  expect_match(page_html, "dew_safety_buffer", fixed = TRUE)
  expect_match(page_html, "dew_point_audit_plot", fixed = TRUE)

  parsed <- dew_point_fixture()
  modified <- as.POSIXct("2026-07-13 16:27:42", tz = "UTC")
  fake_index <- list(
    measurements = data.frame(
      id = c("primary-id", "overlay-id"),
      name = c(
        "20260713_1023_chamber_oak(area10)_postblackout.csv",
        "20260713_1023_chamber_prunus_area10_postblackout.csv"
      ),
      modified_time = c(modified, modified - 60),
      modified_iso = c(
        "2026-07-13T16:27:42.000Z",
        "2026-07-13T16:26:42.000Z"
      ),
      mime_type = c("text/csv", "text/csv"),
      size = c(1, 1),
      stringsAsFactors = FALSE
    ),
    protocols = data.frame(
      id = c("oak-protocol-id", "prunus-protocol-id"),
      name = c(
        "20260713_1023_chamber_oak(10).txt",
        "20260713_1023_chamber_prunus_area10.txt"
      ),
      modified_time = c(modified, modified),
      modified_iso = c(
        "2026-07-13T16:27:42.000Z",
        "2026-07-13T16:27:42.000Z"
      ),
      mime_type = c("text/plain", "text/plain"),
      size = c(1, 1),
      stringsAsFactors = FALSE
    ),
    refreshed_at = modified
  )

  app_environment$list_walz_drive <- function(root_id) fake_index
  app_environment$load_remote_measurement <- function(record) {
    value <- parsed
    if (record$id[[1]] == "overlay-id") {
      value$data$Datetime <- value$data$Datetime + (24 * 60 * 60)
      value$data$A <- value$data$A + 0.5
    }
    value
  }
  app_environment$load_remote_protocol <- function(record) "Set CO2 = 440"

  shiny::testServer(app_environment$server, {
    session$flushReact()
    session$setInputs(
      measurement_id = "primary-id",
      overlay_enabled = FALSE,
      show_grid = FALSE,
      dew_humidity_mode = "ppm",
      dew_h2o_ppm = 15000,
      dew_relative_humidity = 60,
      dew_tcuv = 22,
      dew_tamb = 20,
      dew_couple_temperatures = FALSE,
      dew_pamb = 100,
      dew_safety_buffer = 2
    )
    session$flushReact()

    expect_match(output$source_status$html, "<dl", fixed = TRUE)
    expect_match(
      output$source_status$html,
      "https://drive.google.com/drive/folders/1wC9zXLEWQe4z7jBxfBfPRiVBuPJiF8vE",
      fixed = TRUE
    )
    expect_match(output$variable_selector$html, "Response parameters", fixed = TRUE)
    expect_match(output$variable_selector$html, "Environmental parameters", fixed = TRUE)
    expect_match(output$variable_selector$html, "Physiological constant", fixed = TRUE)
    expect_match(output$variable_selector$html, "name=\"response_variables\" value=\"GH2O\" checked", fixed = TRUE)
    expect_match(output$variable_selector$html, "value=\"Tcuv\"", fixed = TRUE)
    expect_match(output$variable_selector$html, "name=\"environmental_variables\" value=\"Tamb\" checked", fixed = TRUE)
    expect_match(output$variable_selector$html, "value=\"Tleaf\"", fixed = TRUE)
    expect_match(output$variable_selector$html, "value=\"Area\"", fixed = TRUE)
    expect_match(output$dew_point_results$html, "Calculated dew point", fixed = TRUE)
    expect_match(output$dew_point_results$html, "Recommended minimum ambient", fixed = TRUE)
    expect_match(output$dew_point_results$html, "Ambient margin", fixed = TRUE)
    expect_match(output$dew_point_results$html, "Internal margin", fixed = TRUE)
    expect_match(output$dew_point_results$html, "Ambient relative to cuvette", fixed = TRUE)
    expect_match(output$dew_point_results$html, "Cuvette warmer than ambient", fixed = TRUE)
    expect_match(output$dew_point_results$html, "tube", fixed = TRUE)
    expect_match(
      output$dew_point_audit_heading$html,
      "20260713_1023_chamber_oak(area10)_postblackout.csv",
      fixed = TRUE
    )
    expect_null(dew_point_audit_widget_result()$error)
    expect_s3_class(dew_point_audit_widget_result()$value, "plotly")
    expect_length(dew_point_audit_widget_result()$value$x$data, 4L)
    expect_match(output$dew_point_audit_alert$html, "Tcuv exceeded Tamb", fixed = TRUE)

    session$setInputs(dew_couple_temperatures = TRUE)
    session$flushReact()
    expect_match(
      output$dew_point_results$html,
      "Temperature conditions clear both checks",
      fixed = TRUE
    )

    variable_html <- output$variable_selector$html
    expect_lt(
      regexpr("Response parameters", variable_html, fixed = TRUE)[[1]],
      regexpr("Environmental parameters", variable_html, fixed = TRUE)[[1]]
    )
    expect_lt(
      regexpr("value=\"A\"", variable_html, fixed = TRUE)[[1]],
      regexpr("value=\"GH2O\"", variable_html, fixed = TRUE)[[1]]
    )
    expect_lt(
      regexpr("value=\"GH2O\"", variable_html, fixed = TRUE)[[1]],
      regexpr("value=\"E\"", variable_html, fixed = TRUE)[[1]]
    )
    expect_lt(
      regexpr("Environmental parameters", variable_html, fixed = TRUE)[[1]],
      regexpr("value=\"Area\"", variable_html, fixed = TRUE)[[1]]
    )

    session$setInputs(
      overlay_enabled = TRUE,
      comparison_id = "overlay-id",
      response_variables = c("A", "GH2O"),
      environmental_variables = c("Tcuv", "PARtop"),
      constant_variables = character(),
      show_grid = TRUE
    )
    session$flushReact()

    expect_match(output$source_status$html, "Overlay upload modified", fixed = TRUE)
    expect_match(
      output$selected_file_heading$html,
      "20260713_1023_chamber_oak(area10)_postblackout.csv",
      fixed = TRUE
    )
    expect_match(
      output$selected_file_heading$html,
      "20260713_1023_chamber_prunus_area10_postblackout.csv",
      fixed = TRUE
    )
    expect_match(output$timeseries_alerts$html, "elapsed minute zero", fixed = TRUE)
    expect_match(output$protocol_panel$html, "Primary measurement protocol", fixed = TRUE)
    expect_match(output$protocol_panel$html, "Overlay measurement protocol", fixed = TRUE)
    expect_match(output$protocol_panel$html, "No fuzzy matching was used", fixed = TRUE)
    expect_null(timeseries_widget_result()$error)
    expect_s3_class(timeseries_widget_result()$value, "plotly")
    expect_null(state_widget_result()$error)
    expect_s3_class(state_widget_result()$value, "plotly")
    expect_length(dew_point_audit_widget_result()$value$x$data, 4L)
  })
})

test_that("missing audit columns render inline without disabling the planner", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  withr::local_dir(project_root)
  app_environment <- new.env(parent = globalenv())
  source("app.R", local = app_environment)

  parsed <- read_walz_csv(fixture_path("walz_sample.csv"))
  modified <- as.POSIXct("2026-07-13 16:27:42", tz = "UTC")
  fake_index <- list(
    measurements = data.frame(
      id = "primary-id",
      name = "measurement-without-dew-columns.csv",
      modified_time = modified,
      modified_iso = "2026-07-13T16:27:42.000Z",
      mime_type = "text/csv",
      size = 1,
      stringsAsFactors = FALSE
    ),
    protocols = data.frame(
      id = character(),
      name = character(),
      modified_time = as.POSIXct(character(), tz = "UTC"),
      modified_iso = character(),
      mime_type = character(),
      size = numeric(),
      stringsAsFactors = FALSE
    ),
    refreshed_at = modified
  )

  app_environment$list_walz_drive <- function(root_id) fake_index
  app_environment$load_remote_measurement <- function(record) parsed

  shiny::testServer(app_environment$server, {
    session$setInputs(
      measurement_id = "primary-id",
      overlay_enabled = FALSE,
      dew_humidity_mode = "ppm",
      dew_h2o_ppm = 15000,
      dew_relative_humidity = 60,
      dew_tcuv = 22,
      dew_tamb = 20,
      dew_couple_temperatures = FALSE,
      dew_pamb = 100,
      dew_safety_buffer = 2
    )
    session$flushReact()

    expect_match(
      output$dew_point_audit_alert$html,
      "missing required dew-point column(s): wa, Pamb",
      fixed = TRUE
    )
    expect_match(output$dew_point_results$html, "Calculated dew point", fixed = TRUE)
  })
})
