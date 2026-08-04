test_that("multi-run controls, metadata, plots, and protocols stay synchronized", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  withr::local_dir(project_root)
  withr::local_envvar(WALZ_ENABLE_DEW_POINT_TAB = "false")
  app_environment <- new.env(parent = globalenv())
  source("app.R", local = app_environment)

  page_html <- htmltools::renderTags(app_environment$ui)$html
  expect_gt(
    regexpr("source_status", page_html, fixed = TRUE)[[1]],
    regexpr("measurement_ids", page_html, fixed = TRUE)[[1]]
  )
  expect_gt(
    regexpr("source_status", page_html, fixed = TRUE)[[1]],
    regexpr("refresh_latest", page_html, fixed = TRUE)[[1]]
  )
  expect_match(page_html, "measurement_ids", fixed = TRUE)
  expect_match(page_html, "multiple", fixed = TRUE)
  expect_match(page_html, "\"closeAfterSelect\":false", fixed = TRUE)
  expect_match(page_html, "\"hideSelected\":true", fixed = TRUE)
  expect_match(page_html, "time_axis_mode", fixed = TRUE)
  expect_match(page_html, "value=\"elapsed\" checked", fixed = TRUE)
  expect_match(page_html, "one_column_plots", fixed = TRUE)
  expect_match(page_html, "One-column graph view", fixed = TRUE)
  expect_match(page_html, "timeseries_plot_container", fixed = TRUE)
  expect_match(page_html, "state_plot_container", fixed = TRUE)
  expect_match(page_html, "run_metadata_panel", fixed = TRUE)
  expect_match(page_html, "full_metadata_panel", fixed = TRUE)
  expect_match(page_html, "detail_accordion", fixed = TRUE)
  expect_match(page_html, "metadata_details", fixed = TRUE)
  expect_match(page_html, "protocol_details", fixed = TRUE)
  expect_gt(
    regexpr("detail_accordion", page_html, fixed = TRUE)[[1]],
    regexpr("plot_view", page_html, fixed = TRUE)[[1]]
  )
  expect_gt(
    regexpr("full_metadata_panel", page_html, fixed = TRUE)[[1]],
    regexpr("run_metadata_panel", page_html, fixed = TRUE)[[1]]
  )
  expect_gt(
    regexpr("protocol_panel", page_html, fixed = TRUE)[[1]],
    regexpr("full_metadata_panel", page_html, fixed = TRUE)[[1]]
  )
  expect_false(grepl("overlay_enabled", page_html, fixed = TRUE))
  expect_false(grepl("comparison_id", page_html, fixed = TRUE))
  expect_false(grepl("show_grid", page_html, fixed = TRUE))
  expect_false(grepl("15-minute time grid", page_html, fixed = TRUE))
  expect_false(grepl("Dew-Point Calculation", page_html, fixed = TRUE))
  expect_match(page_html, "Quality Control", fixed = TRUE)
  expect_match(page_html, "Response Analysis", fixed = TRUE)
  expect_match(page_html, "qc-species", fixed = TRUE)
  expect_match(page_html, "analysis-species", fixed = TRUE)
  expect_match(page_html, "value=\"good\" checked", fixed = TRUE)

  parsed <- dew_point_fixture()
  modified <- as.POSIXct("2026-07-27 16:27:42", tz = "UTC")
  fake_index <- list(
    measurements = data.frame(
      id = c("run-one-id", "run-two-id", "run-three-id"),
      name = c("run-one.csv", "run-two.csv", "run-three.csv"),
      modified_time = c(modified, modified - 60, modified - 120),
      modified_iso = c(
        "2026-07-27T16:27:42.000Z",
        "2026-07-27T16:26:42.000Z",
        "2026-07-27T16:25:42.000Z"
      ),
      mime_type = rep("text/csv", 3),
      size = rep(1, 3),
      stringsAsFactors = FALSE
    ),
    protocols = data.frame(
      id = c("protocol-one", "protocol-two", "protocol-three"),
      name = c("run-one.txt", "run-two.txt", "run-three.txt"),
      modified_time = rep(modified, 3),
      modified_iso = rep("2026-07-27T16:27:42.000Z", 3),
      mime_type = rep("text/plain", 3),
      size = rep(1, 3),
      stringsAsFactors = FALSE
    ),
    refreshed_at = modified
  )
  fake_metadata <- clean_run_metadata(data.frame(
    timestamp = c("run-one", "run-two"),
    `TREE species` = c("beech", "oak"),
    `plant id` = c("B1", "O1"),
    `WALZ walz number` = c("1", "2"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  ))

  app_environment$list_measurement_drive <- function(...) {
    c(fake_index[c("measurements", "refreshed_at")], list(measurements_id = "measurements"))
  }
  app_environment$list_protocol_drive <- function(...) {
    c(fake_index[c("protocols", "refreshed_at")], list(protocols_id = "protocols"))
  }
  app_environment$load_cached_run_metadata <- function(...) {
    fake_metadata
  }
  app_environment$load_remote_measurement <- function(record) {
    value <- parsed
    offset <- match(record$id[[1]], fake_index$measurements$id) - 1L
    value$data$Datetime <- value$data$Datetime + (offset * 24 * 60 * 60)
    value$data$A <- value$data$A + offset
    value
  }
  app_environment$load_remote_protocol <- function(record) "Set CO2 = 440"

  shiny::testServer(app_environment$server, {
    session$setInputs(detail_accordion = "metadata_details")
    session$flushReact()
    session$setInputs(
      measurement_ids = c("run-one-id", "run-two-id"),
      detail_accordion = c("metadata_details", "protocol_details")
    )
    session$flushReact()
    expect_true(all(
      c("A", "GH2O", "White x T") %in% selected_variables()
    ))

    session$setInputs(
      measurement_ids = c("run-one-id", "run-two-id"),
      response_variables = c("A", "GH2O"),
      environmental_variables = c("Tcuv", "Tamb", "PARtop"),
      constant_variables = character(),
      time_axis_mode = "elapsed"
    )
    session$flushReact()
    expect_setequal(
      selected_variables(),
      c("A", "GH2O", "Tcuv", "Tamb", "PARtop")
    )

    session$setInputs(
      response_variables = "E",
      environmental_variables = "Tcuv",
      constant_variables = character()
    )
    session$flushReact()
    expect_setequal(selected_variables(), c("E", "Tcuv"))

    session$setInputs(
      response_variables = c("A", "GH2O"),
      environmental_variables = c("Tcuv", "Tamb", "PARtop"),
      constant_variables = character()
    )
    session$flushReact()

    expect_match(output$source_status$html, "Source status", fixed = TRUE)
    expect_match(
      output$source_status$html,
      "https://docs.google.com/spreadsheets/d/1BlUdEIKP-iEJICpzTF8NQG4nyEVBv_7Vgywc7KQqAX0/edit",
      fixed = TRUE
    )
    expect_match(output$source_status$html, "Metadata rows", fixed = TRUE)
    full_metadata_html <- output$full_metadata_panel$html
    expect_match(
      full_metadata_html,
      "All run metadata (read-only)",
      fixed = TRUE
    )
    expect_match(
      full_metadata_html,
      "2 metadata rows loaded from the public CSV",
      fixed = TRUE
    )
    expect_match(full_metadata_html, "Newest run IDs are shown first.", fixed = TRUE)
    expect_match(full_metadata_html, "<table", fixed = TRUE)
    expect_match(full_metadata_html, "run-date-cell", fixed = TRUE)
    expect_gt(
      regexpr(">Date</th>", full_metadata_html, fixed = TRUE)[[1]],
      regexpr(">Run ID</th>", full_metadata_html, fixed = TRUE)[[1]]
    )
    expect_match(full_metadata_html, "beech", fixed = TRUE)
    expect_match(full_metadata_html, "oak", fixed = TRUE)
    expect_false(grepl("<input", full_metadata_html, fixed = TRUE))
    expect_false(grepl("<textarea", full_metadata_html, fixed = TRUE))
    expect_false(grepl("contenteditable", full_metadata_html, fixed = TRUE))
    expect_match(output$variable_selector$html, "Response parameters", fixed = TRUE)
    expect_match(output$variable_selector$html, "Environmental parameters", fixed = TRUE)
    expect_match(output$variable_selector$html, "Physiological constant", fixed = TRUE)
    expect_match(
      output$variable_selector$html,
      "name=\"response_variables\" value=\"GH2O\" checked",
      fixed = TRUE
    )
    expect_match(
      output$variable_selector$html,
      "name=\"environmental_variables\" value=\"Tamb\" checked",
      fixed = TRUE
    )

    metadata_html <- output$run_metadata_panel$html
    expect_match(metadata_html, "run-one.csv", fixed = TRUE)
    expect_match(metadata_html, "run-two.csv", fixed = TRUE)
    expect_match(metadata_html, "beech", fixed = TRUE)
    expect_match(metadata_html, "oak", fixed = TRUE)
    expect_match(metadata_html, "Exact ID", fixed = TRUE)
    expect_match(metadata_html, "run-date-cell", fixed = TRUE)
    expect_gt(
      regexpr(">Date</th>", metadata_html, fixed = TRUE)[[1]],
      regexpr(">Run ID</th>", metadata_html, fixed = TRUE)[[1]]
    )
    expect_match(
      metadata_html,
      "filename stem = sheet Run ID",
      fixed = TRUE
    )
    expect_gte(
      lengths(regmatches(
        metadata_html,
        gregexpr("background-color:", metadata_html, fixed = TRUE)
      )),
      2L
    )

    expect_match(
      output$timeseries_alerts$html,
      "every run starts at elapsed minute zero",
      fixed = TRUE
    )
    expect_null(timeseries_widget_result()$error)
    expect_s3_class(timeseries_widget_result()$value, "plotly")
    expect_null(state_widget_result()$error)
    expect_s3_class(state_widget_result()$value, "plotly")
    expect_equal(plot_columns(), 2L)
    expect_match(
      output$timeseries_plot_container$html,
      "height:900px",
      fixed = TRUE
    )
    expect_match(output$state_plot_container$html, "height:850px", fixed = TRUE)

    built <- plotly::plotly_build(timeseries_widget_result()$value)
    axis_names <- grep("^[xy]axis[0-9]*$", names(built$x$layout), value = TRUE)
    expect_true(length(axis_names) > 0L)
    expect_true(all(vapply(
      built$x$layout[axis_names],
      function(axis) isTRUE(axis$showspikes),
      logical(1)
    )))
    elapsed_annotations <- vapply(
      built$x$layout$annotations,
      function(annotation) {
        if (is.null(annotation$text)) "" else annotation$text
      },
      character(1)
    )
    expect_true(any(grepl(
      "Elapsed time from run start",
      elapsed_annotations,
      fixed = TRUE
    )))

    session$setInputs(time_axis_mode = "local")
    session$flushReact()
    expect_match(
      output$timeseries_alerts$html,
      "each run retains its original Europe/Zurich timestamp",
      fixed = TRUE
    )
    expect_null(timeseries_widget_result()$error)
    local_built <- plotly::plotly_build(timeseries_widget_result()$value)
    local_annotations <- vapply(
      local_built$x$layout$annotations,
      function(annotation) {
        if (is.null(annotation$text)) "" else annotation$text
      },
      character(1)
    )
    expect_true(any(grepl(
      "Local time (Europe/Zurich)",
      local_annotations,
      fixed = TRUE
    )))

    session$setInputs(one_column_plots = TRUE)
    session$flushReact()
    expect_equal(plot_columns(), 1L)
    expect_match(
      output$timeseries_plot_container$html,
      "height:1050px",
      fixed = TRUE
    )
    expect_match(output$state_plot_container$html, "height:870px", fixed = TRUE)
    one_column_built <- plotly::plotly_build(timeseries_widget_result()$value)
    one_column_x_axes <- grep(
      "^xaxis[0-9]*$",
      names(one_column_built$x$layout),
      value = TRUE
    )
    expect_length(one_column_x_axes, 1L)

    expect_match(output$protocol_panel$html, "Run 1 measurement protocol", fixed = TRUE)
    expect_match(output$protocol_panel$html, "Run 2 measurement protocol", fixed = TRUE)
    expect_match(output$protocol_panel$html, "No fuzzy matching was used", fixed = TRUE)

    colors <- unname(selected_run_context()$colors)
    expect_length(colors, 2L)
    expect_length(unique(colors), 2L)

    session$setInputs(measurement_ids = c("run-one-id", "run-three-id"))
    session$flushReact()
    expect_match(
      output$run_metadata_panel$html,
      "No exact metadata row was found for: run-three",
      fixed = TRUE
    )
    expect_match(
      output$run_metadata_panel$html,
      "No fuzzy or timestamp-only guess was used",
      fixed = TRUE
    )
  })
})

test_that("the variable selector is always a two-column grid", {
  stylesheet <- paste(
    readLines(file.path(project_root, "www", "styles.css"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    stylesheet,
    "grid-template-columns: minmax(0, 1fr) minmax(0, 1fr) !important;",
    fixed = TRUE
  )
  expect_match(
    stylesheet,
    "grid-auto-flow: row !important;",
    fixed = TRUE
  )
  expect_match(stylesheet, "max-height: none !important;", fixed = TRUE)
  expect_false(grepl("column-count: 2", stylesheet, fixed = TRUE))
  expect_false(grepl("max-height: 25rem", stylesheet, fixed = TRUE))
  expect_equal(
    lengths(regmatches(
      stylesheet,
      gregexpr(
        ".variable-selector .shiny-options-group",
        stylesheet,
        fixed = TRUE
      )
    )),
    1L
  )
})

test_that("dew-point tab requires explicit development opt-in", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  withr::local_dir(project_root)
  withr::local_envvar(WALZ_ENABLE_DEW_POINT_TAB = "true")
  app_environment <- new.env(parent = globalenv())
  source("app.R", local = app_environment)

  page_html <- htmltools::renderTags(app_environment$ui)$html
  expect_match(page_html, "Dew-Point Calculation", fixed = TRUE)
  expect_match(page_html, "dew_water_mode", fixed = TRUE)
  expect_match(page_html, "dew_point_plan_plot", fixed = TRUE)
  expect_match(page_html, "dew_point_audit_plot", fixed = TRUE)
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
  fake_metadata <- clean_run_metadata(data.frame(
    timestamp = "measurement-without-dew-columns",
    stringsAsFactors = FALSE
  ))

  app_environment$list_measurement_drive <- function(...) {
    c(fake_index[c("measurements", "refreshed_at")], list(measurements_id = "measurements"))
  }
  app_environment$list_protocol_drive <- function(...) {
    c(fake_index[c("protocols", "refreshed_at")], list(protocols_id = "protocols"))
  }
  app_environment$load_cached_run_metadata <- function(...) {
    fake_metadata
  }
  app_environment$load_remote_measurement <- function(record) parsed

  shiny::testServer(app_environment$server, {
    session$setInputs(
      measurement_ids = "primary-id",
      dew_water_mode = "outlet",
      dew_outlet_h2o_ppm = 15000,
      dew_inlet_h2o_ppm = 15000,
      dew_leaf_h2o_added_ppm = 2000,
      dew_tcuv = 22,
      dew_tamb = 20,
      dew_pamb = 100,
      dew_safety_buffer = 2
    )
    session$flushReact()

    expect_match(
      output$dew_point_audit_alert$html,
      "missing required dew-point column(s): wa, Pamb",
      fixed = TRUE
    )
    expect_null(dew_point_plan_widget_result()$error)
    expect_s3_class(dew_point_plan_widget_result()$value, "plotly")
  })
})
