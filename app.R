required_packages <- c("shiny", "plotly", "ggplot2", "bslib", "googledrive")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    sprintf(
      "Install required package(s) before starting the app: %s",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

source("R/config.R", local = TRUE)
source("R/walz_parser.R", local = TRUE)
source("R/dew_point.R", local = TRUE)
source("R/protocol_match.R", local = TRUE)
source("R/run_metadata.R", local = TRUE)
source("R/drive_data.R", local = TRUE)
source("R/plots.R", local = TRUE)

config <- walz_config()
configure_drive_access(config$api_key)

drive_folder_url <- sprintf(
  "https://drive.google.com/drive/folders/%s",
  config$drive_folder_id
)
metadata_sheet_web_url <- sprintf(
  "https://docs.google.com/spreadsheets/d/%s/edit",
  config$metadata_sheet_id
)

drive_status_link_ui <- function() {
  shiny::a(
    "Open Google Drive folder",
    href = drive_folder_url,
    target = "_blank",
    rel = "noopener noreferrer",
    class = "drive-link drive-status-link"
  )
}

metadata_sheet_link_ui <- function(class = "drive-link") {
  shiny::a(
    "Open run metadata sheet",
    href = metadata_sheet_web_url,
    target = "_blank",
    rel = "noopener noreferrer",
    class = class
  )
}

alert_ui <- function(message, level = c("warning", "danger", "info")) {
  level <- match.arg(level)
  icon <- switch(
    level,
    warning = shiny::icon("triangle-exclamation"),
    danger = shiny::icon("circle-exclamation"),
    info = shiny::icon("circle-info")
  )
  shiny::div(
    class = paste("walz-alert", paste0("walz-alert-", level)),
    icon,
    shiny::span(message)
  )
}

protocol_card_ui <- function(protocol, role, measurement_name, colour = NULL) {
  header <- sprintf("%s measurement protocol", role)
  card_style <- if (is.null(colour)) {
    NULL
  } else {
    sprintf("border-left: 0.32rem solid %s;", colour)
  }
  source_line <- shiny::p(
    class = "protocol-measurement-source",
    shiny::strong("Measurement: "),
    measurement_name
  )

  if (is.null(protocol$match)) {
    return(bslib::card(
      class = "protocol-card",
      style = card_style,
      bslib::card_header(header),
      source_line,
      alert_ui("No measurement is currently selected.", "warning")
    ))
  }

  if (protocol$match$status != "matched") {
    return(bslib::card(
      class = "protocol-card",
      style = card_style,
      bslib::card_header(header),
      source_line,
      alert_ui(protocol$match$message, "warning")
    ))
  }

  filename <- protocol$match$protocol$name[[1]]
  if (!is.null(protocol$error)) {
    return(bslib::card(
      class = "protocol-card",
      style = card_style,
      bslib::card_header(header),
      source_line,
      shiny::p(class = "protocol-filename", filename),
      alert_ui(
        paste(
          "The protocol matched, but its content could not be downloaded:",
          protocol$error
        ),
        "danger"
      )
    ))
  }

  bslib::card(
    class = "protocol-card",
    style = card_style,
    bslib::card_header(header),
    source_line,
    shiny::div(
      class = "protocol-heading",
      shiny::span(class = "protocol-filename", filename),
      shiny::span(class = "match-method", protocol$match$method)
    ),
    alert_ui(protocol$match$message, "info"),
    shiny::tags$pre(class = "protocol-content", protocol$text)
  )
}

ui <- bslib::page_sidebar(
  title = shiny::tagList(
    shiny::span(class = "app-kicker", "TREESILIENCE"),
    shiny::span("WALZ explorer")
  ),
  theme = bslib::bs_theme(
    version = 5,
    bg = "#f7f8f4",
    fg = "#243228",
    primary = "#28754d",
    secondary = "#6c8e75",
    base_font = "system-ui"
  ),
  fillable = FALSE,
  sidebar = bslib::sidebar(
    width = 510,
    shiny::p(
      class = "sidebar-intro",
      "Choose one or more WALZ runs and how to display time."
    ),
    shiny::selectizeInput(
      "measurement_ids",
      "Measurement runs",
      choices = character(),
      selected = character(),
      multiple = TRUE,
      options = list(
        plugins = list("remove_button"),
        closeAfterSelect = TRUE,
        placeholder = "Select one or more measurement runs"
      )
    ),
    shiny::p(
      class = "control-help",
      "Add as many runs as needed. Remove a run with the × on its selection chip."
    ),
    shiny::radioButtons(
      "time_axis_mode",
      "Timeseries x axis",
      choices = c(
        "Elapsed time" = "elapsed",
        "Local time" = "local"
      ),
      selected = "elapsed",
      inline = TRUE
    ),
    shiny::uiOutput("variable_selector"),
    shiny::actionButton(
      "refresh_latest",
      "Refresh and show latest",
      icon = shiny::icon("rotate")
    ),
    shiny::hr(),
    shiny::uiOutput("source_status")
  ),
  shiny::includeCSS("www/styles.css"),
  shiny::div(
    class = "app-introduction",
    shiny::h2("Explore gas-exchange runs without code"),
    shiny::p(
      "Zoom, pan, use the cursor crosshair, or draw directly on the plots. ",
      "Selected variables control both the timeseries and A-versus-state views."
    )
  ),
  shiny::uiOutput("run_metadata_panel"),
  bslib::navset_card_tab(
    id = "plot_view",
    title = NULL,
    bslib::nav_panel(
      "Variables over time",
      shiny::uiOutput("timeseries_alerts"),
      plotly::plotlyOutput("timeseries_plot", height = "900px"),
      shiny::uiOutput("protocol_panel")
    ),
    bslib::nav_panel(
      "A vs state",
      shiny::uiOutput("state_alerts"),
      plotly::plotlyOutput("state_plot", height = "850px")
    ),
    if (isTRUE(config$enable_dew_point_tab)) {
      bslib::nav_panel(
        "Dew-Point Calculation",
        shiny::div(
        class = "dew-point-tab",
        shiny::div(
          class = "dew-point-introduction",
          shiny::h3("Plan ambient conditions before measuring"),
          shiny::p(
            "Estimate the wettest air leaving the cuvette, then compare the dew-point threshold with both possible cold locations: the cuvette interior and the downstream tubes or instrument environment."
          ),
          shiny::p(
            class = "dew-reference-note",
            "Tcuv - 2°C is the GFS-3000 manual's estimate of the coldest internal cuvette location during strong cooling. Tamb is used as a proxy for the coldest tube or instrument environment. The two checks are independent; Tcuv being warmer than Tamb does not itself mean that dew point has been reached."
          )
        ),
        shiny::div(
          class = "dew-planner-grid",
          bslib::card(
            class = "dew-input-card",
            bslib::card_header("Planning inputs"),
            shiny::radioButtons(
              "dew_water_mode",
              "Water-vapour estimate",
              choices = c(
                "Expected chamber/outlet wa" = "outlet",
                "Inlet H2O + expected leaf addition" = "inlet_plus"
              ),
              selected = "outlet"
            ),
            shiny::conditionalPanel(
              condition = "input.dew_water_mode === 'outlet'",
              shiny::sliderInput(
                "dew_outlet_h2o_ppm",
                "Expected chamber/outlet H2O (wa)",
                min = 100,
                max = 75000,
                value = 17000,
                step = 100,
                post = " ppm",
                sep = ""
              ),
              shiny::p(
                class = "control-help",
                "Recommended when a previous comparable run provides a realistic maximum wa."
              )
            ),
            shiny::conditionalPanel(
              condition = "input.dew_water_mode === 'inlet_plus'",
              shiny::sliderInput(
                "dew_inlet_h2o_ppm",
                "Controlled inlet H2O",
                min = 100,
                max = 75000,
                value = 15000,
                step = 100,
                post = " ppm",
                sep = ""
              ),
              shiny::sliderInput(
                "dew_leaf_h2o_added_ppm",
                "Expected H2O added by the leaf",
                min = 0,
                max = 30000,
                value = 2000,
                step = 100,
                post = " ppm",
                sep = ""
              ),
              shiny::p(
                class = "control-help",
                "The calculator adds both values to estimate chamber/outlet wa. Leaf transpiration makes the inlet setpoint alone non-conservative."
              )
            ),
            shiny::sliderInput(
              "dew_pamb",
              "Ambient pressure (Pamb)",
              min = 60,
              max = 110,
              value = 101.3,
              step = 0.1,
              post = " kPa"
            ),
            shiny::sliderInput(
              "dew_tcuv",
              "Cuvette temperature (Tcuv)",
              min = -10,
              max = 50,
              value = 22,
              step = 0.1,
              post = "°C"
            ),
            shiny::sliderInput(
              "dew_tamb",
              "Coldest expected tube/environment temperature (Tamb proxy)",
              min = -10,
              max = 55,
              value = 20,
              step = 0.1,
              post = "°C"
            ),
            shiny::sliderInput(
              "dew_safety_buffer",
              "Required clearance above dew point",
              min = 0,
              max = 5,
              value = 2,
              step = 0.5,
              post = "°C"
            ),
            shiny::actionButton(
              "load_dew_from_run",
              "Load conservative values from primary run",
              icon = shiny::icon("arrow-down")
            )
          ),
          bslib::card(
            class = "dew-results-card",
            bslib::card_header("Planned temperature comparison"),
            shiny::uiOutput("dew_point_results"),
            plotly::plotlyOutput("dew_point_plan_plot", height = "520px")
          )
        ),
        bslib::card(
          class = "dew-audit-card",
          bslib::card_header("Recorded primary run"),
          shiny::uiOutput("dew_point_audit_heading"),
          shiny::uiOutput("dew_point_audit_alert"),
          plotly::plotlyOutput("dew_point_audit_plot", height = "650px")
        )
        )
      )
    }
  )
)

server <- function(input, output, session) {
  drive_index <- shiny::reactiveVal(NULL)
  source_error <- shiny::reactiveVal(NULL)
  run_metadata <- shiny::reactiveVal(NULL)
  metadata_error <- shiny::reactiveVal(NULL)

  refresh_sources <- function() {
    source_error(NULL)
    metadata_error(NULL)

    drive_result <- tryCatch(
      list_walz_drive(config$drive_folder_id),
      error = function(error) error
    )

    if (inherits(drive_result, "error")) {
      source_error(conditionMessage(drive_result))
    } else {
      drive_index(drive_result)
      choices <- stats::setNames(
        drive_result$measurements$id,
        drive_result$measurements$name
      )
      selected <- if (length(choices) > 0L) {
        unname(choices[[1]])
      } else {
        character()
      }
      shiny::updateSelectizeInput(
        session,
        "measurement_ids",
        choices = choices,
        selected = selected,
        server = TRUE
      )
    }

    metadata_result <- tryCatch(
      load_public_run_metadata(
        config$metadata_sheet_id,
        config$metadata_sheet_name
      ),
      error = function(error) error
    )
    if (inherits(metadata_result, "error")) {
      metadata_error(conditionMessage(metadata_result))
    } else {
      run_metadata(metadata_result)
    }

    invisible(!inherits(drive_result, "error"))
  }

  shiny::observeEvent(TRUE, refresh_sources(), once = TRUE)
  shiny::observeEvent(input$refresh_latest, refresh_sources(), ignoreInit = TRUE)

  selected_records <- shiny::reactive({
    resolve_selected_measurements(drive_index(), input$measurement_ids)
  })

  selected_run_context <- shiny::reactive({
    records <- selected_records()
    if (is.null(records) || nrow(records) == 0L) {
      return(list(
        records = NULL,
        labels = character(),
        colors = character()
      ))
    }
    labels <- make.unique(records$name)
    colors <- stats::setNames(run_palette(nrow(records)), labels)
    list(records = records, labels = labels, colors = colors)
  })

  selected_record <- shiny::reactive({
    records <- selected_records()
    if (is.null(records) || nrow(records) == 0L) {
      return(NULL)
    }
    records[1, , drop = FALSE]
  })

  load_measurement_result <- function(record, missing_message) {
    if (is.null(record)) {
      return(list(value = NULL, error = missing_message))
    }
    tryCatch(
      list(value = load_remote_measurement(record), error = NULL),
      error = function(error) list(value = NULL, error = conditionMessage(error))
    )
  }

  measurement_results <- shiny::reactive({
    context <- selected_run_context()
    records <- context$records
    if (is.null(records) || nrow(records) == 0L) {
      return(list())
    }

    lapply(seq_len(nrow(records)), function(index) {
      result <- load_measurement_result(
        records[index, , drop = FALSE],
        "The selected measurement run is no longer available."
      )
      c(
        list(
          record = records[index, , drop = FALSE],
          label = context$labels[[index]],
          colour = unname(context$colors[[index]])
        ),
        result
      )
    })
  })

  valid_measurement_entries <- shiny::reactive({
    Filter(
      function(entry) is.null(entry$error) && !is.null(entry$value),
      measurement_results()
    )
  })

  measurement_result <- shiny::reactive({
    results <- measurement_results()
    if (length(results) == 0L) {
      return(list(
        value = NULL,
        error = "No primary measurement is currently selected."
      ))
    }
    list(value = results[[1]]$value, error = results[[1]]$error)
  })

  shiny::observeEvent(input$load_dew_from_run, {
    primary <- measurement_result()
    if (!is.null(primary$error) || is.null(primary$value)) {
      shiny::showNotification(
        "The primary run is not available for loading.",
        type = "error"
      )
      return()
    }

    values <- tryCatch(
      conservative_run_values(primary$value),
      error = function(error) error
    )
    if (inherits(values, "error")) {
      shiny::showNotification(conditionMessage(values), type = "error")
      return()
    }

    clamp <- function(value, lower, upper) {
      min(max(value, lower), upper)
    }
    h2o_ppm <- clamp(values$h2o_ppm, 100, 75000)
    tcuv_c <- clamp(values$tcuv_c, -10, 50)
    tamb_c <- clamp(values$tamb_c, -10, 55)
    pamb_kpa <- clamp(values$pamb_kpa, 60, 110)
    shiny::updateRadioButtons(session, "dew_water_mode", selected = "outlet")
    shiny::updateSliderInput(
      session,
      "dew_outlet_h2o_ppm",
      value = round(h2o_ppm)
    )
    shiny::updateSliderInput(session, "dew_tcuv", value = round(tcuv_c, 1))
    shiny::updateSliderInput(session, "dew_tamb", value = round(tamb_c, 1))
    shiny::updateSliderInput(session, "dew_pamb", value = round(pamb_kpa, 1))
  }, ignoreInit = TRUE)

  available_variable_choices <- shiny::reactive({
    entries <- valid_measurement_entries()
    if (length(entries) == 0L) {
      return(character())
    }
    plot_variable_choices(lapply(entries, `[[`, "value"))
  })

  selected_group_values <- function(input_value, group_choices) {
    available_values <- unname(group_choices)
    if (is.null(input_value)) {
      return(intersect(WALZ_PLOT_VARIABLES, available_values))
    }

    intersect(input_value, available_values)
  }

  output$variable_selector <- shiny::renderUI({
    choices <- available_variable_choices()
    if (length(choices) == 0L) {
      return(alert_ui("No numeric variables are available for plotting.", "warning"))
    }

    groups <- group_plot_variable_choices(choices)
    checkbox_group <- function(input_id, label, group_choices) {
      if (length(group_choices) == 0L) {
        return(NULL)
      }

      current <- shiny::isolate(input[[input_id]])
      selected <- selected_group_values(current, group_choices)

      shiny::checkboxGroupInput(
        input_id,
        label,
        choices = group_choices,
        selected = selected
      )
    }

    shiny::div(
      class = "variable-selector",
      shiny::h5("Variables to show"),
      checkbox_group(
        "response_variables",
        "Response parameters",
        groups$response
      ),
      checkbox_group(
        "environmental_variables",
        "Environmental parameters",
        groups$environmental
      ),
      checkbox_group(
        "constant_variables",
        "Physiological constant",
        groups$physiological_constant
      ),
      shiny::p(
        class = "control-help",
        "Every numeric CSV variable is available. Selected environmental and physiological variables also appear in the A vs state tab."
      )
    )
  })

  selected_variables <- shiny::reactive({
    groups <- group_plot_variable_choices(available_variable_choices())
    unique(c(
      selected_group_values(input$response_variables, groups$response),
      selected_group_values(
        input$environmental_variables,
        groups$environmental
      ),
      selected_group_values(
        input$constant_variables,
        groups$physiological_constant
      )
    ))
  })

  timeseries_widget_result <- shiny::reactive({
    entries <- valid_measurement_entries()
    if (length(entries) == 0L) {
      return(list(value = NULL, error = NULL))
    }

    tryCatch(
      list(
        value = make_timeseries_plot(
          parsed_runs = lapply(entries, `[[`, "value"),
          time_axis = if (is.null(input$time_axis_mode)) {
            "elapsed"
          } else {
            input$time_axis_mode
          },
          variables = selected_variables(),
          run_labels = vapply(entries, `[[`, character(1), "label"),
          run_colors = vapply(entries, `[[`, character(1), "colour")
        ),
        error = NULL
      ),
      error = function(error) list(value = NULL, error = conditionMessage(error))
    )
  })

  state_widget_result <- shiny::reactive({
    entries <- valid_measurement_entries()
    if (length(entries) == 0L) {
      return(list(value = NULL, error = NULL))
    }

    tryCatch(
      list(
        value = make_state_plot(
          parsed_runs = lapply(entries, `[[`, "value"),
          variables = selected_variables(),
          run_labels = vapply(entries, `[[`, character(1), "label"),
          run_colors = vapply(entries, `[[`, character(1), "colour")
        ),
        error = NULL
      ),
      error = function(error) list(value = NULL, error = conditionMessage(error))
    )
  })

  dew_point_plan_result <- shiny::reactive({
    tryCatch(
      list(
        value = calculate_dew_point_plan(
          water_mode = input$dew_water_mode,
          tcuv_c = input$dew_tcuv,
          tamb_c = input$dew_tamb,
          pamb_kpa = input$dew_pamb,
          outlet_h2o_ppm = input$dew_outlet_h2o_ppm,
          inlet_h2o_ppm = input$dew_inlet_h2o_ppm,
          leaf_h2o_added_ppm = input$dew_leaf_h2o_added_ppm,
          safety_buffer_c = input$dew_safety_buffer
        ),
        error = NULL
      ),
      error = function(error) list(value = NULL, error = conditionMessage(error))
    )
  })

  output$dew_point_results <- shiny::renderUI({
    result <- dew_point_plan_result()
    if (!is.null(result$error)) {
      return(alert_ui(result$error, "warning"))
    }

    value <- result$value
    status_block <- function(surface, status, margin_c) {
      label <- switch(
        status,
        danger = paste(surface, "condensation risk"),
        caution = paste(surface, "clearance is below target"),
        safe = paste(surface, "cold point clears margin")
      )
      detail <- if (status == "danger") {
        sprintf(
          "The cold point is %.1f°C relative to dew point, so condensation is possible.",
          margin_c
        )
      } else if (status == "caution") {
        sprintf(
          "It is %.1f°C above dew point, below the required %.1f°C clearance.",
          margin_c,
          value$safety_buffer_c
        )
      } else {
        sprintf(
          "It is %.1f°C above dew point and meets the required %.1f°C clearance.",
          margin_c,
          value$safety_buffer_c
        )
      }
      shiny::div(
        class = paste("dew-status", paste0("dew-status-", status)),
        shiny::strong(label),
        shiny::span(detail)
      )
    }

    water_detail <- if (value$water_mode == "inlet_plus") {
      sprintf(
        "Expected outlet wa: %.0f ppm (%.0f inlet + %.0f leaf-added).",
        value$outlet_h2o_ppm,
        value$inlet_h2o_ppm,
        value$leaf_h2o_added_ppm
      )
    } else {
      sprintf("Expected outlet wa: %.0f ppm.", value$outlet_h2o_ppm)
    }

    manual_context <- if (value$cuvette_above_ambient) {
      alert_ui(
        sprintf(
          paste0(
            "Manual context: Tcuv is %.1f°C warmer than Tamb. Warm humid air can cool in the tubes, ",
            "so the tubing result above is the relevant check; its calculated Tamb - dew-point margin is %.1f°C."
          ),
          abs(value$temperature_order_margin_c),
          value$ambient_margin_c
        ),
        "info"
      )
    } else {
      NULL
    }

    shiny::tagList(
      shiny::div(
        class = "dew-status-grid",
        status_block("Cuvette", value$internal_status, value$internal_margin_c),
        status_block("Tubing", value$tubing_status, value$ambient_margin_c)
      ),
      shiny::p(
        class = "dew-calculation-summary",
        sprintf(
          "%s Calculated dew point: %.1f°C; target threshold: %.1f°C.",
          water_detail,
          value$dew_point_c,
          value$safety_threshold_c
        )
      ),
      manual_context
    )
  })

  dew_point_plan_widget_result <- shiny::reactive({
    result <- dew_point_plan_result()
    if (!is.null(result$error) || is.null(result$value)) {
      return(list(value = NULL, error = result$error))
    }
    tryCatch(
      list(value = make_dew_point_plan_plot(result$value), error = NULL),
      error = function(error) list(value = NULL, error = conditionMessage(error))
    )
  })

  output$dew_point_plan_plot <- plotly::renderPlotly({
    result <- dew_point_plan_widget_result()
    if (is.null(result$value)) {
      plotly::plotly_empty(type = "scatter", mode = "lines")
    } else {
      result$value
    }
  })

  dew_point_audit_widget_result <- shiny::reactive({
    primary <- measurement_result()
    if (!is.null(primary$error) || is.null(primary$value)) {
      return(list(value = NULL, error = primary$error))
    }

    tryCatch(
      list(
        value = make_dew_point_audit_plot(primary$value),
        summary = dew_point_audit_summary(
          primary$value,
          input$dew_safety_buffer
        ),
        error = NULL
      ),
      error = function(error) list(value = NULL, error = conditionMessage(error))
    )
  })

  output$dew_point_audit_heading <- shiny::renderUI({
    record <- selected_record()
    if (is.null(record)) {
      return(NULL)
    }
    shiny::tagList(
      shiny::p(
        class = "dew-audit-source",
        shiny::strong("Primary run: "),
        record$name[[1]]
      ),
      shiny::p(
        class = "dew-audit-method",
        "Dew point is calculated at every timestamp from the actual recorded wa [ppm] and Pamb [kPa] values. WALZ VPD [Pa/kPa] is a normalized leaf-to-air VPD; convert it to kPa as VPD × Pamb / 1000. VPD is not used for this dew-point calculation."
      )
    )
  })

  output$dew_point_audit_alert <- shiny::renderUI({
    result <- dew_point_audit_widget_result()
    if (!is.null(result$error)) {
      return(alert_ui(
        paste("The recorded dew-point audit is unavailable:", result$error),
        "warning"
      ))
    }
    summary <- result$summary
    danger_count <- summary$tubing$danger_count + summary$internal$danger_count
    caution_count <- summary$tubing$caution_count + summary$internal$caution_count
    risk_alert <- if (danger_count > 0L) {
      alert_ui(
        sprintf(
          paste0(
            "Recorded condensation risk: Tamb reached or fell below dew point in %d rows, and ",
            "Tcuv - 2°C did so in %d rows. Minimum margins were %.1f°C for tubing and %.1f°C internally."
          ),
          summary$tubing$danger_count,
          summary$internal$danger_count,
          summary$tubing$minimum_margin_c,
          summary$internal$minimum_margin_c
        ),
        "danger"
      )
    } else if (caution_count > 0L) {
      alert_ui(
        sprintf(
          paste0(
            "No recorded row reached the dew point, but the selected %.1f°C clearance was missed ",
            "in %d tubing rows and %d internal-cuvette rows. Minimum margins were %.1f°C and %.1f°C."
          ),
          summary$safety_buffer_c,
          summary$tubing$caution_count,
          summary$internal$caution_count,
          summary$tubing$minimum_margin_c,
          summary$internal$minimum_margin_c
        ),
        "warning"
      )
    } else {
      alert_ui(
        sprintf(
          paste0(
            "No recorded row reached the dew point or breached the selected %.1f°C clearance. ",
            "Minimum margins were %.1f°C for tubing and %.1f°C internally."
          ),
          summary$safety_buffer_c,
          summary$tubing$minimum_margin_c,
          summary$internal$minimum_margin_c
        ),
        "info"
      )
    }

    manual_context <- if (summary$cuvette_above_ambient_count > 0L) {
      alert_ui(
        sprintf(
          paste0(
            "Manual context: Tcuv was above Tamb in %d of %d valid rows (maximum %.1f°C). ",
            "This makes the separate tubing margin important, but is not by itself evidence that condensation occurred."
          ),
          summary$cuvette_above_ambient_count,
          summary$valid_count,
          summary$maximum_cuvette_excess_c
        ),
        "info"
      )
    } else {
      NULL
    }

    shiny::tagList(risk_alert, manual_context)
  })

  output$dew_point_audit_plot <- plotly::renderPlotly({
    result <- dew_point_audit_widget_result()
    if (is.null(result$value)) {
      plotly::plotly_empty(type = "scatter", mode = "lines")
    } else {
      result$value
    }
  })

  load_protocol_result <- function(record, index) {
    if (is.null(index) || is.null(record)) {
      return(list(match = NULL, text = NULL, error = NULL))
    }

    matched <- match_protocol(record$name[[1]], index$protocols)
    if (matched$status != "matched") {
      return(list(match = matched, text = NULL, error = NULL))
    }

    content <- tryCatch(
      load_remote_protocol(matched$protocol),
      error = function(error) error
    )
    if (inherits(content, "error")) {
      return(list(
        match = matched,
        text = NULL,
        error = conditionMessage(content)
      ))
    }
    list(match = matched, text = content, error = NULL)
  }

  protocol_results <- shiny::reactive({
    index <- drive_index()
    context <- selected_run_context()
    records <- context$records
    if (is.null(records) || nrow(records) == 0L) {
      return(list())
    }

    lapply(seq_len(nrow(records)), function(run_index) {
      record <- records[run_index, , drop = FALSE]
      list(
        role = sprintf("Run %d", run_index),
        record = record,
        colour = unname(context$colors[[run_index]]),
        protocol = load_protocol_result(record, index)
      )
    })
  })

  output$source_status <- shiny::renderUI({
    index <- drive_index()
    metadata <- run_metadata()
    selected <- selected_records()

    shiny::tagList(
      shiny::h5("Source status"),
      drive_status_link_ui(),
      shiny::br(),
      metadata_sheet_link_ui("drive-link drive-status-link"),
      if (!is.null(source_error())) alert_ui(source_error(), "danger"),
      if (!is.null(metadata_error())) alert_ui(metadata_error(), "warning"),
      if (is.null(index) && is.null(source_error())) {
        shiny::p(class = "muted-status", "Connecting to the public folder …")
      },
      shiny::tags$dl(
        class = "source-details",
        shiny::tags$dt("Runs found"),
        shiny::tags$dd(if (is.null(index)) "—" else nrow(index$measurements)),
        shiny::tags$dt("Protocols found"),
        shiny::tags$dd(if (is.null(index)) "—" else nrow(index$protocols)),
        shiny::tags$dt("Metadata rows"),
        shiny::tags$dd(if (is.null(metadata)) "—" else nrow(metadata)),
        shiny::tags$dt("Selected runs"),
        shiny::tags$dd(if (is.null(selected)) 0L else nrow(selected)),
        shiny::tags$dt("Sources refreshed"),
        shiny::tags$dd(if (is.null(index)) {
          "—"
        } else {
          format(index$refreshed_at, "%Y-%m-%d %H:%M:%S %Z")
        })
      )
    )
  })

  output$run_metadata_panel <- shiny::renderUI({
    context <- selected_run_context()
    records <- context$records
    metadata <- run_metadata()

    if (is.null(records) || nrow(records) == 0L) {
      return(bslib::card(
        class = "run-metadata-card",
        bslib::card_header("Selected run details"),
        alert_ui("Select at least one measurement run.", "warning")
      ))
    }

    if (is.null(metadata)) {
      message <- metadata_error()
      if (is.null(message)) {
        message <- "Connecting to the public run metadata sheet …"
      }
      return(bslib::card(
        class = "run-metadata-card",
        bslib::card_header(
          "Selected run details",
          metadata_sheet_link_ui("metadata-card-link")
        ),
        alert_ui(message, if (is.null(metadata_error())) "info" else "warning")
      ))
    }

    metadata_columns <- setdiff(names(metadata), ".run_id")
    table_rows <- list()
    unmatched <- character()
    duplicate_matches <- character()

    for (run_index in seq_len(nrow(records))) {
      record <- records[run_index, , drop = FALSE]
      matches <- match_run_metadata(metadata, record$name[[1]])
      colour <- unname(context$colors[[run_index]])
      if (nrow(matches) == 0L) {
        unmatched <- c(unmatched, measurement_run_id(record$name[[1]]))
        matches <- as.data.frame(
          stats::setNames(
            as.list(rep("", length(metadata_columns))),
            metadata_columns
          ),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        matches$timestamp <- measurement_run_id(record$name[[1]])
        match_label <- "No exact sheet row"
      } else {
        if (nrow(matches) > 1L) {
          duplicate_matches <- c(
            duplicate_matches,
            measurement_run_id(record$name[[1]])
          )
        }
        match_label <- if (nrow(matches) == 1L) {
          "Exact ID"
        } else {
          sprintf("%d exact rows", nrow(matches))
        }
      }

      for (match_index in seq_len(nrow(matches))) {
        cells <- lapply(metadata_columns, function(column) {
          value <- matches[[column]][[match_index]]
          if (is.na(value) || !nzchar(value)) value <- "—"
          shiny::tags$td(value)
        })
        table_rows <- c(table_rows, list(shiny::tags$tr(
          class = if (match_label == "No exact sheet row") {
            "run-metadata-row run-metadata-row-unmatched"
          } else {
            "run-metadata-row"
          },
          style = sprintf(
            "border-left: 0.42rem solid %s; background-color: %s;",
            colour,
            hex_to_rgba(colour, 0.1)
          ),
          shiny::tags$td(
            class = "run-colour-cell",
            shiny::span(
              class = "run-colour-swatch",
              style = sprintf("background-color: %s;", colour),
              title = colour
            )
          ),
          shiny::tags$td(class = "measurement-filename-cell", record$name[[1]]),
          shiny::tags$td(class = "metadata-match-cell", match_label),
          cells
        )))
      }
    }

    alerts <- list()
    if (length(unmatched) > 0L) {
      alerts <- c(alerts, list(alert_ui(
        paste0(
          "No exact metadata row was found for: ",
          paste(unique(unmatched), collapse = ", "),
          ". No fuzzy or timestamp-only guess was used."
        ),
        "warning"
      )))
    }
    if (length(duplicate_matches) > 0L) {
      alerts <- c(alerts, list(alert_ui(
        paste0(
          "Multiple exact metadata rows were found for: ",
          paste(unique(duplicate_matches), collapse = ", "),
          ". All exact rows are shown."
        ),
        "warning"
      )))
    }

    bslib::card(
      class = "run-metadata-card",
      bslib::card_header(
        shiny::span("Selected run details"),
        metadata_sheet_link_ui("metadata-card-link")
      ),
      shiny::p(
        class = "metadata-match-note",
        "Rows are joined by exact measurement filename stem = sheet Run ID. Row colour matches the plotted run."
      ),
      if (length(alerts) > 0L) shiny::tagList(alerts),
      shiny::div(
        class = "run-metadata-scroll",
        shiny::tags$table(
          class = "run-metadata-table",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("Plot"),
            shiny::tags$th("Measurement file"),
            shiny::tags$th("Metadata match"),
            lapply(metadata_columns, function(column) {
              shiny::tags$th(metadata_column_label(column))
            })
          )),
          shiny::tags$tbody(table_rows)
        )
      )
    )
  })

  output$timeseries_alerts <- shiny::renderUI({
    alerts <- list()
    if (!is.null(source_error())) {
      alerts <- c(alerts, list(alert_ui(source_error(), "danger")))
    }

    results <- measurement_results()
    for (entry in results) {
      if (!is.null(entry$error)) {
        alerts <- c(alerts, list(alert_ui(
          paste(entry$label, entry$error, sep = ": "),
          "danger"
        )))
      } else if (length(entry$value$issues) > 0L) {
        alerts <- c(alerts, lapply(
          entry$value$issues,
          function(issue) alert_ui(paste(entry$label, issue, sep = ": "), "warning")
        ))
      }
    }
    if (length(valid_measurement_entries()) > 1L) {
      alignment_message <- if (identical(input$time_axis_mode, "local")) {
        paste0(
          "Multi-run local time: each run retains its original Europe/Zurich ",
          "timestamp, so runs recorded on different dates may be separated on the x axis."
        )
      } else {
        paste0(
          "Multi-run alignment: every run starts at elapsed minute zero. ",
          "Hover over a point to see its original timestamp."
        )
      }
      alerts <- c(alerts, list(alert_ui(alignment_message, "info")))
    }
    if (length(selected_variables()) == 0L) {
      alerts <- c(alerts, list(alert_ui("Select at least one variable to plot.", "warning")))
    }

    plot_result <- timeseries_widget_result()
    if (!is.null(plot_result$error)) {
      alerts <- c(
        alerts,
        list(alert_ui(
          paste("The timeseries could not be drawn:", plot_result$error),
          "danger"
        ))
      )
    }
    if (length(alerts) == 0L) NULL else shiny::tagList(alerts)
  })

  output$state_alerts <- shiny::renderUI({
    if (!is.null(source_error())) {
      return(alert_ui(source_error(), "danger"))
    }
    entries <- valid_measurement_entries()
    if (length(entries) == 0L) {
      return(alert_ui("No selected run is available for plotting.", "warning"))
    }
    missing_a <- vapply(
      entries,
      function(entry) !"A" %in% names(entry$value$data),
      logical(1)
    )
    if (any(missing_a)) {
      return(alert_ui(
        paste0(
          "The A vs state view requires A in every selected run. Missing in: ",
          paste(vapply(entries[missing_a], `[[`, character(1), "label"), collapse = ", ")
        ),
        "warning"
      ))
    }
    if (length(setdiff(selected_variables(), "A")) == 0L) {
      return(alert_ui("Select at least one state variable for the A vs state view.", "warning"))
    }

    plot_result <- state_widget_result()
    if (!is.null(plot_result$error)) {
      return(alert_ui(
        paste("The A vs state view could not be drawn:", plot_result$error),
        "danger"
      ))
    }
    NULL
  })

  output$timeseries_plot <- plotly::renderPlotly({
    result <- timeseries_widget_result()
    if (is.null(result$value)) {
      plotly::plotly_empty(type = "scatter", mode = "markers")
    } else {
      result$value
    }
  })

  output$state_plot <- plotly::renderPlotly({
    result <- state_widget_result()
    if (is.null(result$value)) {
      plotly::plotly_empty(type = "scatter", mode = "markers")
    } else {
      result$value
    }
  })

  output$protocol_panel <- shiny::renderUI({
    entries <- protocol_results()
    if (length(entries) == 0L) {
      return(alert_ui("Select at least one measurement run.", "warning"))
    }
    shiny::tagList(lapply(entries, function(entry) {
      protocol_card_ui(
        entry$protocol,
        entry$role,
        entry$record$name[[1]],
        entry$colour
      )
    }))
  })
}

shiny::shinyApp(ui, server)
