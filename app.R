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
source("R/protocol_match.R", local = TRUE)
source("R/drive_data.R", local = TRUE)
source("R/plots.R", local = TRUE)

config <- walz_config()
configure_drive_access(config$api_key)

drive_folder_url <- sprintf(
  "https://drive.google.com/drive/folders/%s",
  config$drive_folder_id
)

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

protocol_card_ui <- function(protocol, role, measurement_name) {
  header <- sprintf("%s measurement protocol", role)
  source_line <- shiny::p(
    class = "protocol-measurement-source",
    shiny::strong("Measurement: "),
    measurement_name
  )

  if (is.null(protocol$match)) {
    return(bslib::card(
      class = "protocol-card",
      bslib::card_header(header),
      source_line,
      alert_ui("No measurement is currently selected.", "warning")
    ))
  }

  if (protocol$match$status != "matched") {
    return(bslib::card(
      class = "protocol-card",
      bslib::card_header(header),
      source_line,
      alert_ui(protocol$match$message, "warning")
    ))
  }

  filename <- protocol$match$protocol$name[[1]]
  if (!is.null(protocol$error)) {
    return(bslib::card(
      class = "protocol-card",
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
    shiny::uiOutput("source_status"),
    shiny::hr(),
    shiny::p(
      class = "sidebar-intro",
      "Choose one WALZ run or align and overlay a second run."
    ),
    shiny::selectInput(
      "measurement_id",
      "Primary measurement run",
      choices = character(),
      selectize = TRUE
    ),
    shiny::checkboxInput(
      "overlay_enabled",
      "Overlay a second measurement run",
      value = FALSE
    ),
    shiny::conditionalPanel(
      condition = "input.overlay_enabled",
      shiny::selectInput(
        "comparison_id",
        "Overlay measurement run",
        choices = character(),
        selectize = TRUE
      ),
      shiny::p(
        class = "control-help",
        "Overlay runs are aligned at elapsed minute zero; original timestamps remain in the hover text."
      )
    ),
    shiny::uiOutput("variable_selector"),
    shiny::actionButton(
      "refresh_latest",
      "Refresh and show latest",
      icon = shiny::icon("rotate")
    ),
    shiny::checkboxInput(
      "show_grid",
      "Show 15-minute time grid",
      value = FALSE
    ),
    shiny::a(
      "Open source folder in Google Drive",
      href = drive_folder_url,
      target = "_blank",
      rel = "noopener noreferrer",
      class = "drive-link"
    )
  ),
  shiny::includeCSS("www/styles.css"),
  shiny::div(
    class = "app-introduction",
    shiny::h2("Explore gas-exchange runs without code"),
    shiny::p(
      "Zoom, pan, hover over observations, or draw directly on the plots. ",
      "Selected variables control both the timeseries and A-versus-state views."
    )
  ),
  shiny::uiOutput("selected_file_heading"),
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
    )
  )
)

server <- function(input, output, session) {
  drive_index <- shiny::reactiveVal(NULL)
  source_error <- shiny::reactiveVal(NULL)

  refresh_drive <- function() {
    source_error(NULL)
    result <- tryCatch(
      list_walz_drive(config$drive_folder_id),
      error = function(error) error
    )

    if (inherits(result, "error")) {
      source_error(conditionMessage(result))
      return(invisible(FALSE))
    }

    drive_index(result)
    choices <- stats::setNames(
      result$measurements$id,
      result$measurements$name
    )
    selected <- if (length(choices) > 0L) unname(choices[[1]]) else character()
    shiny::updateSelectInput(
      session,
      "measurement_id",
      choices = choices,
      selected = selected
    )
    invisible(TRUE)
  }

  shiny::observeEvent(TRUE, refresh_drive(), once = TRUE)
  shiny::observeEvent(input$refresh_latest, refresh_drive(), ignoreInit = TRUE)

  shiny::observe({
    index <- drive_index()
    if (is.null(index)) {
      return()
    }

    primary_id <- input$measurement_id
    candidates <- index$measurements
    if (!is.null(primary_id) && nzchar(primary_id)) {
      candidates <- candidates[candidates$id != primary_id, , drop = FALSE]
    }
    choices <- stats::setNames(candidates$id, candidates$name)
    current <- shiny::isolate(input$comparison_id)
    selected <- if (!is.null(current) && current %in% unname(choices)) {
      current
    } else if (length(choices) > 0L) {
      unname(choices[[1]])
    } else {
      character()
    }
    shiny::updateSelectInput(
      session,
      "comparison_id",
      choices = choices,
      selected = selected
    )
  })

  overlay_active <- shiny::reactive(isTRUE(input$overlay_enabled))

  selected_record <- shiny::reactive({
    resolve_selected_measurement(drive_index(), input$measurement_id)
  })

  selected_comparison_record <- shiny::reactive({
    if (!overlay_active()) {
      return(NULL)
    }
    record <- resolve_selected_measurement(drive_index(), input$comparison_id)
    primary <- selected_record()
    if (
      !is.null(record) &&
        !is.null(primary) &&
        identical(record$id[[1]], primary$id[[1]])
    ) {
      return(NULL)
    }
    record
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

  measurement_result <- shiny::reactive({
    load_measurement_result(
      selected_record(),
      "No primary measurement is currently selected."
    )
  })

  comparison_measurement_result <- shiny::reactive({
    if (!overlay_active()) {
      return(list(value = NULL, error = NULL))
    }
    load_measurement_result(
      selected_comparison_record(),
      "Select a different measurement run to use as the overlay."
    )
  })

  available_variable_choices <- shiny::reactive({
    primary <- measurement_result()
    if (!is.null(primary$error) || is.null(primary$value)) {
      return(character())
    }
    parsed_runs <- list(primary$value)
    comparison <- comparison_measurement_result()
    if (overlay_active() && is.null(comparison$error) && !is.null(comparison$value)) {
      parsed_runs <- c(parsed_runs, list(comparison$value))
    }
    plot_variable_choices(parsed_runs)
  })

  output$variable_selector <- shiny::renderUI({
    choices <- available_variable_choices()
    if (length(choices) == 0L) {
      return(alert_ui("No numeric variables are available for plotting.", "warning"))
    }

    current <- shiny::isolate(input$plot_variables)
    available_values <- unname(choices)
    selected <- if (is.null(current)) {
      intersect(WALZ_PLOT_VARIABLES, available_values)
    } else {
      intersect(current, available_values)
    }

    shiny::div(
      class = "variable-selector",
      shiny::checkboxGroupInput(
        "plot_variables",
        "Variables to show",
        choices = choices,
        selected = selected
      ),
      shiny::p(
        class = "control-help",
        "Every numeric CSV variable is available. Selected state variables also appear in the A vs state tab."
      )
    )
  })

  selected_variables <- shiny::reactive({
    variables <- input$plot_variables
    if (is.null(variables)) character() else variables
  })

  active_run_labels <- shiny::reactive({
    primary <- selected_record()
    labels <- if (is.null(primary)) character() else primary$name[[1]]
    comparison <- selected_comparison_record()
    if (overlay_active() && !is.null(comparison)) {
      labels <- c(labels, comparison$name[[1]])
    }
    labels
  })

  timeseries_widget_result <- shiny::reactive({
    primary <- measurement_result()
    comparison <- comparison_measurement_result()
    if (!is.null(primary$error) || is.null(primary$value)) {
      return(list(value = NULL, error = NULL))
    }
    if (overlay_active() && (!is.null(comparison$error) || is.null(comparison$value))) {
      return(list(value = NULL, error = NULL))
    }

    tryCatch(
      list(
        value = make_timeseries_plot(
          parsed = primary$value,
          show_grid = isTRUE(input$show_grid),
          variables = selected_variables(),
          comparison = if (overlay_active()) comparison$value else NULL,
          run_labels = active_run_labels()
        ),
        error = NULL
      ),
      error = function(error) list(value = NULL, error = conditionMessage(error))
    )
  })

  state_widget_result <- shiny::reactive({
    primary <- measurement_result()
    comparison <- comparison_measurement_result()
    if (!is.null(primary$error) || is.null(primary$value)) {
      return(list(value = NULL, error = NULL))
    }
    if (overlay_active() && (!is.null(comparison$error) || is.null(comparison$value))) {
      return(list(value = NULL, error = NULL))
    }

    tryCatch(
      list(
        value = make_state_plot(
          parsed = primary$value,
          variables = selected_variables(),
          comparison = if (overlay_active()) comparison$value else NULL,
          run_labels = active_run_labels()
        ),
        error = NULL
      ),
      error = function(error) list(value = NULL, error = conditionMessage(error))
    )
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
    primary <- selected_record()
    entries <- list(list(
      role = "Primary",
      record = primary,
      protocol = load_protocol_result(primary, index)
    ))

    comparison <- selected_comparison_record()
    if (overlay_active()) {
      entries <- c(entries, list(list(
        role = "Overlay",
        record = comparison,
        protocol = load_protocol_result(comparison, index)
      )))
    }
    entries
  })

  output$source_status <- shiny::renderUI({
    if (!is.null(source_error())) {
      return(shiny::tagList(
        shiny::h5("Drive status"),
        alert_ui(source_error(), "danger")
      ))
    }

    index <- drive_index()
    if (is.null(index)) {
      return(shiny::tagList(
        shiny::h5("Drive status"),
        shiny::p(class = "muted-status", "Connecting to the public folder …")
      ))
    }

    primary <- selected_record()
    comparison <- selected_comparison_record()
    format_modified <- function(record) {
      if (is.null(record)) {
        return("No run selected")
      }
      format(
        record$modified_time[[1]],
        "%Y-%m-%d %H:%M %Z",
        tz = WALZ_TIMEZONE
      )
    }

    shiny::tagList(
      shiny::h5("Drive status"),
      shiny::tags$dl(
        class = "source-details",
        shiny::tags$dt("Runs found"),
        shiny::tags$dd(nrow(index$measurements)),
        shiny::tags$dt("Protocols found"),
        shiny::tags$dd(nrow(index$protocols)),
        shiny::tags$dt("List refreshed"),
        shiny::tags$dd(format(index$refreshed_at, "%Y-%m-%d %H:%M:%S %Z")),
        shiny::tags$dt("Primary upload modified"),
        shiny::tags$dd(format_modified(primary)),
        if (overlay_active()) shiny::tags$dt("Overlay upload modified"),
        if (overlay_active()) shiny::tags$dd(format_modified(comparison))
      )
    )
  })

  output$selected_file_heading <- shiny::renderUI({
    primary <- selected_record()
    if (is.null(primary)) {
      return(shiny::div(
        class = "selected-file-banner selected-file-banner-empty",
        shiny::span(class = "selected-file-label", "Measurement file"),
        shiny::h3("No measurement selected")
      ))
    }

    comparison <- selected_comparison_record()
    shiny::div(
      class = "selected-file-banner",
      shiny::div(
        class = "selected-file-entry",
        shiny::span(class = "selected-file-label", "Primary measurement file"),
        shiny::h3(primary$name[[1]])
      ),
      if (overlay_active()) shiny::div(
        class = "selected-file-entry selected-file-overlay",
        shiny::span(class = "selected-file-label", "Overlay measurement file"),
        shiny::h3(if (is.null(comparison)) "No overlay selected" else comparison$name[[1]])
      )
    )
  })

  output$timeseries_alerts <- shiny::renderUI({
    alerts <- list()
    if (!is.null(source_error())) {
      alerts <- c(alerts, list(alert_ui(source_error(), "danger")))
    }

    primary <- measurement_result()
    comparison <- comparison_measurement_result()
    if (!is.null(primary$error)) {
      alerts <- c(alerts, list(alert_ui(primary$error, "danger")))
    } else if (length(primary$value$issues) > 0L) {
      alerts <- c(alerts, lapply(primary$value$issues, alert_ui, level = "warning"))
    }
    if (overlay_active() && !is.null(comparison$error)) {
      alerts <- c(alerts, list(alert_ui(comparison$error, "danger")))
    } else if (
      overlay_active() &&
        !is.null(comparison$value) &&
        length(comparison$value$issues) > 0L
    ) {
      alerts <- c(
        alerts,
        lapply(comparison$value$issues, alert_ui, level = "warning")
      )
    }
    if (overlay_active() && is.null(comparison$error)) {
      alerts <- c(alerts, list(alert_ui(
        paste0(
          "Overlay alignment: both runs start at elapsed minute zero. ",
          "Hover over a point to see its original timestamp."
        ),
        "info"
      )))
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
    primary <- measurement_result()
    comparison <- comparison_measurement_result()
    if (!is.null(source_error())) {
      return(alert_ui(source_error(), "danger"))
    }
    if (!is.null(primary$error)) {
      return(alert_ui(primary$error, "danger"))
    }
    if (overlay_active() && !is.null(comparison$error)) {
      return(alert_ui(comparison$error, "danger"))
    }
    if (!"A" %in% names(primary$value$data)) {
      return(alert_ui("The primary run does not contain A, so this view is unavailable.", "warning"))
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
    shiny::tagList(lapply(entries, function(entry) {
      measurement_name <- if (is.null(entry$record)) {
        "No measurement selected"
      } else {
        entry$record$name[[1]]
      }
      protocol_card_ui(
        entry$protocol,
        entry$role,
        measurement_name
      )
    }))
  })
}

shiny::shinyApp(ui, server)
