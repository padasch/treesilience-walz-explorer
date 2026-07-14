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
      "Choose a WALZ run. The newest Drive upload is selected automatically."
    ),
    shiny::selectInput(
      "measurement_id",
      "Measurement run",
      choices = character(),
      selectize = TRUE
    ),
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
    shiny::hr(),
    shiny::uiOutput("source_status"),
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
      "The corresponding raw instrument protocol is shown below the timeseries when it can be matched safely."
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

  selected_record <- shiny::reactive({
    index <- drive_index()
    resolve_selected_measurement(index, input$measurement_id)
  })

  measurement_result <- shiny::reactive({
    record <- selected_record()
    if (is.null(record)) {
      return(list(value = NULL, error = "No measurement is currently selected."))
    }

    tryCatch(
      list(value = load_remote_measurement(record), error = NULL),
      error = function(error) list(value = NULL, error = conditionMessage(error))
    )
  })

  timeseries_widget_result <- shiny::reactive({
    result <- measurement_result()
    if (!is.null(result$error) || is.null(result$value)) {
      return(list(value = NULL, error = NULL))
    }
    tryCatch(
      list(
        value = make_timeseries_plot(result$value, isTRUE(input$show_grid)),
        error = NULL
      ),
      error = function(error) list(value = NULL, error = conditionMessage(error))
    )
  })

  state_widget_result <- shiny::reactive({
    result <- measurement_result()
    if (!is.null(result$error) || is.null(result$value)) {
      return(list(value = NULL, error = NULL))
    }
    tryCatch(
      list(value = make_state_plot(result$value), error = NULL),
      error = function(error) list(value = NULL, error = conditionMessage(error))
    )
  })

  protocol_result <- shiny::reactive({
    index <- drive_index()
    record <- selected_record()
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

    record <- selected_record()
    selected_modified <- if (is.null(record)) {
      "No run selected"
    } else {
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
        shiny::tags$dt("Selected upload modified"),
        shiny::tags$dd(selected_modified)
      )
    )
  })

  output$selected_file_heading <- shiny::renderUI({
    record <- selected_record()
    if (is.null(record)) {
      return(shiny::div(
        class = "selected-file-banner selected-file-banner-empty",
        shiny::span(class = "selected-file-label", "Measurement file"),
        shiny::h3("No measurement selected")
      ))
    }

    shiny::div(
      class = "selected-file-banner",
      shiny::span(class = "selected-file-label", "Showing measurement file"),
      shiny::h3(record$name[[1]])
    )
  })

  output$timeseries_alerts <- shiny::renderUI({
    alerts <- list()
    if (!is.null(source_error())) {
      alerts <- c(alerts, list(alert_ui(source_error(), "danger")))
    }
    result <- measurement_result()
    if (!is.null(result$error)) {
      alerts <- c(alerts, list(alert_ui(result$error, "danger")))
    } else if (length(result$value$issues) > 0L) {
      alerts <- c(
        alerts,
        lapply(result$value$issues, alert_ui, level = "warning")
      )
    }
    plot_result <- timeseries_widget_result()
    if (!is.null(plot_result$error)) {
      alerts <- c(
        alerts,
        list(alert_ui(paste("The timeseries could not be drawn:", plot_result$error), "danger"))
      )
    }
    if (length(alerts) == 0L) NULL else shiny::tagList(alerts)
  })

  output$state_alerts <- shiny::renderUI({
    result <- measurement_result()
    if (!is.null(source_error())) {
      return(alert_ui(source_error(), "danger"))
    }
    if (!is.null(result$error)) {
      return(alert_ui(result$error, "danger"))
    }
    if ("A" %in% result$value$missing_variables) {
      return(alert_ui("This run does not contain A, so this view is unavailable.", "warning"))
    }
    plot_result <- state_widget_result()
    if (!is.null(plot_result$error)) {
      return(alert_ui(paste("The A vs state view could not be drawn:", plot_result$error), "danger"))
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
    protocol <- protocol_result()
    if (is.null(protocol$match)) {
      return(bslib::card(
        class = "protocol-card",
        bslib::card_header("Measurement protocol"),
        alert_ui("No measurement is currently selected.", "warning")
      ))
    }

    if (protocol$match$status != "matched") {
      return(bslib::card(
        class = "protocol-card",
        bslib::card_header("Measurement protocol"),
        alert_ui(protocol$match$message, "warning")
      ))
    }

    filename <- protocol$match$protocol$name[[1]]
    if (!is.null(protocol$error)) {
      return(bslib::card(
        class = "protocol-card",
        bslib::card_header("Measurement protocol"),
        shiny::p(class = "protocol-filename", filename),
        alert_ui(
          paste("The protocol matched, but its content could not be downloaded:", protocol$error),
          "danger"
        )
      ))
    }

    bslib::card(
      class = "protocol-card",
      bslib::card_header("Measurement protocol"),
      shiny::div(
        class = "protocol-heading",
        shiny::span(class = "protocol-filename", filename),
        shiny::span(class = "match-method", protocol$match$method)
      ),
      alert_ui(protocol$match$message, "info"),
      shiny::tags$pre(class = "protocol-content", protocol$text)
    )
  })
}

shiny::shinyApp(ui, server)
