.walz_future_initialized <- FALSE

ensure_background_workers <- function(workers = 2L) {
  if (!isTRUE(.walz_future_initialized)) {
    future::plan(future::multisession, workers = max(2L, as.integer(workers)))
    .walz_future_initialized <<- TRUE
  }
  invisible(TRUE)
}

metadata_value <- function(row, candidates, default = "") {
  candidate <- intersect(candidates, names(row))
  if (length(candidate) == 0L || nrow(row) == 0L) return(default)
  value <- trimws(as.character(row[[candidate[[1]]]][[1]]))
  if (is.na(value)) default else value
}

build_run_catalog <- function(measurements, metadata) {
  if (is.null(measurements) || nrow(measurements) == 0L) {
    return(data.frame())
  }
  rows <- lapply(seq_len(nrow(measurements)), function(index) {
    record <- measurements[index, , drop = FALSE]
    run_id <- measurement_run_id(record$name[[1]])
    matches <- match_run_metadata(metadata, record$name[[1]])
    match_count <- nrow(matches)
    row <- if (match_count >= 1L) matches[1, , drop = FALSE] else data.frame()
    quality_raw <- if (match_count == 1L) {
      metadata_value(row, WALZ_QUALITY_COLUMN)
    } else ""
    quality <- as.character(canonical_quality(quality_raw))
    invalid_quality <- nzchar(quality_raw) &&
      !tolower(quality_raw) %in% c("good", "medium", "bad")
    datetime <- run_datetime_from_id(run_id)
    data.frame(
      id = record$id[[1]],
      name = record$name[[1]],
      run_id = run_id,
      modified_iso = record$modified_iso[[1]],
      modified_time = record$modified_time[[1]],
      run_datetime = datetime,
      date = as.Date(datetime, tz = WALZ_TIMEZONE),
      species = metadata_value(row, c("TREE species", "species")),
      plant_id = metadata_value(row, c("plant id", "plant ID")),
      walz_number = metadata_value(row, c("WALZ walz number")),
      target_tcuv = metadata_value(row, c("walz cuvette temp")),
      h2o_input = metadata_value(row, c("walz h2o input")),
      co2_input = metadata_value(row, c("walz co2 input")),
      xibox_temperature = metadata_value(row, c("XIBOX xibox temp")),
      xibox_light = metadata_value(row, c("xibox light")),
      xibox_humidity = metadata_value(row, c("xibox humidity")),
      protocol_description = metadata_value(
        row,
        grep("protocol description", names(row), value = TRUE, ignore.case = TRUE)
      ),
      quality_raw = quality_raw,
      quality = quality,
      quality_invalid = invalid_quality,
      metadata_matches = match_count,
      metadata_status = if (match_count == 1L) "Exact ID" else if (match_count == 0L) {
        "No metadata row"
      } else "Duplicate metadata rows",
      colour = stable_run_colour(run_id),
      stringsAsFactors = FALSE
    )
  })
  catalog <- do.call(rbind, rows)
  rownames(catalog) <- NULL
  catalog
}

filter_run_catalog <- function(
    catalog,
    species = character(),
    plant_ids = character(),
    date_range = NULL,
    qualities = NULL) {
  if (is.null(catalog) || nrow(catalog) == 0L) return(catalog)
  clean_values <- function(values) {
    values <- as.character(values)
    values[!is.na(values) & nzchar(values)]
  }
  species <- clean_values(species)
  plant_ids <- clean_values(plant_ids)
  if (!is.null(qualities)) qualities <- clean_values(qualities)
  keep <- rep(TRUE, nrow(catalog))
  if (length(species) > 0L) keep <- keep & catalog$species %in% species
  if (length(plant_ids) > 0L) keep <- keep & catalog$plant_id %in% plant_ids
  if (!is.null(date_range) && length(date_range) == 2L && all(!is.na(date_range))) {
    keep <- keep & !is.na(catalog$date) &
      catalog$date >= as.Date(date_range[[1]]) &
      catalog$date <= as.Date(date_range[[2]])
  }
  if (!is.null(qualities) && length(qualities) > 0L) {
    keep <- keep & catalog$quality %in% qualities
  }
  catalog[keep, , drop = FALSE]
}

prepared_step_table <- function(prepared, catalog) {
  if (length(prepared) == 0L || is.null(catalog) || nrow(catalog) == 0L) {
    return(data.frame())
  }
  tables <- lapply(seq_len(nrow(catalog)), function(index) {
    entry <- prepared[[catalog$id[[index]]]]
    if (is.null(entry) || !is.null(entry$error) || nrow(entry$extraction$summary) == 0L) {
      return(NULL)
    }
    summary <- entry$extraction$summary
    summary$id <- catalog$id[[index]]
    summary$name <- catalog$name[[index]]
    summary$quality <- catalog$quality[[index]]
    summary$species <- catalog$species[[index]]
    summary$plant_id <- catalog$plant_id[[index]]
    summary$metadata_status <- catalog$metadata_status[[index]]
    summary$colour <- catalog$colour[[index]]
    summary
  })
  tables <- Filter(Negate(is.null), tables)
  if (length(tables) == 0L) data.frame() else do.call(rbind, tables)
}

qc_trace_hover <- function(data) {
  sprintf(
    paste0(
      "<b>%s</b><br>Species: %s<br>Plant: %s<br>",
      "PPFD: %.1f ± %.1f<br>A: %.3f ± %.3f<br>",
      "A drift: %.4f min⁻¹<br>Coverage: %.0f%%<br>%s"
    ),
    data$run_id, ifelse(nzchar(data$species), data$species, "—"),
    ifelse(nzchar(data$plant_id), data$plant_id, "—"),
    data$PPFD_mean, data$PPFD_sd, data$A_mean, data$A_sd,
    data$A_slope, 100 * data$coverage, data$metadata_status
  )
}

make_qc_overview_plot <- function(step_data) {
  groups <- c("good", "medium", "bad", "unassessed")
  labels <- c(Good = "good", Medium = "medium", Bad = "bad", Unassessed = "unassessed")
  if (is.null(step_data) || nrow(step_data) == 0L) {
    return(list(
      widget = plotly::event_register(
        plotly::plotly_empty(type = "scatter", mode = "lines"),
        "plotly_click"
      ),
      warnings = character()
    ))
  }
  complete <- step_data[step_data$window_complete, , drop = FALSE]
  if (nrow(complete) == 0L) {
    return(list(
      widget = plotly::event_register(
        plotly::plotly_empty(type = "scatter", mode = "lines"),
        "plotly_click"
      ),
      warnings = "No complete extraction windows are available."
    ))
  }
  x_range <- range(c(0, complete$PPFD_mean), finite = TRUE)
  raw_range <- range(complete$A_mean, finite = TRUE)
  raw_padding <- max(0.1, diff(raw_range) * 0.05)
  raw_range <- raw_range + c(-raw_padding, raw_padding)
  normalized_values <- numeric()
  warnings <- character()
  panels <- list()

  for (quality in groups) {
    group_data <- complete[complete$quality == quality, , drop = FALSE]
    raw_panel <- plotly::plot_ly(source = "qc_overview", type = "scatter", mode = "lines")
    normalized_panel <- plotly::plot_ly(source = "qc_overview", type = "scatter", mode = "lines")
    for (run in unique(group_data$run_id)) {
      run_data <- group_data[group_data$run_id == run, , drop = FALSE]
      run_data <- run_data[order(run_data$PPFD_mean), , drop = FALSE]
      colour <- run_data$colour[[1]]
      hover <- qc_trace_hover(run_data)
      raw_panel <- plotly::add_trace(
        raw_panel, x = run_data$PPFD_mean, y = run_data$A_mean,
        type = "scatter", mode = "lines+markers", name = run,
        line = list(color = colour, width = 1.7),
        marker = list(color = colour, size = 5),
        customdata = rep(run, nrow(run_data)), text = hover,
        hovertemplate = "%{text}<extra></extra>", showlegend = FALSE
      )
      positive_max <- suppressWarnings(max(run_data$A_mean[run_data$A_mean > 0], na.rm = TRUE))
      if (!is.finite(positive_max) || positive_max <= 0) {
        warnings <- c(warnings, sprintf(
          "%s has no positive A maximum and is omitted from normalized panels.", run
        ))
      } else {
        normalized <- run_data$A_mean / positive_max
        normalized_values <- c(normalized_values, normalized)
        normalized_panel <- plotly::add_trace(
          normalized_panel, x = run_data$PPFD_mean, y = normalized,
          type = "scatter", mode = "lines+markers", name = run,
          line = list(color = colour, width = 1.7),
          marker = list(color = colour, size = 5),
          customdata = rep(run, nrow(run_data)), text = hover,
          hovertemplate = paste0(
            "%{text}<br>Normalized A: %{y:.3f}<extra></extra>"
          ), showlegend = FALSE
        )
      }
    }
    raw_panel <- plotly::layout(
      raw_panel,
      xaxis = list(range = x_range, title = "Measured PPFD"),
      yaxis = list(range = raw_range, title = "A")
    )
    normalized_panel <- plotly::layout(
      normalized_panel,
      xaxis = list(range = x_range, title = "Measured PPFD"),
      yaxis = list(title = "A / positive run maximum")
    )
    panels <- c(panels, list(raw_panel, normalized_panel))
  }

  widget <- do.call(plotly::subplot, c(
    panels,
    list(nrows = 4L, margin = 0.055, shareX = TRUE, titleX = TRUE, titleY = TRUE)
  ))
  titles <- as.vector(t(rbind(
    paste(names(labels), "— original scale"),
    paste(names(labels), "— normalized")
  )))
  title_y <- rep(c(1, 0.75, 0.50, 0.25), each = 2L)
  title_x <- rep(c(0.23, 0.77), times = 4L)
  annotations <- lapply(seq_along(titles), function(index) list(
    text = titles[[index]], x = title_x[[index]], y = title_y[[index]],
    xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom",
    showarrow = FALSE, font = list(size = 13, color = "#33423a")
  ))
  widget <- plotly::layout(
    widget, showlegend = FALSE, hovermode = "closest",
    margin = list(l = 70, r = 25, t = 55, b = 55),
    annotations = annotations
  )
  widget <- plotly::event_register(widget, "plotly_click")
  list(widget = widget, warnings = unique(warnings))
}

make_qc_audit_plot <- function(entry) {
  if (is.null(entry) || !is.null(entry$error)) {
    return(plotly::plotly_empty(type = "scatter", mode = "lines"))
  }
  raw <- entry$extraction$raw
  summary <- entry$extraction$summary
  a_panel <- plotly::plot_ly(source = "qc_audit")
  a_panel <- plotly::add_lines(
    a_panel, x = raw$.elapsed_minutes, y = raw$A,
    name = "Raw A", line = list(color = "#4d5962", width = 1)
  )
  a_panel <- plotly::add_markers(
    a_panel,
    x = as.numeric(difftime(summary$window_end, min(raw$Datetime), units = "mins")),
    y = summary$A_mean, name = "Extracted A mean",
    marker = list(color = WALZ_DARK2[[1]], size = 8),
    text = sprintf(
      "Step %d<br>SD %.3f<br>Slope %.4f/min<br>Coverage %.0f%%<br>%s",
      summary$step_id, summary$A_sd, summary$A_slope,
      summary$coverage * 100, ifelse(nzchar(summary$warning), summary$warning, "Complete")
    ), hovertemplate = "%{text}<extra></extra>"
  )
  ppfd_panel <- plotly::plot_ly(source = "qc_audit")
  ppfd_panel <- plotly::add_lines(
    ppfd_panel, x = raw$.elapsed_minutes, y = raw$PARtop,
    name = "Raw PPFD", line = list(color = "#c6922d", width = 1.2)
  )
  for (index in seq_len(nrow(summary))) {
    x0 <- as.numeric(difftime(summary$window_start[[index]], min(raw$Datetime), units = "mins"))
    x1 <- as.numeric(difftime(summary$window_end[[index]], min(raw$Datetime), units = "mins"))
    a_panel <- plotly::layout(a_panel, shapes = c(a_panel$x$layout$shapes, list(list(
      type = "rect", x0 = x0, x1 = x1, y0 = 0, y1 = 1, yref = "paper",
      fillcolor = "rgba(40,117,77,0.10)", line = list(width = 0), layer = "below"
    ))))
  }
  plotly::subplot(a_panel, ppfd_panel, nrows = 2L, shareX = TRUE, titleY = TRUE) |>
    plotly::layout(
      showlegend = TRUE, legend = list(orientation = "h"),
      xaxis2 = list(title = "Elapsed time (minutes)"),
      yaxis = list(title = "A"), yaxis2 = list(title = "PPFD")
    )
}

quality_metadata_table_ui <- function(catalog, title = "Runs in this view") {
  if (is.null(catalog) || nrow(catalog) == 0L) {
    return(alert_ui("No runs are in the current view.", "info"))
  }
  catalog <- catalog[order(catalog$run_datetime, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  rows <- lapply(seq_len(nrow(catalog)), function(index) {
    row <- catalog[index, , drop = FALSE]
    shiny::tags$tr(
      style = sprintf(
        "border-left: .42rem solid %s; background:%s;",
        row$colour, hex_to_rgba(row$colour, 0.09)
      ),
      shiny::tags$td(shiny::span(
        class = "run-colour-swatch",
        style = sprintf("background:%s", row$colour)
      )),
      shiny::tags$td(row$run_id),
      shiny::tags$td(relative_run_date(row$run_id)),
      shiny::tags$td(ifelse(nzchar(row$species), row$species, "—")),
      shiny::tags$td(ifelse(nzchar(row$plant_id), row$plant_id, "—")),
      shiny::tags$td(tools::toTitleCase(row$quality)),
      shiny::tags$td(row$metadata_status)
    )
  })
  shiny::div(
    class = "full-metadata-scroll",
    shiny::h4(title),
    shiny::tags$table(
      class = "run-metadata-table full-metadata-table",
      shiny::tags$thead(shiny::tags$tr(lapply(
        c("Plot", "Run ID", "Date", "Species", "Plant ID", "Quality", "Metadata match"),
        shiny::tags$th
      ))),
      shiny::tags$tbody(rows)
    )
  )
}

qc_run_choices <- function(catalog) {
  if (is.null(catalog) || nrow(catalog) == 0L) return(character())
  labels <- sprintf(
    "%s — %s · %s · %s",
    catalog$run_id,
    ifelse(nzchar(catalog$species), catalog$species, "species unavailable"),
    ifelse(nzchar(catalog$plant_id), paste("plant", catalog$plant_id), "plant unavailable"),
    tools::toTitleCase(catalog$quality)
  )
  stats::setNames(as.character(catalog$id), labels)
}

manual_qc_catalog <- function(catalog, selected_ids) {
  selected_ids <- as.character(selected_ids)
  selected_ids <- selected_ids[!is.na(selected_ids) & nzchar(selected_ids)]
  if (is.null(catalog)) return(data.frame())
  if (nrow(catalog) == 0L || length(selected_ids) == 0L) {
    return(catalog[0, , drop = FALSE])
  }
  positions <- match(selected_ids, as.character(catalog$id))
  catalog[positions[!is.na(positions)], , drop = FALSE]
}

qc_selected_run_control_state <- function(catalog, current_run, previous_choices) {
  choices <- if (is.null(catalog) || nrow(catalog) == 0L) {
    character()
  } else {
    stats::setNames(catalog$run_id, catalog$run_id)
  }
  values <- unname(choices)
  current_run <- if (is.null(current_run) || length(current_run) == 0L) {
    ""
  } else {
    as.character(current_run[[1]])
  }
  desired_run <- if (nzchar(current_run) && current_run %in% values) {
    current_run
  } else if (length(values) > 0L) {
    values[[1]]
  } else {
    ""
  }
  previous_choices <- as.character(previous_choices)
  list(
    choices = choices,
    values = values,
    selected = if (nzchar(desired_run)) desired_run else character(),
    update = !identical(values, previous_choices) || !identical(current_run, desired_run)
  )
}

qc_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h5("Quality overview"),
    shiny::radioButtons(
      ns("selection_mode"), "How to choose runs",
      choices = c(
        "Filter by metadata" = "filters",
        "Choose runs manually" = "manual"
      ),
      selected = "filters"
    ),
    shiny::conditionalPanel(
      condition = "input.selection_mode === 'filters'",
      ns = ns,
      shiny::selectizeInput(ns("species"), "Species", choices = character(), multiple = TRUE),
      shiny::selectizeInput(ns("plant_ids"), "Plant IDs", choices = character(), multiple = TRUE),
      shiny::checkboxGroupInput(
        ns("qualities"), "Quality assessment",
        choices = c(
          "Good" = "good", "Medium" = "medium",
          "Bad" = "bad", "Unassessed" = "unassessed"
        ),
        selected = c("good", "medium", "bad", "unassessed")
      ),
      shiny::dateRangeInput(ns("date_range"), "Run date"),
      shiny::p(
        class = "control-help",
        "All four quality panels stay in place; these filters control which runs are compared."
      )
    ),
    shiny::conditionalPanel(
      condition = "input.selection_mode === 'manual'",
      ns = ns,
      shiny::selectizeInput(
        ns("manual_runs"), "Measurement runs",
        choices = character(), selected = character(), multiple = TRUE,
        options = list(
          plugins = list("remove_button"), closeAfterSelect = FALSE,
          hideSelected = TRUE, placeholder = "Select one or more runs"
        )
      ),
      shiny::p(
        class = "control-help",
        "Add any set of runs to compare. The menu stays open while you select multiple runs."
      )
    ),
    shiny::p(
      class = "control-help",
      "Quality labels are read from the metadata sheet and are not editable in this app."
    ),
    metadata_sheet_link_ui("metadata-card-link"),
    shiny::hr(),
    shiny::h5("Raw-run audit"),
    shiny::selectizeInput(ns("selected_run"), "Selected run", choices = character())
  )
}

qc_main_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "quality-control-tab",
    shiny::uiOutput(ns("progress")),
    bslib::card(
      bslib::card_header("A versus measured PPFD by quality assessment"),
      shiny::p(class = "control-help", "Each curve uses the final-three-minute means of its stable light steps. Click a curve to open its raw audit."),
      shiny::uiOutput(ns("overview_warnings")),
      plotly::plotlyOutput(ns("overview"), height = "1700px")
    ),
    bslib::card(
      bslib::card_header("Selected-run extraction audit"),
      shiny::uiOutput(ns("audit_heading")),
      plotly::plotlyOutput(ns("audit"), height = "620px"),
      shiny::uiOutput(ns("audit_table"))
    ),
    bslib::accordion(
      bslib::accordion_panel(
        "Metadata for plotted runs",
        shiny::uiOutput(ns("metadata_table"))
      )
    )
  )
}

quality_control_server <- function(
    id,
    active,
    measurements,
    metadata,
    config) {
  shiny::moduleServer(id, function(input, output, session) {
    prepared <- shiny::reactiveVal(list())
    progress <- shiny::reactiveVal(list(done = 0L, total = 0L, loading = FALSE, error = NULL))
    started <- shiny::reactiveVal(FALSE)
    manual_choice_signature <- shiny::reactiveVal(character())
    selected_run_choice_values <- shiny::reactiveVal(character())
    selected_entry_state <- shiny::reactiveVal(list(id = "", value = NULL))
    catalog <- shiny::reactive(build_run_catalog(measurements(), metadata()))

    shiny::observe({
      current <- catalog()
      if (nrow(current) == 0L) return()
      species <- sort(unique(current$species[nzchar(current$species)]))
      plants <- sort(unique(current$plant_id[nzchar(current$plant_id)]))
      shiny::updateSelectizeInput(session, "species", choices = species, server = TRUE)
      shiny::updateSelectizeInput(session, "plant_ids", choices = plants, server = TRUE)
      dates <- current$date[!is.na(current$date)]
      if (length(dates) > 0L) {
        shiny::updateDateRangeInput(session, "date_range", start = min(dates), end = max(dates))
      }
      manual_choices <- qc_run_choices(current)
      current_ids <- unname(manual_choices)
      choice_signature <- paste(names(manual_choices), current_ids, sep = "::")
      if (!identical(choice_signature, manual_choice_signature())) {
        selected <- if (is.null(input$manual_runs)) character() else input$manual_runs
        selected <- selected[selected %in% current_ids]
        shiny::freezeReactiveValue(input, "manual_runs")
        shiny::updateSelectizeInput(
          session, "manual_runs", choices = manual_choices,
          selected = selected, server = FALSE
        )
        manual_choice_signature(choice_signature)
      }
    })

    scoped_catalog <- shiny::reactive({
      current <- catalog()
      mode <- if (is.null(input$selection_mode)) "filters" else input$selection_mode
      if (identical(mode, "manual")) {
        return(manual_qc_catalog(current, input$manual_runs))
      }
      filter_run_catalog(
        current, input$species, input$plant_ids, input$date_range,
        qualities = input$qualities
      )
    })

    prepare_entry <- function(record, parsed = NULL, error = NULL) {
      if (!is.null(error)) return(list(record = record, value = NULL, extraction = NULL, error = error))
      extraction <- tryCatch(
        extract_cached_light_steps(record, parsed),
        error = function(error) error
      )
      if (inherits(extraction, "error")) {
        return(list(record = record, value = parsed, extraction = NULL, error = conditionMessage(extraction)))
      }
      list(record = record, value = parsed, extraction = extraction, error = NULL)
    }

    load_batch <- function(batch) {
      promise <- promises::future_promise({
        configure_drive_access("")
        batch_load_remote_measurements(batch)
      }, seed = TRUE)
      promises::then(
        promise,
        onFulfilled = function(results) {
          merge_measurement_batch_cache(batch, results)
          state <- prepared()
          for (result in results) {
            record <- batch[batch$id == result$id, , drop = FALSE]
            state[[result$id]] <- prepare_entry(
              record, result$value, result$error
            )
          }
          prepared(state)
          status <- progress()
          status$done <- min(status$total, status$done + nrow(batch))
          status$loading <- status$done < status$total
          progress(status)
        },
        onRejected = function(error) {
          status <- progress()
          status$done <- min(status$total, status$done + nrow(batch))
          status$loading <- status$done < status$total
          status$error <- paste(
            Filter(nzchar, c(status$error, conditionMessage(error))),
            collapse = "; "
          )
          progress(status)
        }
      )
      invisible(NULL)
    }

    start_loading <- function() {
      records <- measurements()
      if (is.null(records) || nrow(records) == 0L || isTRUE(started())) return()
      started(TRUE)
      ensure_background_workers(config$background_workers)
      state <- prepared()
      pending <- list()
      for (index in seq_len(nrow(records))) {
        record <- records[index, , drop = FALSE]
        cached <- get_remote_cached("measurement", record)
        if (!is.null(cached)) {
          state[[record$id[[1]]]] <- prepare_entry(record, cached)
        } else {
          pending <- c(pending, index)
        }
      }
      prepared(state)
      pending_records <- records[unlist(pending), , drop = FALSE]
      progress(list(
        done = nrow(records) - nrow(pending_records), total = nrow(records),
        loading = nrow(pending_records) > 0L, error = NULL
      ))
      if (nrow(pending_records) > 0L) {
        groups <- split(
          seq_len(nrow(pending_records)),
          ceiling(seq_len(nrow(pending_records)) / 12L)
        )
        lapply(groups, function(rows) {
          load_batch(pending_records[rows, , drop = FALSE])
        })
      }
    }

    shiny::observeEvent(active(), {
      if (isTRUE(active())) start_loading()
    }, ignoreInit = FALSE)

    output$progress <- shiny::renderUI({
      state <- progress()
      if (!is.null(state$error)) return(alert_ui(state$error, "danger"))
      if (!isTRUE(started())) return(alert_ui("Preparing Quality Control when this tab opens …", "info"))
      if (isTRUE(state$loading)) {
        return(shiny::div(
          class = "walz-progress",
          shiny::div(class = "progress", shiny::div(
            class = "progress-bar", role = "progressbar",
            style = sprintf("width: %.1f%%", 100 * state$done / max(1, state$total))
          )),
          shiny::p(sprintf("%d of %d runs prepared", state$done, state$total))
        ))
      }
      alert_ui(sprintf("%d runs prepared and cached for this process.", state$done), "info")
    })

    step_data <- shiny::reactive(prepared_step_table(prepared(), scoped_catalog()))
    overview_result <- shiny::reactive(make_qc_overview_plot(step_data()))

    output$overview <- plotly::renderPlotly(overview_result()$widget)
    output$overview_warnings <- shiny::renderUI({
      warnings <- overview_result()$warnings
      failures <- Filter(function(entry) !is.null(entry$error), prepared())
      if (length(failures) > 0L) {
        failure_labels <- vapply(failures, function(entry) {
          sprintf("%s (%s)", entry$record$name[[1]], entry$error)
        }, character(1))
        warnings <- c(warnings, sprintf(
          "%d run(s) could not be prepared: %s",
          length(failures), paste(failure_labels, collapse = "; ")
        ))
      }
      invalid <- scoped_catalog()$run_id[scoped_catalog()$quality_invalid]
      if (length(invalid) > 0L) warnings <- c(warnings, sprintf(
        "Unexpected quality values are grouped as Unassessed: %s.",
        paste(invalid, collapse = ", ")
      ))
      if (length(warnings) == 0L) return(NULL)
      shiny::tagList(lapply(unique(warnings), alert_ui, level = "warning"))
    })

    shiny::observe({
      current <- scoped_catalog()
      state <- qc_selected_run_control_state(
        current, input$selected_run, selected_run_choice_values()
      )
      if (!isTRUE(state$update)) return()
      shiny::updateSelectizeInput(
        session, "selected_run", choices = state$choices,
        selected = state$selected, server = TRUE
      )
      selected_run_choice_values(state$values)
    })

    qc_click <- shiny::reactive({
      shiny::req(active())
      suppressWarnings(plotly::event_data("plotly_click", source = "qc_overview"))
    })
    shiny::observeEvent(qc_click(), {
      clicked <- qc_click()
      run_id <- as.character(clicked$customdata[[1]])
      if (nzchar(run_id)) shiny::updateSelectizeInput(session, "selected_run", selected = run_id)
    }, ignoreInit = TRUE)

    selected_catalog <- shiny::reactive({
      current <- scoped_catalog()
      selected_run <- input$selected_run
      if (is.null(selected_run) || !nzchar(selected_run)) {
        selected_run <- if (nrow(current) > 0L) current$run_id[[1]] else ""
      }
      current[current$run_id == selected_run, , drop = FALSE]
    })
    selected_entry <- shiny::reactive({
      selected_entry_state()$value
    })
    shiny::observe({
      selected <- selected_catalog()
      selected_id <- if (nrow(selected) == 1L) as.character(selected$id[[1]]) else ""
      candidate <- if (nzchar(selected_id)) prepared()[[selected_id]] else NULL
      previous <- selected_entry_state()
      if (!identical(previous$id, selected_id) || !identical(previous$value, candidate)) {
        selected_entry_state(list(id = selected_id, value = candidate))
      }
    })

    output$audit <- plotly::renderPlotly(make_qc_audit_plot(selected_entry()))
    output$audit_heading <- shiny::renderUI({
      selected <- selected_catalog()
      if (nrow(selected) != 1L) return(alert_ui("Select a run for the audit.", "info"))
      shiny::tagList(
        shiny::h4(selected$run_id),
        shiny::p(sprintf(
          "%s · %s · Quality: %s",
          ifelse(nzchar(selected$species), selected$species, "Species unavailable"),
          ifelse(nzchar(selected$plant_id), selected$plant_id, "Plant ID unavailable"),
          tools::toTitleCase(selected$quality)
        )),
        if (selected$metadata_matches != 1L) alert_ui(
          "This run does not have exactly one matching metadata row.", "warning"
        )
      )
    })
    output$audit_table <- shiny::renderUI({
      entry <- selected_entry()
      if (is.null(entry) || !is.null(entry$error)) {
        return(if (!is.null(entry$error)) alert_ui(entry$error, "danger") else NULL)
      }
      table <- entry$extraction$summary[, c(
        "step_id", "PPFD_mean", "A_mean", "A_sd", "A_slope",
        "n_window", "coverage", "window_complete", "warning"
      ), drop = FALSE]
      table$coverage <- sprintf("%.0f%%", 100 * table$coverage)
      table$window_complete <- ifelse(table$window_complete, "Yes", "No")
      shiny::div(class = "full-metadata-scroll", shiny::tags$table(
        class = "run-metadata-table",
        shiny::tags$thead(shiny::tags$tr(lapply(
          c("Step", "PPFD mean", "A mean", "A SD", "A slope/min", "n", "Coverage", "Complete", "Warning"),
          shiny::tags$th
        ))),
        shiny::tags$tbody(lapply(seq_len(nrow(table)), function(row) {
          shiny::tags$tr(lapply(table[row, , drop = TRUE], function(value) {
            shiny::tags$td(ifelse(is.na(value) || !nzchar(as.character(value)), "—", as.character(value)))
          }))
        }))
      ))
    })
    output$metadata_table <- shiny::renderUI(quality_metadata_table_ui(scoped_catalog()))

    list(prepared = prepared, catalog = catalog)
  })
}
