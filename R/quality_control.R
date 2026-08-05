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
      !tolower(quality_raw) %in% WALZ_QUALITY_SOURCE_LEVELS
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

qc_empty_timeseries_data <- function() {
  list(
    raw = data.frame(
      run_id = character(), elapsed_minutes = numeric(), A_raw = numeric(),
      A_normalized = numeric(), hover = character(), stringsAsFactors = FALSE
    ),
    windows = data.frame(
      run_id = character(), step_id = integer(), window_start = numeric(),
      window_end = numeric(), window_midpoint = numeric(), A_mean = numeric(),
      A_sd = numeric(), A_mean_normalized = numeric(), A_sd_normalized = numeric(),
      PPFD_mean = numeric(), coverage = numeric(),
      stringsAsFactors = FALSE
    ),
    warnings = character(), normalization_warnings = character(),
    run_ids = character()
  )
}

prepare_qc_timeseries_data <- function(prepared, catalog) {
  empty <- qc_empty_timeseries_data()
  if (length(prepared) == 0L || is.null(catalog) || nrow(catalog) == 0L) {
    return(empty)
  }

  raw_rows <- list()
  window_rows <- list()
  warnings <- character()
  normalization_warnings <- character()
  run_ids <- character()
  for (index in seq_len(nrow(catalog))) {
    run_id <- as.character(catalog$run_id[[index]])
    entry <- prepared[[catalog$id[[index]]]]
    if (is.null(entry) || !is.null(entry$error) || is.null(entry$extraction)) next
    raw <- entry$extraction$raw
    summary <- entry$extraction$summary
    if (
      is.null(raw) || nrow(raw) == 0L ||
        !all(c("Datetime", ".elapsed_minutes", "A") %in% names(raw))
    ) {
      warnings <- c(warnings, sprintf("%s has no usable raw A timeseries.", run_id))
      next
    }

    elapsed <- suppressWarnings(as.numeric(raw$.elapsed_minutes))
    assimilation <- suppressWarnings(as.numeric(raw$A))
    keep <- is.finite(elapsed) & is.finite(assimilation)
    if (!any(keep)) {
      warnings <- c(warnings, sprintf("%s has no finite A observations.", run_id))
      next
    }
    positive_values <- assimilation[keep & assimilation > 0]
    positive_max <- if (length(positive_values)) max(positive_values) else NA_real_
    if (!is.finite(positive_max) || positive_max <= 0) {
      normalization_warnings <- c(normalization_warnings, sprintf(
        "%s has no positive A maximum and is omitted from normalized view.", run_id
      ))
    }
    run_ids <- c(run_ids, run_id)
    raw_rows[[run_id]] <- data.frame(
      run_id = run_id,
      elapsed_minutes = elapsed[keep],
      A_raw = assimilation[keep],
      A_normalized = assimilation[keep] / positive_max,
      hover = sprintf(
        "<b>%s</b><br>Elapsed time: %.2f min<br>A: %.3f µmol m⁻² s⁻¹",
        run_id, elapsed[keep], assimilation[keep]
      ),
      stringsAsFactors = FALSE
    )

    if (is.null(summary) || nrow(summary) == 0L) next
    start_time <- suppressWarnings(min(raw$Datetime, na.rm = TRUE))
    if (!is.finite(as.numeric(start_time))) next
    window_start <- as.numeric(difftime(summary$window_start, start_time, units = "mins"))
    window_end <- as.numeric(difftime(summary$window_end, start_time, units = "mins"))
    window_rows[[run_id]] <- data.frame(
      run_id = run_id,
      step_id = summary$step_id,
      window_start = window_start,
      window_end = window_end,
      window_midpoint = (window_start + window_end) / 2,
      A_mean = summary$A_mean,
      A_sd = summary$A_sd,
      A_mean_normalized = summary$A_mean / positive_max,
      A_sd_normalized = summary$A_sd / positive_max,
      PPFD_mean = summary$PPFD_mean,
      coverage = summary$coverage,
      stringsAsFactors = FALSE
    )
  }

  if (length(raw_rows) == 0L) {
    empty$warnings <- unique(warnings)
    empty$normalization_warnings <- unique(normalization_warnings)
    return(empty)
  }
  raw_data <- do.call(rbind, raw_rows)
  window_data <- if (length(window_rows)) do.call(rbind, window_rows) else empty$windows
  raw_data$run_id <- factor(raw_data$run_id, levels = run_ids)
  if (nrow(window_data) > 0L) {
    window_data$run_id <- factor(window_data$run_id, levels = run_ids)
  }
  list(
    raw = raw_data, windows = window_data,
    warnings = unique(warnings),
    normalization_warnings = unique(normalization_warnings),
    run_ids = run_ids
  )
}

qc_display_run_ids <- function(prepared, normalized = FALSE) {
  if (is.null(prepared) || nrow(prepared$raw) == 0L) return(character())
  if (!isTRUE(normalized)) return(prepared$run_ids)
  unique(as.character(prepared$raw$run_id[is.finite(prepared$raw$A_normalized)]))
}

qc_a_axis_label <- function(normalized = FALSE) {
  if (isTRUE(normalized)) {
    "A / positive run maximum"
  } else {
    "Net CO₂ assimilation, A [µmol m⁻² s⁻¹]"
  }
}

qc_facet_columns <- function(value) {
  value <- suppressWarnings(as.integer(value))
  if (length(value) == 0L || is.na(value)) return(2L)
  max(1L, min(4L, value[[1]]))
}

qc_timeseries_plot_height <- function(run_count, columns) {
  rows <- ceiling(max(1L, as.integer(run_count)) / qc_facet_columns(columns))
  max(380L, 245L * rows + 105L)
}

qc_metadata_sheet_url <- function(sheet_id) {
  sprintf("https://docs.google.com/spreadsheets/d/%s/edit", sheet_id)
}

make_qc_timeseries_plot <- function(
    prepared,
    columns = 2L,
    normalized = FALSE,
    metadata_sheet_id = WALZ_DEFAULT_METADATA_SHEET_ID) {
  if (is.null(prepared) || nrow(prepared$raw) == 0L) {
    return(plotly::plotly_empty(type = "scatter", mode = "lines"))
  }
  columns <- qc_facet_columns(columns)
  mean_colour <- WALZ_DARK2[[1]]
  display_runs <- qc_display_run_ids(prepared, normalized)
  if (length(display_runs) == 0L) {
    return(plotly::plotly_empty(type = "scatter", mode = "lines"))
  }
  raw_data <- prepared$raw[as.character(prepared$raw$run_id) %in% display_runs, , drop = FALSE]
  window_data <- prepared$windows[
    as.character(prepared$windows$run_id) %in% display_runs, , drop = FALSE
  ]
  raw_data$A_display <- if (isTRUE(normalized)) raw_data$A_normalized else raw_data$A_raw
  window_data$A_display <- if (isTRUE(normalized)) {
    window_data$A_mean_normalized
  } else window_data$A_mean
  window_data$A_sd_display <- if (isTRUE(normalized)) {
    window_data$A_sd_normalized
  } else window_data$A_sd
  y_label <- qc_a_axis_label(normalized)
  raw_line_layer <- suppressWarnings(ggplot2::geom_line(
    ggplot2::aes(text = hover, key = run_id),
    colour = "#56616b", linewidth = 0.38, na.rm = TRUE
  ))
  plot <- ggplot2::ggplot(
    raw_data,
    ggplot2::aes(x = elapsed_minutes, y = A_display, group = run_id)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "#a8b0aa", linewidth = 0.3) +
    ggplot2::geom_rect(
      data = window_data,
      ggplot2::aes(
        xmin = window_start, xmax = window_end,
        ymin = -Inf, ymax = Inf
      ),
      inherit.aes = FALSE, fill = mean_colour, alpha = 0.055
    ) +
    raw_line_layer +
    ggplot2::geom_segment(
      data = window_data,
      ggplot2::aes(
        x = window_start, xend = window_end,
        y = A_display, yend = A_display
      ),
      inherit.aes = FALSE, colour = mean_colour, linewidth = 1.7,
      lineend = "round", na.rm = TRUE
    ) +
    ggplot2::geom_errorbar(
      data = window_data,
      ggplot2::aes(
        x = window_midpoint,
        ymin = A_display - A_sd_display,
        ymax = A_display + A_sd_display
      ),
      inherit.aes = FALSE, colour = mean_colour, alpha = 0.58,
      linewidth = 0.55, width = 0.5, na.rm = TRUE
    ) +
    ggplot2::facet_wrap(
      stats::as.formula("~run_id"), ncol = columns, scales = "free_x"
    ) +
    ggplot2::labs(
      x = "Elapsed time from run start (minutes)",
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#e7ebe7", linewidth = 0.3),
      strip.background = ggplot2::element_rect(fill = "#eef4ed", colour = NA),
      strip.text = ggplot2::element_text(colour = "#006d4c", face = "bold"),
      axis.title = ggplot2::element_text(colour = "#34443a"),
      axis.text = ggplot2::element_text(colour = "#4b5563")
    )

  widget <- suppressWarnings(plotly::ggplotly(
    plot, dynamicTicks = TRUE, tooltip = "text", source = "qc_overview"
  ))
  sheet_url <- qc_metadata_sheet_url(metadata_sheet_id)
  annotations <- widget$x$layout$annotations
  if (length(annotations)) {
    for (index in seq_along(annotations)) {
      label <- as.character(annotations[[index]]$text)
      if (!label %in% display_runs) next
      annotations[[index]]$text <- sprintf(
        "<a href=\"%s\" target=\"_blank\">%s ↗</a>",
        sheet_url, htmltools::htmlEscape(label)
      )
      annotations[[index]]$name <- label
      annotations[[index]]$captureevents <- TRUE
      annotations[[index]]$hovertext <- "Open the metadata sheet to review this run"
      annotations[[index]]$font$color <- "#006d4c"
    }
    widget$x$layout$annotations <- annotations
  }
  widget <- plotly::layout(
    widget, hovermode = "closest", showlegend = FALSE,
    margin = list(l = 90, r = 25, t = 35, b = 70)
  )
  widget <- plotly::event_register(widget, "plotly_click")
  plotly::event_register(widget, "plotly_clickannotation")
}

make_qc_audit_plot <- function(entry) {
  if (is.null(entry) || !is.null(entry$error)) {
    return(plotly::plotly_empty(type = "scatter", mode = "lines"))
  }
  raw <- entry$extraction$raw
  summary <- entry$extraction$summary
  start_time <- min(raw$Datetime, na.rm = TRUE)
  window_start <- as.numeric(difftime(summary$window_start, start_time, units = "mins"))
  window_end <- as.numeric(difftime(summary$window_end, start_time, units = "mins"))
  window_midpoint <- (window_start + window_end) / 2
  overlay_colour <- WALZ_DARK2[[1]]
  overlay_error_colour <- hex_to_rgba(overlay_colour, 0.58)
  hover_status <- ifelse(
    nzchar(summary$warning), summary$warning,
    ifelse(summary$window_complete, "Complete", "Incomplete")
  )
  a_panel <- plotly::plot_ly(source = "qc_audit")
  a_panel <- plotly::add_lines(
    a_panel, x = raw$.elapsed_minutes, y = raw$A,
    name = "Raw A", line = list(color = "#4d5962", width = 1)
  )
  a_panel <- plotly::add_segments(
    a_panel,
    x = window_start, xend = window_end,
    y = summary$A_mean, yend = summary$A_mean,
    name = "3-minute mean",
    line = list(color = overlay_colour, width = 6),
    text = sprintf(
      "Step %d<br>A mean %.3f<br>A SD %.3f<br>Slope %.4f/min<br>Coverage %.0f%%<br>%s",
      summary$step_id, summary$A_mean, summary$A_sd, summary$A_slope,
      summary$coverage * 100, hover_status
    ), hovertemplate = "%{text}<extra></extra>"
  )
  a_panel <- plotly::add_markers(
    a_panel, x = window_midpoint, y = summary$A_mean,
    name = "Mean ± 1 SD",
    marker = list(color = overlay_colour, size = 5),
    error_y = list(
      type = "data", array = summary$A_sd, visible = TRUE,
      color = overlay_error_colour, thickness = 1.6, width = 5
    ),
    text = sprintf(
      "Step %d<br>A mean %.3f ± %.3f SD<br>Coverage %.0f%%<br>%s",
      summary$step_id, summary$A_mean, summary$A_sd,
      summary$coverage * 100, hover_status
    ), hovertemplate = "%{text}<extra></extra>"
  )
  ppfd_panel <- plotly::plot_ly(source = "qc_audit")
  ppfd_panel <- plotly::add_lines(
    ppfd_panel, x = raw$.elapsed_minutes, y = raw$PARtop,
    name = "Raw PPFD", line = list(color = "#c6922d", width = 1.2)
  )
  ppfd_panel <- plotly::add_segments(
    ppfd_panel,
    x = window_start, xend = window_end,
    y = summary$PPFD_mean, yend = summary$PPFD_mean,
    name = "3-minute mean", showlegend = FALSE,
    line = list(color = overlay_colour, width = 6),
    text = sprintf(
      "Step %d<br>PPFD mean %.1f<br>PPFD SD %.1f<br>Coverage %.0f%%<br>%s",
      summary$step_id, summary$PPFD_mean, summary$PPFD_sd,
      summary$coverage * 100, hover_status
    ), hovertemplate = "%{text}<extra></extra>"
  )
  ppfd_panel <- plotly::add_markers(
    ppfd_panel, x = window_midpoint, y = summary$PPFD_mean,
    name = "Mean ± 1 SD", showlegend = FALSE,
    marker = list(color = overlay_colour, size = 5),
    error_y = list(
      type = "data", array = summary$PPFD_sd, visible = TRUE,
      color = overlay_error_colour, thickness = 1.6, width = 5
    ),
    text = sprintf(
      "Step %d<br>PPFD mean %.1f ± %.1f SD<br>Coverage %.0f%%<br>%s",
      summary$step_id, summary$PPFD_mean, summary$PPFD_sd,
      summary$coverage * 100, hover_status
    ), hovertemplate = "%{text}<extra></extra>"
  )
  for (index in seq_len(nrow(summary))) {
    x0 <- window_start[[index]]
    x1 <- window_end[[index]]
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

format_qc_audit_table <- function(summary) {
  if (is.null(summary) || nrow(summary) == 0L) return(data.frame())

  fixed <- function(values, digits) {
    values <- suppressWarnings(as.numeric(values))
    ifelse(
      is.finite(values),
      formatC(values, format = "f", digits = digits),
      "—"
    )
  }
  integers <- function(values) {
    values <- suppressWarnings(as.integer(values))
    ifelse(is.na(values), "—", as.character(values))
  }
  warnings <- trimws(as.character(summary$warning))
  warnings[is.na(warnings) | !nzchar(warnings)] <- "—"

  data.frame(
    step_id = integers(summary$step_id),
    PPFD_mean = fixed(summary$PPFD_mean, 1L),
    A_mean = fixed(summary$A_mean, 3L),
    A_sd = fixed(summary$A_sd, 3L),
    A_slope = fixed(summary$A_slope, 4L),
    n_window = integers(summary$n_window),
    coverage = ifelse(
      is.finite(summary$coverage),
      sprintf("%.0f%%", 100 * summary$coverage),
      "—"
    ),
    window_complete = ifelse(
      is.na(summary$window_complete),
      "—",
      ifelse(summary$window_complete, "Yes", "No")
    ),
    warning = warnings,
    stringsAsFactors = FALSE
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

qc_default_date_range <- function(
    now = Sys.time(),
    timezone = WALZ_TIMEZONE) {
  today <- as.Date(format(now, "%Y-%m-%d", tz = timezone))
  c(as.Date("2026-07-01"), today)
}

qc_selection_signature <- function(
    mode,
    species = character(),
    plant_ids = character(),
    date_range = NULL,
    qualities = character(),
    manual_ids = character()) {
  clean <- function(values) {
    values <- trimws(as.character(values))
    sort(unique(values[!is.na(values) & nzchar(values)]))
  }
  dates <- if (is.null(date_range)) character() else {
    format(as.Date(date_range), "%Y-%m-%d")
  }
  paste(
    as.character(mode),
    paste(clean(species), collapse = ","),
    paste(clean(plant_ids), collapse = ","),
    paste(dates, collapse = ":"),
    paste(clean(qualities), collapse = ","),
    paste(clean(manual_ids), collapse = ","),
    sep = "|"
  )
}

qc_catalog_signature <- function(catalog) {
  if (is.null(catalog) || nrow(catalog) == 0L) return("")
  columns <- intersect(
    c(
      "id", "modified_iso", "quality", "species", "plant_id",
      "metadata_matches", "metadata_status"
    ),
    names(catalog)
  )
  rows <- apply(catalog[, columns, drop = FALSE], 1L, function(row) {
    paste(as.character(row), collapse = "::")
  })
  paste(sort(rows), collapse = "|")
}

qc_record_version_key <- function(record) {
  modified <- if ("modified_iso" %in% names(record)) {
    as.character(record$modified_iso[[1]])
  } else if ("modified_time" %in% names(record)) {
    as.character(record$modified_time[[1]])
  } else {
    ""
  }
  paste(as.character(record$id[[1]]), modified, sep = "@")
}

qc_entry_matches_record <- function(entry, record) {
  !is.null(entry) && !is.null(entry$record) &&
    identical(qc_record_version_key(entry$record), qc_record_version_key(record))
}

qc_prepared_count <- function(prepared, records) {
  if (is.null(records) || nrow(records) == 0L) return(0L)
  sum(vapply(seq_len(nrow(records)), function(index) {
    record <- records[index, , drop = FALSE]
    qc_entry_matches_record(prepared[[as.character(record$id[[1]])]], record)
  }, logical(1)))
}

qc_failed_count <- function(prepared, records) {
  if (is.null(records) || nrow(records) == 0L) return(0L)
  sum(vapply(seq_len(nrow(records)), function(index) {
    record <- records[index, , drop = FALSE]
    entry <- prepared[[as.character(record$id[[1]])]]
    qc_entry_matches_record(entry, record) && !is.null(entry$error)
  }, logical(1)))
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
  default_dates <- qc_default_date_range()
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
      shiny::dateRangeInput(
        ns("date_range"), "Run date",
        start = default_dates[[1]],
        end = default_dates[[2]]
      ),
      shiny::p(
        class = "control-help",
        "All four quality sections stay in place; these filters control which runs are compared."
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
    shiny::div(
      class = "qc-analysis-actions",
      bslib::input_task_button(
        ns("analyze_filtered"), "Analyze filtered runs",
        icon = shiny::icon("filter"),
        label_busy = "Analysis running …",
        type = "primary",
        auto_reset = FALSE
      ),
      bslib::input_task_button(
        ns("analyze_all"), "Analyze all runs",
        icon = shiny::icon("layer-group"),
        label_busy = "Analysis running …",
        type = "secondary",
        auto_reset = FALSE
      )
    ),
    shiny::p(
      class = "control-help",
      paste0(
        "No curves are prepared automatically. Apply the current filters with the first button, ",
        "or ignore them and prepare every run with Analyze all runs."
      )
    ),
    shiny::sliderInput(
      ns("facet_columns"), "Timeseries columns",
      min = 1L, max = 4L, value = 2L, step = 1L
    ),
    shiny::p(
      class = "control-help",
      "Choose how many run panels are shown per row in each quality section."
    ),
    bslib::input_switch(
      ns("normalize_a"), "Normalize A by each run's positive maximum",
      value = FALSE
    ),
    shiny::p(
      class = "control-help",
      "Off by default: raw A values and units are shown."
    ),
    shiny::p(
      class = "control-help",
      "Quality labels are read from the metadata sheet and are not editable in this app."
    ),
    metadata_sheet_link_ui("metadata-card-link"),
    shiny::hr(),
    shiny::h5("Raw-run audit"),
    shiny::selectizeInput(ns("selected_run"), "Selected run", choices = character()),
    shiny::p(
      class = "control-help",
      "This list contains every measurement CSV in the Drive folder, independent of the overview filters."
    )
  )
}

qc_main_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "quality-control-tab",
    shiny::uiOutput(ns("progress")),
    shiny::uiOutput(ns("scope_notice")),
    bslib::card(
      bslib::card_header("A over elapsed time by quality assessment"),
      shiny::p(
        class = "control-help",
        paste0(
          "Each facet is one measurement run. The sidebar scale switch defaults to original A ",
          "and can divide each run by its positive maximum. Pale bands mark the ",
          "three-minute extraction windows; thick green segments show their means and the ",
          "lighter vertical bars show mean ± 1 SD. Click a line to open its detailed audit, ",
          "or click a run-ID title to open the metadata Sheet."
        )
      ),
      shiny::uiOutput(ns("overview_warnings"))
    ),
    shiny::uiOutput(ns("overview_sections")),
    bslib::card(
      bslib::card_header("Selected-run extraction audit"),
      shiny::uiOutput(ns("audit_heading")),
      plotly::plotlyOutput(ns("audit"), height = "620px"),
      shiny::uiOutput(ns("audit_table"))
    ),
    bslib::accordion(
      open = FALSE,
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
    loading_keys <- shiny::reactiveVal(character())
    requested_records <- shiny::reactiveVal(NULL)
    analyzed_catalog <- shiny::reactiveVal(NULL)
    analysis_mode <- shiny::reactiveVal("")
    analyzed_filter_signature <- shiny::reactiveVal("")
    analyzed_source_signature <- shiny::reactiveVal("")
    manual_choice_signature <- shiny::reactiveVal(character())
    selected_run_choice_values <- shiny::reactiveVal(character())
    selected_run_id <- shiny::reactiveVal("")
    selected_entry_state <- shiny::reactiveVal(list(id = "", value = NULL))
    catalog <- shiny::reactive(build_run_catalog(measurements(), metadata()))

    shiny::observe({
      current <- catalog()
      if (nrow(current) == 0L) return()
      species <- sort(unique(current$species[nzchar(current$species)]))
      plants <- sort(unique(current$plant_id[nzchar(current$plant_id)]))
      shiny::updateSelectizeInput(session, "species", choices = species, server = TRUE)
      shiny::updateSelectizeInput(session, "plant_ids", choices = plants, server = TRUE)
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

    current_filter_signature <- shiny::reactive({
      qc_selection_signature(
        mode = if (is.null(input$selection_mode)) "filters" else input$selection_mode,
        species = input$species,
        plant_ids = input$plant_ids,
        date_range = input$date_range,
        qualities = input$qualities,
        manual_ids = input$manual_runs
      )
    })

    display_catalog <- shiny::reactive({
      current <- analyzed_catalog()
      if (is.null(current)) catalog()[0, , drop = FALSE] else current
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

    set_analysis_button_state <- function(state) {
      bslib::update_task_button(
        "analyze_filtered", state = state, session = session
      )
      bslib::update_task_button(
        "analyze_all", state = state, session = session
      )
    }

    update_loading_progress <- function(error = NULL) {
      records <- requested_records()
      total <- if (is.null(records)) 0L else nrow(records)
      current_keys <- if (total == 0L) character() else vapply(
        seq_len(total),
        function(index) qc_record_version_key(records[index, , drop = FALSE]),
        character(1)
      )
      active_keys <- intersect(loading_keys(), current_keys)
      loading_keys(active_keys)
      state <- prepared()
      failure_messages <- unlist(lapply(seq_len(total), function(index) {
        record <- records[index, , drop = FALSE]
        entry <- state[[as.character(record$id[[1]])]]
        if (!qc_entry_matches_record(entry, record) || is.null(entry$error)) {
          return(character())
        }
        as.character(entry$error)
      }))
      messages <- Filter(nzchar, c(failure_messages, error))
      done <- qc_prepared_count(state, records)
      loading <- length(active_keys) > 0L || done < total
      progress(list(
        done = done,
        total = total,
        loading = loading,
        error = if (length(messages)) paste(unique(messages), collapse = "; ") else NULL
      ))
      if (!loading) set_analysis_button_state("ready")
    }

    finish_batch <- function(batch) {
      batch_keys <- vapply(
        seq_len(nrow(batch)),
        function(index) qc_record_version_key(batch[index, , drop = FALSE]),
        character(1)
      )
      loading_keys(setdiff(loading_keys(), batch_keys))
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
          finish_batch(batch)
          update_loading_progress()
        },
        onRejected = function(error) {
          message <- conditionMessage(error)
          state <- prepared()
          for (index in seq_len(nrow(batch))) {
            record <- batch[index, , drop = FALSE]
            state[[record$id[[1]]]] <- prepare_entry(record, error = message)
          }
          prepared(state)
          finish_batch(batch)
          update_loading_progress(message)
        }
      )
      invisible(NULL)
    }

    start_loading <- function(records) {
      started(TRUE)
      requested_records(records)
      if (is.null(records) || nrow(records) == 0L) {
        progress(list(
          done = 0L, total = 0L, loading = FALSE,
          error = "No runs match the current Quality Control selection."
        ))
        set_analysis_button_state("ready")
        return()
      }
      state <- prepared()
      active_keys <- loading_keys()
      pending <- integer()
      for (index in seq_len(nrow(records))) {
        record <- records[index, , drop = FALSE]
        id <- as.character(record$id[[1]])
        key <- qc_record_version_key(record)
        if (qc_entry_matches_record(state[[id]], record) || key %in% active_keys) {
          next
        }
        cached <- get_remote_cached("measurement", record)
        if (!is.null(cached)) {
          state[[id]] <- prepare_entry(record, cached)
        } else {
          pending <- c(pending, index)
        }
      }
      prepared(state)

      pending_records <- records[pending, , drop = FALSE]
      if (nrow(pending_records) > 0L) {
        pending_keys <- vapply(
          seq_len(nrow(pending_records)),
          function(index) qc_record_version_key(pending_records[index, , drop = FALSE]),
          character(1)
        )
        loading_keys(unique(c(loading_keys(), pending_keys)))
      }
      update_loading_progress()

      if (nrow(pending_records) > 0L) {
        ensure_background_workers(config$background_workers)
        groups <- split(
          seq_len(nrow(pending_records)),
          ceiling(seq_len(nrow(pending_records)) / 12L)
        )
        lapply(groups, function(rows) {
          load_batch(pending_records[rows, , drop = FALSE])
        })
      }
    }

    begin_analysis <- function(selection, mode) {
      if (isTRUE(progress()$loading)) {
        set_analysis_button_state("busy")
        shiny::showNotification(
          "Quality Control analysis is already running.", type = "message"
        )
        return()
      }
      analyzed_catalog(selection)
      analysis_mode(mode)
      analyzed_filter_signature(current_filter_signature())
      analyzed_source_signature(qc_catalog_signature(selection))
      records <- measurements()
      records <- records[
        match(as.character(selection$id), as.character(records$id)),
        ,
        drop = FALSE
      ]
      records <- records[!is.na(records$id), , drop = FALSE]
      set_analysis_button_state("busy")
      start_loading(records)
    }

    shiny::observeEvent(input$analyze_filtered, {
      begin_analysis(scoped_catalog(), "filtered")
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$analyze_all, {
      begin_analysis(catalog(), "all")
    }, ignoreInit = TRUE)

    output$progress <- shiny::renderUI({
      state <- progress()
      if (!isTRUE(started())) {
        return(alert_ui(
          paste0(
            "Ready. Adjust the filters, then choose Analyze filtered runs, ",
            "or choose Analyze all runs."
          ),
          "info"
        ))
      }
      if (state$total == 0L && !is.null(state$error)) {
        return(alert_ui(state$error, "warning"))
      }
      scope_label <- if (identical(analysis_mode(), "all")) {
        "the all-run selection"
      } else {
        "the filtered selection"
      }
      if (isTRUE(state$loading)) {
        return(shiny::tagList(
          shiny::div(
            class = "walz-progress", `aria-live` = "polite",
            shiny::div(class = "progress", shiny::div(
              class = "progress-bar", role = "progressbar",
              `aria-valuemin` = 0, `aria-valuemax` = state$total,
              `aria-valuenow` = state$done,
              style = sprintf("width: %.1f%%", 100 * state$done / max(1, state$total))
            )),
            shiny::p(sprintf(
              "Analyzing %s: %d of %d runs checked",
              scope_label, state$done, state$total
            ))
          ),
          if (!is.null(state$error)) alert_ui(state$error, "warning")
        ))
      }
      failures <- qc_failed_count(prepared(), requested_records())
      shiny::tagList(
        alert_ui(if (failures == 0L) {
          sprintf(
            "%d runs from %s are ready and cached for this process.",
            state$done, scope_label
          )
        } else {
          sprintf(
            paste0(
              "%d runs from %s are ready; %d failed. ",
              "Results are cached for this process."
            ),
            max(0L, state$done - failures), scope_label, failures
          )
        }, "info"),
        if (!is.null(state$error)) alert_ui(state$error, "warning")
      )
    })

    output$scope_notice <- shiny::renderUI({
      selection <- analyzed_catalog()
      if (is.null(selection)) return(NULL)
      notices <- list()
      if (
        identical(analysis_mode(), "filtered") &&
          !identical(analyzed_filter_signature(), current_filter_signature())
      ) {
        notices <- c(notices, list(alert_ui(
          paste0(
            "The filters have changed. The current overview still shows the previous result; ",
            "choose Analyze filtered runs to update it."
          ),
          "warning"
        )))
      }
      current <- catalog()
      positions <- match(as.character(selection$id), as.character(current$id))
      current_selection <- current[positions[!is.na(positions)], , drop = FALSE]
      if (!identical(
        analyzed_source_signature(),
        qc_catalog_signature(current_selection)
      )) {
        notices <- c(notices, list(alert_ui(
          paste0(
            "The Drive listing or metadata changed after this result was prepared. ",
            "Run the analysis again to refresh the overview."
          ),
          "warning"
        )))
      }
      if (length(notices) == 0L) NULL else shiny::tagList(notices)
    })

    quality_levels <- c("good", "medium", "bad", "unassessed")
    quality_labels <- c(
      good = "Good", medium = "Medium", bad = "Bad", unassessed = "Unassessed"
    )
    quality_data <- shiny::reactive({
      current <- display_catalog()
      stats::setNames(lapply(quality_levels, function(quality) {
        prepare_qc_timeseries_data(
          prepared(), current[current$quality == quality, , drop = FALSE]
        )
      }), quality_levels)
    })

    for (quality in quality_levels) {
      local({
        current_quality <- quality
        output[[paste0("overview_", current_quality)]] <- plotly::renderPlotly({
          current <- quality_data()[[current_quality]]
          shiny::req(nrow(current$raw) > 0L)
          make_qc_timeseries_plot(
            current,
            columns = qc_facet_columns(input$facet_columns),
            normalized = isTRUE(input$normalize_a),
            metadata_sheet_id = config$metadata_sheet_id
          )
        })
      })
    }

    output$overview_sections <- shiny::renderUI({
      current_catalog <- display_catalog()
      if (is.null(analyzed_catalog())) {
        return(alert_ui(
          "No Quality Control overview has been analyzed yet.", "info"
        ))
      }
      load_state <- progress()
      if (isTRUE(load_state$loading)) {
        return(shiny::tagList(lapply(quality_levels, function(quality) {
          catalog_count <- sum(current_catalog$quality == quality)
          bslib::card(
            class = paste("qc-quality-section", paste0("qc-quality-", quality)),
            bslib::card_header(sprintf(
              "%s (%d run%s)", quality_labels[[quality]], catalog_count,
              if (catalog_count == 1L) "" else "s"
            )),
            alert_ui(
              "Preparing this section. Its chart will appear when all runs have been checked.",
              "info"
            )
          )
        })))
      }
      current_data <- quality_data()
      columns <- qc_facet_columns(input$facet_columns)
      shiny::tagList(lapply(quality_levels, function(quality) {
        catalog_count <- sum(current_catalog$quality == quality)
        prepared_count <- length(current_data[[quality]]$run_ids)
        display_count <- length(qc_display_run_ids(
          current_data[[quality]], isTRUE(input$normalize_a)
        ))
        header <- sprintf("%s (%d run%s)",
          quality_labels[[quality]], catalog_count,
          if (catalog_count == 1L) "" else "s"
        )
        content <- if (catalog_count == 0L) {
          alert_ui("No runs in the current selection.", "info")
        } else if (prepared_count == 0L) {
          alert_ui("These runs are still being prepared or could not be parsed.", "info")
        } else if (display_count == 0L) {
          alert_ui("No run in this section has a positive A maximum for normalization.", "warning")
        } else {
          plotly::plotlyOutput(
            session$ns(paste0("overview_", quality)),
            height = sprintf(
              "%dpx", qc_timeseries_plot_height(display_count, columns)
            )
          )
        }
        bslib::card(
          class = paste("qc-quality-section", paste0("qc-quality-", quality)),
          bslib::card_header(header),
          content
        )
      }))
    })

    output$overview_warnings <- shiny::renderUI({
      if (isTRUE(progress()$loading)) return(NULL)
      warnings <- unique(unlist(lapply(quality_data(), `[[`, "warnings")))
      if (isTRUE(input$normalize_a)) {
        warnings <- c(warnings, unlist(lapply(
          quality_data(), `[[`, "normalization_warnings"
        )))
      }
      records <- requested_records()
      failures <- if (is.null(records) || nrow(records) == 0L) {
        list()
      } else {
        Filter(Negate(is.null), lapply(seq_len(nrow(records)), function(index) {
          record <- records[index, , drop = FALSE]
          entry <- prepared()[[as.character(record$id[[1]])]]
          if (qc_entry_matches_record(entry, record) && !is.null(entry$error)) {
            entry
          } else {
            NULL
          }
        }))
      }
      if (length(failures) > 0L) {
        failure_labels <- vapply(failures, function(entry) {
          sprintf("%s (%s)", entry$record$name[[1]], entry$error)
        }, character(1))
        warnings <- c(warnings, sprintf(
          "%d run(s) could not be prepared: %s",
          length(failures), paste(failure_labels, collapse = "; ")
        ))
      }
      invalid <- display_catalog()$run_id[display_catalog()$quality_invalid]
      if (length(invalid) > 0L) warnings <- c(warnings, sprintf(
        "Unexpected quality values are grouped as Unassessed: %s.",
        paste(invalid, collapse = ", ")
      ))
      if (length(warnings) == 0L) return(NULL)
      shiny::tagList(lapply(unique(warnings), alert_ui, level = "warning"))
    })

    shiny::observe({
      current <- catalog()
      state <- qc_selected_run_control_state(
        current, input$selected_run, selected_run_choice_values()
      )
      desired_run <- if (length(state$selected) > 0L) state$selected[[1]] else ""
      if (!identical(selected_run_id(), desired_run)) selected_run_id(desired_run)
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
      run_id <- if ("key" %in% names(clicked)) {
        as.character(clicked$key[[1]])
      } else if ("customdata" %in% names(clicked)) {
        as.character(clicked$customdata[[1]])
      } else ""
      if (nzchar(run_id)) {
        selected_run_id(run_id)
        shiny::updateSelectizeInput(session, "selected_run", selected = run_id)
      }
    }, ignoreInit = TRUE)

    qc_annotation_click <- shiny::reactive({
      shiny::req(active())
      suppressWarnings(plotly::event_data(
        "plotly_clickannotation", source = "qc_overview"
      ))
    })
    shiny::observeEvent(qc_annotation_click(), {
      clicked <- qc_annotation_click()
      run_id <- if ("name" %in% names(clicked)) as.character(clicked$name[[1]]) else ""
      if (nzchar(run_id)) {
        selected_run_id(run_id)
        shiny::updateSelectizeInput(session, "selected_run", selected = run_id)
      }
    }, ignoreInit = TRUE)

    selected_catalog <- shiny::reactive({
      current <- catalog()
      selected_run <- selected_run_id()
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
      if (!is.null(candidate)) {
        records <- measurements()
        record <- records[as.character(records$id) == selected_id, , drop = FALSE]
        if (nrow(record) != 1L || !qc_entry_matches_record(candidate, record)) {
          candidate <- NULL
        }
      }
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
        if (selected$metadata_matches == 0L) alert_ui(
          paste0(
            "No matching row was found in the metadata CSV for this run. ",
            "The measurement and extraction audit are still shown."
          ), "warning"
        ) else if (selected$metadata_matches > 1L) alert_ui(
          paste0(
            "More than one metadata CSV row matches this run, so its metadata are ambiguous. ",
            "The measurement and extraction audit are still shown."
          ), "warning"
        )
      )
    })
    output$audit_table <- shiny::renderUI({
      entry <- selected_entry()
      if (is.null(entry) || !is.null(entry$error)) {
        return(if (!is.null(entry$error)) {
          alert_ui(entry$error, "danger")
        } else {
          selected <- selected_catalog()
          target_ids <- if (is.null(requested_records())) {
            character()
          } else {
            as.character(requested_records()$id)
          }
          selected_id <- if (nrow(selected) == 1L) {
            as.character(selected$id[[1]])
          } else {
            ""
          }
          if (
            isTRUE(progress()$loading) && nzchar(selected_id) &&
              selected_id %in% target_ids
          ) {
            alert_ui("Preparing the selected run audit …", "info")
          } else {
            alert_ui(
              paste0(
                "This run has not been analyzed yet. Include it in the current filters and ",
                "choose Analyze filtered runs, or choose Analyze all runs."
              ),
              "info"
            )
          }
        })
      }
      table <- entry$extraction$summary[, c(
        "step_id", "PPFD_mean", "A_mean", "A_sd", "A_slope",
        "n_window", "coverage", "window_complete", "warning"
      ), drop = FALSE]
      table <- format_qc_audit_table(table)
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
    output$metadata_table <- shiny::renderUI(
      quality_metadata_table_ui(display_catalog())
    )

    list(
      prepared = prepared,
      catalog = catalog,
      analyzed_catalog = analyzed_catalog,
      progress = progress
    )
  })
}
