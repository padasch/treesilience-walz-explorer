variable_display_label <- function(variable, units) {
  label <- unname(WALZ_VARIABLE_LABELS[variable])
  if (length(label) == 0L || is.na(label)) {
    label <- variable
  }

  unit <- unname(units[variable])
  if (length(unit) == 0L) {
    unit <- NA_character_
  }
  if (identical(variable, "White x T")) {
    unit <- "% of maximum light"
  }

  if (is.na(unit) || !nzchar(trimws(unit)) || unit == "-") {
    return(label)
  }

  sprintf("%s [%s]", label, unit)
}

plottable_variables <- function(parsed) {
  variables <- names(parsed$data)[vapply(parsed$data, is.numeric, logical(1))]
  setdiff(variables, "Datetime")
}

plot_variable_choices <- function(parsed_runs) {
  parsed_runs <- Filter(Negate(is.null), parsed_runs)
  if (length(parsed_runs) == 0L) {
    return(character())
  }

  variables <- unique(unlist(
    lapply(parsed_runs, plottable_variables),
    use.names = FALSE
  ))
  variables <- c(
    intersect(WALZ_RESPONSE_VARIABLES, variables),
    setdiff(
      variables,
      c(WALZ_RESPONSE_VARIABLES, WALZ_PHYSIOLOGICAL_CONSTANTS)
    ),
    intersect(WALZ_PHYSIOLOGICAL_CONSTANTS, variables)
  )

  labels <- vapply(variables, function(variable) {
    source <- parsed_runs[[which(vapply(
      parsed_runs,
      function(parsed) variable %in% names(parsed$data),
      logical(1)
    ))[[1]]]]
    variable_display_label(variable, source$units)
  }, character(1))

  stats::setNames(variables, labels)
}

group_plot_variable_choices <- function(choices) {
  choice_values <- unname(choices)
  subset_choices <- function(values) {
    choices[match(values, choice_values)]
  }

  response_values <- intersect(WALZ_RESPONSE_VARIABLES, choice_values)
  constant_values <- intersect(WALZ_PHYSIOLOGICAL_CONSTANTS, choice_values)
  environmental_values <- setdiff(
    choice_values,
    c(response_values, constant_values)
  )

  list(
    response = subset_choices(response_values),
    environmental = subset_choices(environmental_values),
    physiological_constant = subset_choices(constant_values)
  )
}

empty_measurement_long <- function() {
  data.frame(
    Datetime = as.POSIXct(character(), tz = WALZ_TIMEZONE),
    ElapsedMinutes = numeric(),
    variable = character(),
    value = numeric(),
    panel = character(),
    Run = character(),
    hover = character(),
    stringsAsFactors = FALSE
  )
}

measurement_long_data <- function(
    parsed,
    variables = WALZ_PLOT_VARIABLES,
    run_label = "Selected run") {
  available <- intersect(variables, plottable_variables(parsed))
  if (length(available) == 0L) {
    return(empty_measurement_long())
  }

  valid_times <- parsed$data$Datetime[!is.na(parsed$data$Datetime)]
  if (length(valid_times) == 0L) {
    return(empty_measurement_long())
  }
  run_start <- min(valid_times)

  pieces <- lapply(available, function(variable) {
    data.frame(
      Datetime = parsed$data$Datetime,
      ElapsedMinutes = as.numeric(difftime(
        parsed$data$Datetime,
        run_start,
        units = "mins"
      )),
      variable = variable,
      value = parsed$data[[variable]],
      panel = variable_display_label(variable, parsed$units),
      Run = run_label,
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, pieces)
  long <- long[!is.na(long$Datetime) & !is.na(long$value), , drop = FALSE]
  long$hover <- sprintf(
    paste0(
      "Run: %s<br>%s<br>Recorded: %s<br>",
      "Elapsed: %.1f min<br>Value: %s"
    ),
    long$Run,
    long$panel,
    format(long$Datetime, "%Y-%m-%d %H:%M:%S", tz = WALZ_TIMEZONE),
    long$ElapsedMinutes,
    format(long$value, digits = 7, trim = TRUE)
  )
  long
}

empty_state_long <- function() {
  data.frame(
    A = numeric(),
    state = numeric(),
    variable = character(),
    Datetime = as.POSIXct(character(), tz = WALZ_TIMEZONE),
    panel = character(),
    Run = character(),
    hover = character(),
    stringsAsFactors = FALSE
  )
}

state_long_data <- function(
    parsed,
    variables = WALZ_PLOT_VARIABLES,
    run_label = "Selected run") {
  state_variables <- setdiff(intersect(variables, plottable_variables(parsed)), "A")
  if (!"A" %in% names(parsed$data)) {
    stop("The A vs state view requires the variable A.", call. = FALSE)
  }
  if (length(state_variables) == 0L) {
    return(empty_state_long())
  }

  pieces <- lapply(state_variables, function(variable) {
    data.frame(
      A = parsed$data$A,
      state = parsed$data[[variable]],
      variable = variable,
      Datetime = parsed$data$Datetime,
      panel = variable_display_label(variable, parsed$units),
      Run = run_label,
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, pieces)
  long <- long[
    !is.na(long$A) & !is.na(long$state) & !is.na(long$Datetime),
    ,
    drop = FALSE
  ]
  long$hover <- sprintf(
    "%s<br>Run: %s<br>%s<br>State: %s<br>A: %s",
    format(long$Datetime, "%Y-%m-%d %H:%M:%S", tz = WALZ_TIMEZONE),
    long$Run,
    long$panel,
    format(long$state, digits = 7, trim = TRUE),
    format(long$A, digits = 7, trim = TRUE)
  )
  long
}

panel_levels_for_variables <- function(variables, parsed_runs) {
  vapply(variables, function(variable) {
    sources <- which(vapply(
      parsed_runs,
      function(parsed) variable %in% names(parsed$data),
      logical(1)
    ))
    if (length(sources) == 0L) {
      return(variable)
    }
    variable_display_label(variable, parsed_runs[[sources[[1]]]]$units)
  }, character(1))
}

fifteen_minute_breaks <- function(datetimes) {
  limits <- range(datetimes, na.rm = TRUE)
  start <- as.POSIXct(
    floor(as.numeric(limits[[1]]) / (15 * 60)) * (15 * 60),
    origin = "1970-01-01",
    tz = WALZ_TIMEZONE
  )
  end <- as.POSIXct(
    ceiling(as.numeric(limits[[2]]) / (15 * 60)) * (15 * 60),
    origin = "1970-01-01",
    tz = WALZ_TIMEZONE
  )
  seq(start, end, by = "15 min")
}

fifteen_minute_elapsed_breaks <- function(elapsed_minutes) {
  limits <- range(elapsed_minutes, na.rm = TRUE)
  seq(
    floor(limits[[1]] / 15) * 15,
    ceiling(limits[[2]] / 15) * 15,
    by = 15
  )
}

walz_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#e5e7eb"),
      panel.grid.major.x = ggplot2::element_line(colour = "#eef0ed"),
      strip.text = ggplot2::element_text(face = "bold", colour = "#243228"),
      strip.background = ggplot2::element_rect(fill = "#eef4ed", colour = NA),
      axis.title = ggplot2::element_text(colour = "#34443a"),
      axis.text = ggplot2::element_text(colour = "#4b5563"),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 8),
      plot.margin = ggplot2::margin(8, 12, 8, 8)
    )
}

plotly_controls <- function(widget) {
  plotly::config(
    widget,
    scrollZoom = TRUE,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "select2d", "lasso2d", "sendDataToCloud", "autoScale2d"
    ),
    modeBarButtonsToAdd = c("drawline", "drawopenpath", "eraseshape")
  )
}

make_timeseries_plot <- function(
    parsed,
    show_grid = FALSE,
    variables = WALZ_PLOT_VARIABLES,
    comparison = NULL,
    run_labels = c("Selected run", "Overlay run")) {
  if (length(variables) == 0L) {
    stop("Select at least one numeric variable to draw the timeseries.", call. = FALSE)
  }

  parsed_runs <- list(parsed)
  if (!is.null(comparison)) {
    parsed_runs <- c(parsed_runs, list(comparison))
  }
  run_labels <- rep_len(run_labels, length(parsed_runs))
  pieces <- Map(
    measurement_long_data,
    parsed_runs,
    MoreArgs = list(variables = variables),
    run_label = run_labels
  )
  long <- do.call(rbind, pieces)
  if (nrow(long) == 0L) {
    stop("The selected variables contain no plottable numeric values.", call. = FALSE)
  }

  panel_levels <- panel_levels_for_variables(variables, parsed_runs)
  long$panel <- factor(long$panel, levels = unique(panel_levels))
  long$Run <- factor(long$Run, levels = run_labels)
  overlay <- length(parsed_runs) == 2L

  if (overlay) {
    plot <- ggplot2::ggplot(
      long,
      ggplot2::aes(
        x = ElapsedMinutes,
        y = value,
        text = hover,
        colour = Run,
        group = interaction(variable, Run)
      )
    ) +
      ggplot2::geom_line(linewidth = 0.65, na.rm = TRUE) +
      ggplot2::geom_point(size = 0.7, alpha = 0.68, na.rm = TRUE) +
      ggplot2::scale_colour_manual(values = c("#28754d", "#bd5d38")) +
      ggplot2::labs(
        x = "Elapsed time from run start (minutes)",
        y = NULL,
        colour = "Measurement run"
      )
    if (isTRUE(show_grid)) {
      plot <- plot + ggplot2::scale_x_continuous(
        breaks = fifteen_minute_elapsed_breaks(long$ElapsedMinutes)
      )
    }
  } else {
    plot <- ggplot2::ggplot(
      long,
      ggplot2::aes(x = Datetime, y = value, text = hover, group = variable)
    ) +
      ggplot2::geom_line(linewidth = 0.55, colour = "#28754d", na.rm = TRUE) +
      ggplot2::geom_point(
        size = 0.65,
        colour = "#1f5b3c",
        alpha = 0.75,
        na.rm = TRUE
      ) +
      ggplot2::labs(x = "Local time (Europe/Zurich)", y = NULL)
    if (isTRUE(show_grid)) {
      plot <- plot + ggplot2::scale_x_datetime(
        breaks = fifteen_minute_breaks(long$Datetime),
        date_labels = "%H:%M"
      )
    } else {
      plot <- plot + ggplot2::scale_x_datetime(date_labels = "%H:%M")
    }
  }

  plot <- plot +
    ggplot2::facet_wrap(ggplot2::vars(panel), ncol = 2, scales = "free_y") +
    walz_plot_theme() +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

  widget <- plotly::ggplotly(
    plot,
    tooltip = "text",
    dynamicTicks = TRUE,
    height = 900
  )
  plotly_controls(widget)
}

make_state_plot <- function(
    parsed,
    variables = WALZ_PLOT_VARIABLES,
    comparison = NULL,
    run_labels = c("Selected run", "Overlay run")) {
  state_variables <- setdiff(variables, "A")
  if (length(state_variables) == 0L) {
    stop(
      "Select at least one state variable in addition to A for this view.",
      call. = FALSE
    )
  }

  parsed_runs <- list(parsed)
  if (!is.null(comparison)) {
    parsed_runs <- c(parsed_runs, list(comparison))
  }
  run_labels <- rep_len(run_labels, length(parsed_runs))
  pieces <- Map(
    state_long_data,
    parsed_runs,
    MoreArgs = list(variables = variables),
    run_label = run_labels
  )
  long <- do.call(rbind, pieces)
  if (nrow(long) == 0L) {
    stop("The selected state variables contain no paired values with A.", call. = FALSE)
  }

  panel_levels <- panel_levels_for_variables(state_variables, parsed_runs)
  long$panel <- factor(long$panel, levels = unique(panel_levels))
  long$Run <- factor(long$Run, levels = run_labels)
  overlay <- length(parsed_runs) == 2L
  a_label <- variable_display_label("A", parsed$units)

  if (overlay) {
    plot <- ggplot2::ggplot(
      long,
      ggplot2::aes(
        x = state,
        y = A,
        text = hover,
        colour = Run,
        group = Run
      )
    ) +
      ggplot2::geom_path(linewidth = 0.55, alpha = 0.7) +
      ggplot2::geom_point(size = 1.05, alpha = 0.75) +
      ggplot2::scale_colour_manual(values = c("#28754d", "#bd5d38")) +
      ggplot2::labs(x = NULL, y = a_label, colour = "Measurement run")
  } else {
    plot <- ggplot2::ggplot(
      long,
      ggplot2::aes(x = state, y = A, text = hover, group = 1)
    ) +
      ggplot2::geom_path(linewidth = 0.45, colour = "#6c8e75", alpha = 0.75) +
      ggplot2::geom_point(size = 1.1, colour = "#28754d", alpha = 0.8) +
      ggplot2::labs(x = NULL, y = a_label)
  }

  plot <- plot +
    ggplot2::facet_wrap(ggplot2::vars(panel), ncol = 2, scales = "free_x") +
    walz_plot_theme() +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

  widget <- plotly::ggplotly(
    plot,
    tooltip = "text",
    dynamicTicks = TRUE,
    height = 850
  )
  plotly_controls(widget)
}

make_dew_point_audit_plot <- function(parsed) {
  long <- dew_point_audit_data(parsed)
  colours <- c(
    "Dew point" = "#b23a32",
    "Ambient temperature (Tamb)" = "#426a8c",
    "Cuvette temperature (Tcuv)" = "#28754d",
    "Estimated coldest internal point (Tcuv - 2°C)" = "#c27b2c"
  )
  line_types <- c(
    "Dew point" = "solid",
    "Ambient temperature (Tamb)" = "solid",
    "Cuvette temperature (Tcuv)" = "solid",
    "Estimated coldest internal point (Tcuv - 2°C)" = "dashed"
  )

  plot <- ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = Datetime,
      y = Temperature,
      colour = Series,
      linetype = Series,
      group = Series,
      text = hover
    )
  ) +
    ggplot2::geom_line(linewidth = 0.75, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = colours, drop = FALSE) +
    ggplot2::scale_linetype_manual(values = line_types, drop = FALSE) +
    ggplot2::scale_x_datetime(date_labels = "%H:%M") +
    ggplot2::labs(
      x = "Local time (Europe/Zurich)",
      y = "Temperature [°C]",
      colour = NULL,
      linetype = NULL
    ) +
    walz_plot_theme() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(
      colour = ggplot2::guide_legend(nrow = 2, byrow = TRUE),
      linetype = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
    )

  widget <- plotly::ggplotly(
    plot,
    tooltip = "text",
    dynamicTicks = TRUE,
    height = 650
  )
  plotly_controls(widget)
}

make_dew_point_plan_plot <- function(plan) {
  required <- c(
    "dew_point_c",
    "safety_threshold_c",
    "internal_reference_c",
    "tcuv_c",
    "tamb_c",
    "safety_buffer_c"
  )
  missing <- setdiff(required, names(plan))
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "The dew-point plan is missing value(s): %s.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  series <- data.frame(
    label = c(
      "Dew point",
      "Dew point + safety margin",
      "Estimated coldest cuvette point (Tcuv - 2°C)",
      "Cuvette temperature (Tcuv)",
      "Tube/environment temperature (Tamb proxy)"
    ),
    temperature = c(
      plan$dew_point_c,
      plan$safety_threshold_c,
      plan$internal_reference_c,
      plan$tcuv_c,
      plan$tamb_c
    ),
    colour = c("#b23a32", "#b23a32", "#c27b2c", "#28754d", "#426a8c"),
    dash = c("solid", "dash", "dash", "solid", "solid"),
    stringsAsFactors = FALSE
  )
  if (any(!is.finite(series$temperature))) {
    stop("The dew-point plan contains an invalid temperature.", call. = FALSE)
  }

  temperature_range <- range(series$temperature)
  padding <- max(1, diff(temperature_range) * 0.1)
  widget <- plotly::plot_ly()
  for (index in seq_len(nrow(series))) {
    row <- series[index, , drop = FALSE]
    widget <- plotly::add_trace(
      widget,
      x = c(0, 1),
      y = rep(row$temperature, 2),
      type = "scatter",
      mode = "lines",
      name = sprintf("%s: %.1f°C", row$label, row$temperature),
      line = list(
        color = row$colour,
        width = if (row$label == "Dew point + safety margin") 3 else 2.2,
        dash = row$dash
      ),
      hovertemplate = paste0(
        "<b>", row$label, "</b><br>",
        sprintf("%.1f°C", row$temperature),
        "<extra></extra>"
      ),
      showlegend = TRUE
    )
  }

  widget <- plotly::layout(
    widget,
    xaxis = list(
      fixedrange = TRUE,
      range = c(0, 1),
      showgrid = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      title = "",
      zeroline = FALSE
    ),
    yaxis = list(
      range = c(temperature_range[[1]] - padding, temperature_range[[2]] + padding),
      title = "Temperature [°C]",
      gridcolor = "#e5e9e3",
      zeroline = FALSE
    ),
    legend = list(
      orientation = "h",
      x = 0,
      xanchor = "left",
      y = -0.12,
      yanchor = "top"
    ),
    hovermode = "closest",
    margin = list(l = 70, r = 25, t = 20, b = 145),
    paper_bgcolor = "#ffffff",
    plot_bgcolor = "#ffffff"
  )

  plotly_controls(widget)
}
