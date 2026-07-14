variable_display_label <- function(variable, units) {
  label <- WALZ_VARIABLE_LABELS[[variable]]
  if (is.null(label) || is.na(label)) {
    label <- variable
  }

  unit <- units[[variable]]
  if (identical(variable, "White x T")) {
    unit <- "% of maximum light"
  }

  if (is.null(unit) || is.na(unit) || !nzchar(trimws(unit)) || unit == "-") {
    return(label)
  }

  sprintf("%s [%s]", label, unit)
}

measurement_long_data <- function(parsed) {
  available <- intersect(WALZ_PLOT_VARIABLES, names(parsed$data))
  if (length(available) == 0L) {
    stop("None of the eight expected variables is available for plotting.", call. = FALSE)
  }

  pieces <- lapply(available, function(variable) {
    data.frame(
      Datetime = parsed$data$Datetime,
      variable = variable,
      value = parsed$data[[variable]],
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, pieces)
  long <- long[!is.na(long$Datetime) & !is.na(long$value), , drop = FALSE]

  if (nrow(long) == 0L) {
    stop("The expected variables contain no plottable numeric values.", call. = FALSE)
  }

  labels <- vapply(
    available,
    variable_display_label,
    character(1),
    units = parsed$units
  )
  long$panel <- factor(labels[long$variable], levels = labels)
  long$hover <- sprintf(
    "%s<br>%s<br>Value: %s",
    as.character(long$panel),
    format(long$Datetime, "%Y-%m-%d %H:%M:%S", tz = WALZ_TIMEZONE),
    format(long$value, digits = 7, trim = TRUE)
  )
  long
}

state_long_data <- function(parsed) {
  state_variables <- setdiff(intersect(WALZ_PLOT_VARIABLES, names(parsed$data)), "A")
  if (!"A" %in% names(parsed$data)) {
    stop("The A vs state view requires the variable A.", call. = FALSE)
  }
  if (length(state_variables) == 0L) {
    stop("No state variables are available for the A vs state view.", call. = FALSE)
  }

  pieces <- lapply(state_variables, function(variable) {
    data.frame(
      A = parsed$data$A,
      state = parsed$data[[variable]],
      variable = variable,
      Datetime = parsed$data$Datetime,
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, pieces)
  long <- long[
    !is.na(long$A) & !is.na(long$state) & !is.na(long$Datetime),
    ,
    drop = FALSE
  ]

  if (nrow(long) == 0L) {
    stop("The A and state variables contain no paired numeric values.", call. = FALSE)
  }

  labels <- vapply(
    state_variables,
    variable_display_label,
    character(1),
    units = parsed$units
  )
  long$panel <- factor(labels[long$variable], levels = labels)
  long$hover <- sprintf(
    "%s<br>%s<br>State: %s<br>A: %s",
    as.character(long$panel),
    format(long$Datetime, "%Y-%m-%d %H:%M:%S", tz = WALZ_TIMEZONE),
    format(long$state, digits = 7, trim = TRUE),
    format(long$A, digits = 7, trim = TRUE)
  )
  long
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

make_timeseries_plot <- function(parsed, show_grid = FALSE) {
  long <- measurement_long_data(parsed)
  plot <- ggplot2::ggplot(
    long,
    ggplot2::aes(x = Datetime, y = value, text = hover, group = variable)
  ) +
    ggplot2::geom_line(linewidth = 0.55, colour = "#28754d", na.rm = TRUE) +
    ggplot2::geom_point(size = 0.65, colour = "#1f5b3c", alpha = 0.75, na.rm = TRUE) +
    ggplot2::facet_wrap(ggplot2::vars(panel), ncol = 2, scales = "free_y") +
    ggplot2::labs(x = "Local time (Europe/Zurich)", y = NULL) +
    walz_plot_theme()

  if (isTRUE(show_grid)) {
    plot <- plot + ggplot2::scale_x_datetime(
      breaks = fifteen_minute_breaks(long$Datetime),
      date_labels = "%H:%M"
    )
  } else {
    plot <- plot + ggplot2::scale_x_datetime(date_labels = "%H:%M")
  }

  widget <- plotly::ggplotly(
    plot,
    tooltip = "text",
    dynamicTicks = TRUE,
    height = 900
  )
  plotly_controls(widget)
}
make_state_plot <- function(parsed) {
  long <- state_long_data(parsed)
  a_label <- variable_display_label("A", parsed$units)
  plot <- ggplot2::ggplot(
    long,
    ggplot2::aes(x = state, y = A, text = hover, group = 1)
  ) +
    ggplot2::geom_path(linewidth = 0.45, colour = "#6c8e75", alpha = 0.75) +
    ggplot2::geom_point(size = 1.1, colour = "#28754d", alpha = 0.8) +
    ggplot2::facet_wrap(ggplot2::vars(panel), ncol = 2, scales = "free_x") +
    ggplot2::labs(x = NULL, y = a_label) +
    walz_plot_theme()

  widget <- plotly::ggplotly(
    plot,
    tooltip = "text",
    dynamicTicks = TRUE,
    height = 850
  )
  plotly_controls(widget)
}
