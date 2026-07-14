GFS_STEAM_POINT_K <- 373.16
GFS_STEAM_PRESSURE_HPA <- 1013.246
DEW_POINT_REQUIRED_COLUMNS <- c("Datetime", "wa", "Pamb", "Tcuv", "Tamb")
DEW_POINT_SERIES <- c(
  "Dew point",
  "Ambient temperature (Tamb)",
  "Cuvette temperature (Tcuv)",
  "Estimated coldest internal point (Tcuv - 2°C)"
)

gfs_saturation_vapor_pressure <- function(temperature_c) {
  temperature_c <- as.numeric(temperature_c)
  result <- rep(NA_real_, length(temperature_c))
  valid <- is.finite(temperature_c) & temperature_c > -273.15
  if (!any(valid)) {
    return(result)
  }

  temperature_k <- temperature_c[valid] + 273.15
  ts_over_t <- GFS_STEAM_POINT_K / temperature_k
  log10_pressure_hpa <-
    -7.90298 * (ts_over_t - 1) +
    5.02808 * log10(ts_over_t) -
    1.3816e-7 * (
      10^(11.344 * (1 - temperature_k / GFS_STEAM_POINT_K)) - 1
    ) +
    8.1328e-3 * (10^(-3.49149 * (ts_over_t - 1)) - 1) +
    log10(GFS_STEAM_PRESSURE_HPA)

  result[valid] <- 10^log10_pressure_hpa / 10
  result
}

gfs_dew_point_from_vapor_pressure <- function(vapor_pressure_kpa) {
  vapor_pressure_kpa <- as.numeric(vapor_pressure_kpa)
  result <- rep(NA_real_, length(vapor_pressure_kpa))
  lower <- -100
  upper <- 100
  lower_pressure <- gfs_saturation_vapor_pressure(lower)
  upper_pressure <- gfs_saturation_vapor_pressure(upper)
  valid <- is.finite(vapor_pressure_kpa) &
    vapor_pressure_kpa >= lower_pressure &
    vapor_pressure_kpa <= upper_pressure

  result[valid] <- vapply(vapor_pressure_kpa[valid], function(pressure) {
    stats::uniroot(
      function(temperature) {
        gfs_saturation_vapor_pressure(temperature) - pressure
      },
      interval = c(lower, upper),
      tol = 1e-8
    )$root
  }, numeric(1))
  result
}

dew_point_from_h2o_ppm <- function(h2o_ppm, pamb_kpa) {
  vapor_pressure_kpa <- as.numeric(h2o_ppm) / 1e6 * as.numeric(pamb_kpa)
  gfs_dew_point_from_vapor_pressure(vapor_pressure_kpa)
}

dew_point_from_relative_humidity <- function(tcuv_c, relative_humidity) {
  vapor_pressure_kpa <-
    gfs_saturation_vapor_pressure(tcuv_c) *
    as.numeric(relative_humidity) / 100
  gfs_dew_point_from_vapor_pressure(vapor_pressure_kpa)
}

relative_humidity_from_h2o_ppm <- function(h2o_ppm, pamb_kpa, tcuv_c) {
  vapor_pressure_kpa <- as.numeric(h2o_ppm) / 1e6 * as.numeric(pamb_kpa)
  100 * vapor_pressure_kpa / gfs_saturation_vapor_pressure(tcuv_c)
}

validate_planner_number <- function(value, label, lower, upper) {
  if (
    length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < lower ||
      value > upper
  ) {
    stop(
      sprintf("%s must be between %s and %s.", label, lower, upper),
      call. = FALSE
    )
  }
  as.numeric(value)
}

calculate_dew_point_plan <- function(
    humidity_mode = c("ppm", "rh"),
    tcuv_c,
    tamb_c,
    pamb_kpa,
    h2o_ppm = NA_real_,
    relative_humidity = NA_real_,
    safety_buffer_c = 2) {
  humidity_mode <- match.arg(humidity_mode)
  tcuv_c <- validate_planner_number(tcuv_c, "Cuvette temperature", -10, 50)
  tamb_c <- validate_planner_number(tamb_c, "Ambient temperature", -10, 50)
  pamb_kpa <- validate_planner_number(pamb_kpa, "Ambient pressure", 60, 110)
  safety_buffer_c <- validate_planner_number(
    safety_buffer_c,
    "Safety buffer",
    0,
    5
  )

  if (humidity_mode == "ppm") {
    h2o_ppm <- validate_planner_number(
      h2o_ppm,
      "Expected chamber H2O",
      100,
      75000
    )
    dew_point_c <- dew_point_from_h2o_ppm(h2o_ppm, pamb_kpa)
    relative_humidity <- relative_humidity_from_h2o_ppm(
      h2o_ppm,
      pamb_kpa,
      tcuv_c
    )
  } else {
    relative_humidity <- validate_planner_number(
      relative_humidity,
      "Relative humidity",
      1,
      100
    )
    dew_point_c <- dew_point_from_relative_humidity(
      tcuv_c,
      relative_humidity
    )
    h2o_ppm <-
      relative_humidity / 100 *
      gfs_saturation_vapor_pressure(tcuv_c) /
      pamb_kpa * 1e6
  }

  if (!is.finite(dew_point_c)) {
    stop("The selected conditions do not produce a valid dew point.", call. = FALSE)
  }

  internal_reference_c <- tcuv_c - 2
  ambient_margin_c <- tamb_c - dew_point_c
  internal_margin_c <- internal_reference_c - dew_point_c
  limiting_margin_c <- min(ambient_margin_c, internal_margin_c)
  temperature_order_margin_c <- tamb_c - tcuv_c
  cuvette_above_ambient <- tcuv_c > tamb_c
  dew_point_minimum_ambient_c <- dew_point_c + safety_buffer_c
  status <- if (limiting_margin_c <= 0) {
    "danger"
  } else if (cuvette_above_ambient || limiting_margin_c < safety_buffer_c) {
    "caution"
  } else {
    "safe"
  }

  list(
    humidity_mode = humidity_mode,
    dew_point_c = unname(dew_point_c),
    ambient_margin_c = unname(ambient_margin_c),
    internal_reference_c = unname(internal_reference_c),
    internal_margin_c = unname(internal_margin_c),
    limiting_margin_c = unname(limiting_margin_c),
    temperature_order_margin_c = unname(temperature_order_margin_c),
    cuvette_above_ambient = cuvette_above_ambient,
    dew_point_minimum_ambient_c = unname(dew_point_minimum_ambient_c),
    minimum_ambient_c = unname(max(tcuv_c, dew_point_minimum_ambient_c)),
    safety_buffer_c = safety_buffer_c,
    h2o_ppm = unname(h2o_ppm),
    relative_humidity = unname(relative_humidity),
    status = status
  )
}

dew_point_temperature_order_summary <- function(parsed) {
  required <- c("Tcuv", "Tamb")
  missing <- setdiff(required, names(parsed$data))
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "The selected run is missing required temperature column(s): %s.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  tcuv <- as.numeric(parsed$data$Tcuv)
  tamb <- as.numeric(parsed$data$Tamb)
  valid <- is.finite(tcuv) & is.finite(tamb)
  if (!any(valid)) {
    stop(
      "The selected run contains no valid paired Tcuv and Tamb values.",
      call. = FALSE
    )
  }

  excess <- tcuv[valid] - tamb[valid]
  warmer <- excess > 0
  list(
    valid_count = sum(valid),
    warning_count = sum(warmer),
    maximum_excess_c = if (any(warmer)) max(excess[warmer]) else 0
  )
}

conservative_run_values <- function(parsed) {
  missing <- setdiff(DEW_POINT_REQUIRED_COLUMNS, names(parsed$data))
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "The selected run is missing required dew-point column(s): %s.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  finite_value <- function(values, summary_function, label) {
    values <- values[is.finite(values)]
    if (length(values) == 0L) {
      stop(
        sprintf("The selected run has no valid %s values.", label),
        call. = FALSE
      )
    }
    unname(summary_function(values))
  }

  list(
    h2o_ppm = finite_value(parsed$data$wa, max, "wa"),
    tamb_c = finite_value(parsed$data$Tamb, min, "Tamb"),
    tcuv_c = finite_value(parsed$data$Tcuv, stats::median, "Tcuv"),
    pamb_kpa = finite_value(parsed$data$Pamb, stats::median, "Pamb")
  )
}

dew_point_audit_data <- function(parsed) {
  missing <- setdiff(DEW_POINT_REQUIRED_COLUMNS, names(parsed$data))
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "The selected run is missing required dew-point column(s): %s.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  data <- parsed$data
  dew_point_c <- dew_point_from_h2o_ppm(data$wa, data$Pamb)
  required_values <- list(
    `dew-point` = dew_point_c,
    Tamb = data$Tamb,
    Tcuv = data$Tcuv
  )
  invalid_values <- names(required_values)[!vapply(
    required_values,
    function(values) any(is.finite(values)),
    logical(1)
  )]
  if (length(invalid_values) > 0L) {
    stop(
      sprintf(
        "The selected run contains no valid dew-point audit values for: %s.",
        paste(invalid_values, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  wide <- data.frame(
    Datetime = data$Datetime,
    `Dew point` = dew_point_c,
    `Ambient temperature (Tamb)` = data$Tamb,
    `Cuvette temperature (Tcuv)` = data$Tcuv,
    `Estimated coldest internal point (Tcuv - 2°C)` = data$Tcuv - 2,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  pieces <- lapply(DEW_POINT_SERIES, function(series) {
    data.frame(
      Datetime = wide$Datetime,
      Series = series,
      Temperature = wide[[series]],
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, pieces)
  long <- long[
    !is.na(long$Datetime) & is.finite(long$Temperature),
    ,
    drop = FALSE
  ]
  if (nrow(long) == 0L) {
    stop("The selected run contains no valid dew-point audit values.", call. = FALSE)
  }
  long$Series <- factor(long$Series, levels = DEW_POINT_SERIES)
  long$hover <- sprintf(
    "%s<br>%s: %.2f°C",
    format(long$Datetime, "%Y-%m-%d %H:%M:%S", tz = WALZ_TIMEZONE),
    long$Series,
    long$Temperature
  )
  rownames(long) <- NULL
  long
}
