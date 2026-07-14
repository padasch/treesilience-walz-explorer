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

walz_vpd_to_kpa <- function(vpd_pa_per_kpa, pamb_kpa) {
  as.numeric(vpd_pa_per_kpa) * as.numeric(pamb_kpa) / 1000
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

dew_point_margin_status <- function(margin_c, safety_buffer_c) {
  if (margin_c <= 0) {
    "danger"
  } else if (margin_c < safety_buffer_c) {
    "caution"
  } else {
    "safe"
  }
}

calculate_dew_point_plan <- function(
    water_mode = c("outlet", "inlet_plus"),
    tcuv_c,
    tamb_c,
    pamb_kpa,
    outlet_h2o_ppm = NA_real_,
    inlet_h2o_ppm = NA_real_,
    leaf_h2o_added_ppm = NA_real_,
    safety_buffer_c = 2) {
  water_mode <- match.arg(water_mode)
  tcuv_c <- validate_planner_number(tcuv_c, "Cuvette temperature", -10, 50)
  tamb_c <- validate_planner_number(tamb_c, "Ambient temperature", -10, 55)
  pamb_kpa <- validate_planner_number(pamb_kpa, "Ambient pressure", 60, 110)
  safety_buffer_c <- validate_planner_number(
    safety_buffer_c,
    "Safety buffer",
    0,
    5
  )

  if (water_mode == "outlet") {
    outlet_h2o_ppm <- validate_planner_number(
      outlet_h2o_ppm,
      "Expected chamber/outlet H2O",
      100,
      75000
    )
    inlet_h2o_ppm <- NA_real_
    leaf_h2o_added_ppm <- NA_real_
  } else {
    inlet_h2o_ppm <- validate_planner_number(
      inlet_h2o_ppm,
      "Controlled inlet H2O",
      100,
      75000
    )
    leaf_h2o_added_ppm <- validate_planner_number(
      leaf_h2o_added_ppm,
      "Expected leaf-added H2O",
      0,
      50000
    )
    outlet_h2o_ppm <- inlet_h2o_ppm + leaf_h2o_added_ppm
    if (outlet_h2o_ppm > 75000) {
      stop(
        "Inlet H2O plus expected leaf-added H2O must not exceed 75000 ppm.",
        call. = FALSE
      )
    }
  }

  dew_point_c <- dew_point_from_h2o_ppm(outlet_h2o_ppm, pamb_kpa)
  relative_humidity <- relative_humidity_from_h2o_ppm(
    outlet_h2o_ppm,
    pamb_kpa,
    tcuv_c
  )

  if (!is.finite(dew_point_c)) {
    stop("The selected conditions do not produce a valid dew point.", call. = FALSE)
  }

  internal_reference_c <- tcuv_c - 2
  ambient_margin_c <- tamb_c - dew_point_c
  internal_margin_c <- internal_reference_c - dew_point_c
  limiting_margin_c <- min(ambient_margin_c, internal_margin_c)
  limiting_temperature_c <- min(tamb_c, internal_reference_c)
  limiting_surface <- if (ambient_margin_c <= internal_margin_c) {
    "Tamb"
  } else {
    "Tcuv - 2°C"
  }
  temperature_order_margin_c <- tamb_c - tcuv_c
  cuvette_above_ambient <- tcuv_c > tamb_c
  safety_threshold_c <- dew_point_c + safety_buffer_c
  tubing_status <- dew_point_margin_status(ambient_margin_c, safety_buffer_c)
  internal_status <- dew_point_margin_status(internal_margin_c, safety_buffer_c)
  status_order <- c(safe = 1L, caution = 2L, danger = 3L)
  status <- names(which.max(c(
    tubing = status_order[[tubing_status]],
    internal = status_order[[internal_status]]
  )))
  status <- if (status == "tubing") tubing_status else internal_status

  list(
    water_mode = water_mode,
    dew_point_c = unname(dew_point_c),
    tcuv_c = tcuv_c,
    tamb_c = tamb_c,
    ambient_margin_c = unname(ambient_margin_c),
    internal_reference_c = unname(internal_reference_c),
    internal_margin_c = unname(internal_margin_c),
    limiting_margin_c = unname(limiting_margin_c),
    limiting_temperature_c = unname(limiting_temperature_c),
    limiting_surface = limiting_surface,
    buffer_clearance_c = unname(limiting_margin_c - safety_buffer_c),
    temperature_order_margin_c = unname(temperature_order_margin_c),
    cuvette_above_ambient = cuvette_above_ambient,
    safety_threshold_c = unname(safety_threshold_c),
    safety_buffer_c = safety_buffer_c,
    outlet_h2o_ppm = unname(outlet_h2o_ppm),
    inlet_h2o_ppm = unname(inlet_h2o_ppm),
    leaf_h2o_added_ppm = unname(leaf_h2o_added_ppm),
    relative_humidity = unname(relative_humidity),
    tubing_status = tubing_status,
    internal_status = internal_status,
    status = status
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

dew_point_audit_summary <- function(parsed, safety_buffer_c = 2) {
  safety_buffer_c <- validate_planner_number(
    safety_buffer_c,
    "Safety buffer",
    0,
    5
  )
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
  tube_margin_c <- as.numeric(data$Tamb) - dew_point_c
  internal_margin_c <- as.numeric(data$Tcuv) - 2 - dew_point_c
  valid <- is.finite(tube_margin_c) & is.finite(internal_margin_c)
  if (!any(valid)) {
    stop(
      "The selected run contains no valid dew-point audit values.",
      call. = FALSE
    )
  }

  tube_margin_c <- tube_margin_c[valid]
  internal_margin_c <- internal_margin_c[valid]
  tcuv <- as.numeric(data$Tcuv)[valid]
  tamb <- as.numeric(data$Tamb)[valid]
  summarize_margin <- function(margin_c) {
    list(
      minimum_margin_c = min(margin_c),
      danger_count = sum(margin_c <= 0),
      caution_count = sum(margin_c > 0 & margin_c < safety_buffer_c),
      status = dew_point_margin_status(min(margin_c), safety_buffer_c)
    )
  }

  list(
    valid_count = sum(valid),
    safety_buffer_c = safety_buffer_c,
    tubing = summarize_margin(tube_margin_c),
    internal = summarize_margin(internal_margin_c),
    cuvette_above_ambient_count = sum(tcuv > tamb),
    maximum_cuvette_excess_c = if (any(tcuv > tamb)) {
      max(tcuv[tcuv > tamb] - tamb[tcuv > tamb])
    } else {
      0
    }
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
