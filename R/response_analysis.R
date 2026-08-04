WALZ_CURATED_OAK_RUNS <- c(
  "20260715_0928_OF",
  "20260715_1439_OF",
  "20260716_0903_OF",
  "20260716_0807_OF",
  "20260716_1233_OF",
  "20260717_1205_OF",
  "20260720_0846_OF",
  "20260720_1330_OF",
  "20260721_0825_OF",
  "20260721_1240_OF"
)

WALZ_CURATED_BEECH_RUNS <- c(
  "20260715_0906_B1",
  "20260715_1439_B1",
  "20260716_0903_B1",
  "20260716_1230_B1",
  "20260717_0807_B1",
  "20260717_1205_B1",
  "20260720_0846_B1",
  "20260720_1330_B1",
  "20260721_0825_B1",
  "20260721_1240_B1"
)

WALZ_RESPONSE_PRESETS <- list(
  curated_oak = WALZ_CURATED_OAK_RUNS,
  curated_beech = WALZ_CURATED_BEECH_RUNS
)

WALZ_RESPONSE_PRESET_LABELS <- c(
  curated_oak = "First Analysis — Oak temperature series (10)",
  curated_beech = "First Analysis — Beech temperature series (10)"
)

resolve_response_preset <- function(preset, catalog) {
  requested <- WALZ_RESPONSE_PRESETS[[preset]]
  if (is.null(requested) || is.null(catalog) || nrow(catalog) == 0L) {
    missing <- if (is.null(requested)) character() else requested
    return(list(ids = character(), matched = character(), missing = missing))
  }
  positions <- match(normalize_run_id(requested), normalize_run_id(catalog$run_id))
  found <- !is.na(positions)
  list(
    ids = as.character(catalog$id[positions[found]]),
    matched = catalog$run_id[positions[found]],
    missing = requested[!found]
  )
}

response_run_choices <- function(catalog) {
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

manual_response_catalog <- function(catalog, selected_ids) {
  selected_ids <- as.character(selected_ids)
  selected_ids <- selected_ids[!is.na(selected_ids) & nzchar(selected_ids)]
  if (is.null(catalog)) return(data.frame())
  if (nrow(catalog) == 0L || length(selected_ids) == 0L) {
    return(catalog[0, , drop = FALSE])
  }
  positions <- match(selected_ids, as.character(catalog$id))
  catalog[positions[!is.na(positions)], , drop = FALSE]
}

response_model_coverage <- function(data) {
  usable <- data[
    data$include_model & is.finite(data$A_mean) &
      is.finite(data$Tleaf_mean) & is.finite(data$PPFD_mean),
    ,
    drop = FALSE
  ]
  n_runs <- length(unique(usable$run_id))
  n_lights <- length(unique(round(usable$PPFD_mean, 1)))
  ready <- nrow(usable) >= 16L && n_runs >= 4L && n_lights >= 4L
  reasons <- character()
  if (nrow(usable) < 16L) reasons <- c(reasons, sprintf("%d of 16 usable steps", nrow(usable)))
  if (n_runs < 4L) reasons <- c(reasons, sprintf("%d of 4 temperature runs", n_runs))
  if (n_lights < 4L) reasons <- c(reasons, sprintf("%d of 4 light levels", n_lights))
  list(
    ready = ready,
    data = usable,
    summary = data.frame(
      usable_steps = nrow(usable), distinct_runs = n_runs,
      distinct_light_levels = n_lights,
      status = if (ready) "ready" else "insufficient coverage",
      message = if (ready) {
        "Coverage meets the minimum requirements for exploratory modeling."
      } else {
        paste("Modeling is suppressed:", paste(reasons, collapse = "; "))
      },
      stringsAsFactors = FALSE
    )
  )
}

adaptive_basis_dimensions <- function(data, maximum = c(5L, 5L)) {
  dimensions <- pmin(
    as.integer(maximum),
    c(length(unique(data$Tleaf_mean)), length(unique(data$log_ppfd)))
  )
  dimensions <- pmax(3L, dimensions)
  while (prod(dimensions) >= nrow(data) && any(dimensions > 3L)) {
    index <- which.max(dimensions)
    dimensions[[index]] <- dimensions[[index]] - 1L
  }
  dimensions
}

fit_response_gam <- function(data, grid_size = 70L, basis_dimensions = c(5L, 5L)) {
  coverage <- response_model_coverage(data)
  if (!coverage$ready) {
    return(list(status = "insufficient", message = coverage$summary$message, coverage = coverage))
  }
  model_data <- coverage$data
  model_data$log_ppfd <- log1p(pmax(model_data$PPFD_mean, 0))
  basis_dimensions <- adaptive_basis_dimensions(model_data, basis_dimensions)
  warnings <- character()
  model <- withCallingHandlers(
    tryCatch(
      mgcv::gam(
        A_mean ~ te(Tleaf_mean, log_ppfd, k = basis_dimensions),
        data = model_data, method = "REML", select = TRUE
      ),
      error = function(error) error
    ),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  if (inherits(model, "error")) {
    return(list(
      status = "failed", message = conditionMessage(model),
      coverage = coverage, model = NULL
    ))
  }

  temperatures <- seq(
    min(model_data$Tleaf_mean), max(model_data$Tleaf_mean), length.out = grid_size
  )
  log_lights <- seq(
    min(model_data$log_ppfd), max(model_data$log_ppfd), length.out = grid_size
  )
  interpolation <- tryCatch(
    interp::interp(
      x = model_data$Tleaf_mean, y = model_data$log_ppfd,
      z = model_data$A_mean, xo = temperatures, yo = log_lights,
      linear = TRUE, extrap = FALSE, duplicate = "mean"
    ),
    error = function(error) error
  )
  if (inherits(interpolation, "error")) {
    return(list(
      status = "failed", message = paste("Support mask failed:", conditionMessage(interpolation)),
      coverage = coverage, model = model
    ))
  }
  grid <- expand.grid(
    Tleaf_mean = interpolation$x,
    log_ppfd = interpolation$y,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  grid$PPFD_mean <- expm1(grid$log_ppfd)
  prediction <- stats::predict(model, newdata = grid, se.fit = TRUE)
  inside <- is.finite(as.vector(interpolation$z))
  grid$predicted_A <- ifelse(inside, as.numeric(prediction$fit), NA_real_)
  grid$prediction_se <- ifelse(inside, as.numeric(prediction$se.fit), NA_real_)
  grid$inside_support <- inside

  residual <- stats::residuals(model, type = "deviance")
  model_summary <- summary(model)
  k_check <- tryCatch(as.data.frame(mgcv::k.check(model)), error = function(error) data.frame())
  if (nrow(k_check) > 0L) k_check$smooth <- rownames(k_check)
  cv <- leave_one_run_out_gam(model_data, basis_dimensions)

  diagnostics <- data.frame(
    n_observations = nrow(model_data),
    deviance_explained = unname(model_summary$dev.expl),
    adjusted_r_squared = unname(model_summary$r.sq),
    rmse = sqrt(mean(residual^2, na.rm = TRUE)),
    predictive_r_squared = cv$summary$predictive_r_squared,
    successful_folds = cv$summary$successful_folds,
    total_folds = cv$summary$total_folds,
    k_index = if (nrow(k_check) > 0L) suppressWarnings(min(k_check[["k-index"]], na.rm = TRUE)) else NA_real_,
    k_p_value = if (nrow(k_check) > 0L) suppressWarnings(min(k_check[["p-value"]], na.rm = TRUE)) else NA_real_,
    fit_warnings = paste(unique(warnings), collapse = "; "),
    stringsAsFactors = FALSE
  )

  optima <- response_optima(grid, model_data)
  boundary_fraction <- if (nrow(optima$all) > 0L) {
    mean(optima$all$status == "boundary optimum")
  } else 1
  diagnostics$boundary_optimum_proportion <- boundary_fraction
  diagnostics$supported <- is.finite(diagnostics$deviance_explained) &&
    diagnostics$deviance_explained >= 0.5 &&
    is.finite(diagnostics$adjusted_r_squared) &&
    diagnostics$adjusted_r_squared >= 0.5 &&
    is.finite(diagnostics$predictive_r_squared) &&
    diagnostics$predictive_r_squared >= 0.5 &&
    (is.na(diagnostics$k_p_value) || diagnostics$k_p_value >= 0.05) &&
    boundary_fraction <= 0.5
  reasons <- character()
  if (!is.finite(diagnostics$deviance_explained) || diagnostics$deviance_explained < 0.5) {
    reasons <- c(reasons, "deviance explained is below 0.5")
  }
  if (!is.finite(diagnostics$adjusted_r_squared) || diagnostics$adjusted_r_squared < 0.5) {
    reasons <- c(reasons, "adjusted R² is below 0.5")
  }
  if (!is.finite(diagnostics$predictive_r_squared) || diagnostics$predictive_r_squared < 0.5) {
    reasons <- c(reasons, "leave-one-run-out predictive R² is below 0.5")
  }
  if (is.finite(diagnostics$k_p_value) && diagnostics$k_p_value < 0.05) {
    reasons <- c(reasons, "the GAM basis check detects residual structure")
  }
  if (boundary_fraction > 0.5) {
    reasons <- c(reasons, "more than half of the optima touch sampled boundaries")
  }
  diagnostics$interpretation <- if (diagnostics$supported) {
    "Diagnostics support exploratory surface and optimum summaries."
  } else {
    paste("Interpret fitted outputs cautiously because", paste(reasons, collapse = "; "))
  }

  list(
    status = "success", message = "Exploratory GAM fitted.",
    coverage = coverage, data = model_data, model = model, grid = grid,
    diagnostics = diagnostics, k_check = k_check, cv = cv,
    optima = optima, supported = isTRUE(diagnostics$supported)
  )
}

leave_one_run_out_gam <- function(data, basis_dimensions = c(5L, 5L)) {
  rows <- lapply(unique(data$run_id), function(run) {
    training <- data[data$run_id != run, , drop = FALSE]
    testing <- data[data$run_id == run, , drop = FALSE]
    fold_dimensions <- adaptive_basis_dimensions(training, basis_dimensions)
    fit <- tryCatch(
      mgcv::gam(
        A_mean ~ te(Tleaf_mean, log_ppfd, k = fold_dimensions),
        data = training, method = "REML", select = TRUE
      ),
      error = function(error) error
    )
    prediction <- if (inherits(fit, "error")) {
      rep(NA_real_, nrow(testing))
    } else {
      suppressWarnings(as.numeric(stats::predict(fit, newdata = testing)))
    }
    data.frame(
      run_id = run, observed = testing$A_mean, predicted = prediction,
      residual = testing$A_mean - prediction,
      status = if (inherits(fit, "error")) "failed" else "success",
      stringsAsFactors = FALSE
    )
  })
  predictions <- do.call(rbind, rows)
  valid <- predictions[is.finite(predictions$predicted), , drop = FALSE]
  denominator <- sum((valid$observed - mean(valid$observed))^2)
  r_squared <- if (nrow(valid) > 1L && denominator > 0) {
    1 - sum(valid$residual^2) / denominator
  } else NA_real_
  list(
    predictions = predictions,
    summary = data.frame(
      total_folds = length(unique(predictions$run_id)),
      successful_folds = length(unique(valid$run_id)),
      n_predictions = nrow(valid),
      rmse = if (nrow(valid)) sqrt(mean(valid$residual^2)) else NA_real_,
      predictive_r_squared = r_squared,
      stringsAsFactors = FALSE
    )
  )
}

optimum_from_slice <- function(slice, x_column, response_column = "predicted_A") {
  slice <- slice[is.finite(slice[[response_column]]), , drop = FALSE]
  if (nrow(slice) == 0L) return(NULL)
  slice <- slice[order(slice[[x_column]]), , drop = FALSE]
  maximum_index <- which.max(slice[[response_column]])
  maximum <- slice[[response_column]][[maximum_index]]
  if (!is.finite(maximum) || maximum <= 0) {
    return(data.frame(
      optimum = slice[[x_column]][[maximum_index]], maximum_A = maximum,
      range_low = NA_real_, range_high = NA_real_, status = "no positive optimum"
    ))
  }
  near <- which(slice[[response_column]] >= 0.9 * maximum)
  boundary <- maximum_index %in% c(1L, nrow(slice)) ||
    min(near) == 1L || max(near) == nrow(slice)
  data.frame(
    optimum = slice[[x_column]][[maximum_index]], maximum_A = maximum,
    range_low = slice[[x_column]][[min(near)]],
    range_high = slice[[x_column]][[max(near)]],
    status = if (boundary) "boundary optimum" else "interior optimum"
  )
}

response_optima <- function(grid, model_data) {
  observed_run_temperatures <- aggregate(
    Tleaf_mean ~ run_id, model_data, mean, na.rm = TRUE
  )
  light_rows <- lapply(seq_len(nrow(observed_run_temperatures)), function(index) {
    temperature <- observed_run_temperatures$Tleaf_mean[[index]]
    closest <- grid$Tleaf_mean[which.min(abs(unique(grid$Tleaf_mean) - temperature))]
    slice <- grid[abs(grid$Tleaf_mean - closest) < 1e-10, , drop = FALSE]
    value <- optimum_from_slice(slice, "PPFD_mean")
    if (is.null(value)) return(NULL)
    data.frame(
      type = "Light optimum", run_id = observed_run_temperatures$run_id[[index]],
      fixed_condition = sprintf("Tleaf %.2f°C", temperature),
      optimum_value = value$optimum, optimum_unit = "PPFD",
      near_optimal_low = value$range_low, near_optimal_high = value$range_high,
      maximum_A = value$maximum_A, status = value$status,
      stringsAsFactors = FALSE
    )
  })
  measured_slices <- unique(stats::quantile(
    model_data$PPFD_mean, probs = seq(0, 1, length.out = 6), na.rm = TRUE
  ))
  temperature_rows <- lapply(measured_slices, function(ppfd) {
    unique_lights <- unique(grid$PPFD_mean)
    closest <- unique_lights[which.min(abs(unique_lights - ppfd))]
    slice <- grid[abs(grid$PPFD_mean - closest) < 1e-8, , drop = FALSE]
    value <- optimum_from_slice(slice, "Tleaf_mean")
    if (is.null(value)) return(NULL)
    data.frame(
      type = "Temperature optimum", run_id = "—",
      fixed_condition = sprintf("PPFD %.1f", ppfd),
      optimum_value = value$optimum, optimum_unit = "°C",
      near_optimal_low = value$range_low, near_optimal_high = value$range_high,
      maximum_A = value$maximum_A, status = value$status,
      stringsAsFactors = FALSE
    )
  })
  light <- do.call(rbind, Filter(Negate(is.null), light_rows))
  temperature <- do.call(rbind, Filter(Negate(is.null), temperature_rows))
  all <- rbind(light, temperature)
  list(light = light, temperature = temperature, all = all)
}

run_response_job <- function(
    records,
    catalog,
    cached_parsed = list(),
    cached_extractions = list()) {
  need_extraction <- !records$id %in% names(cached_extractions)
  missing <- records[
    need_extraction & !records$id %in% names(cached_parsed),
    ,
    drop = FALSE
  ]
  batch_results <- if (nrow(missing) > 0L) {
    configure_drive_access("")
    batch_load_remote_measurements(missing)
  } else list()
  parsed <- cached_parsed
  errors <- character()
  for (result in batch_results) {
    if (is.null(result$error)) parsed[[result$id]] <- result$value else {
      errors[[result$id]] <- result$error
    }
  }
  new_extractions <- list()
  audit_runs <- list()
  extractions <- lapply(seq_len(nrow(records)), function(index) {
    record <- records[index, , drop = FALSE]
    id_value <- record$id[[1]]
    extraction <- cached_extractions[[id_value]]
    if (is.null(extraction)) {
      value <- parsed[[id_value]]
      if (is.null(value)) return(NULL)
      extraction <- extract_light_steps(value, measurement_run_id(record$name[[1]]))
      new_extractions[[id_value]] <<- extraction
    }
    run_id <- measurement_run_id(record$name[[1]])
    audit_runs[[run_id]] <<- list(
      raw = extraction$raw,
      summary = extraction$summary
    )
    extraction$summary$id <- record$id[[1]]
    extraction$summary$name <- record$name[[1]]
    extraction$summary$species <- catalog$species[catalog$id == record$id[[1]]][[1]]
    extraction$summary$plant_id <- catalog$plant_id[catalog$id == record$id[[1]]][[1]]
    extraction$summary$colour <- catalog$colour[catalog$id == record$id[[1]]][[1]]
    extraction$summary
  })
  extractions <- Filter(Negate(is.null), extractions)
  steps <- if (length(extractions)) do.call(rbind, extractions) else data.frame()
  model <- if (nrow(steps)) fit_response_gam(steps) else list(
    status = "insufficient", message = "No runs could be extracted."
  )
  list(
    steps = steps, model = model, batch_results = batch_results,
    new_extractions = new_extractions, errors = errors, selection = catalog,
    audit_runs = audit_runs
  )
}

WALZ_TLEAF_PALETTE <- c(
  "#2166AC", "#D1E5F0", "#FDDBC7", "#EF8A62", "#B2182B"
)

walz_tleaf_colorscale <- function() {
  Map(
    function(position, colour) c(position, colour),
    seq(0, 1, length.out = length(WALZ_TLEAF_PALETTE)),
    WALZ_TLEAF_PALETTE
  )
}

walz_tleaf_colour <- function(temperature, limits) {
  if (length(limits) != 2L || any(!is.finite(limits))) return("#7F7F7F")
  span <- diff(limits)
  position <- if (!is.finite(span) || span <= 0) {
    0.5
  } else {
    (temperature - limits[[1]]) / span
  }
  position <- max(0, min(1, position))
  ramp <- grDevices::colorRamp(WALZ_TLEAF_PALETTE)
  rgb <- ramp(position)
  grDevices::rgb(rgb[[1]], rgb[[2]], rgb[[3]], maxColorValue = 255)
}

make_observed_response_plot <- function(steps) {
  usable <- steps[steps$include_model, , drop = FALSE]
  if (nrow(usable) == 0L) return(plotly::plotly_empty(type = "scatter", mode = "lines"))
  temperatures <- aggregate(Tleaf_mean ~ run_id, usable, mean, na.rm = TRUE)
  range_t <- range(temperatures$Tleaf_mean, finite = TRUE)
  colour_range <- if (diff(range_t) == 0) range_t + c(-0.5, 0.5) else range_t
  colour_scale <- walz_tleaf_colorscale()
  plot <- plotly::plot_ly()
  runs <- unique(usable$run_id)
  for (index in seq_along(runs)) {
    run <- runs[[index]]
    data <- usable[usable$run_id == run, , drop = FALSE]
    data <- data[order(data$PPFD_mean), , drop = FALSE]
    temperature <- mean(data$Tleaf_mean, na.rm = TRUE)
    colour <- walz_tleaf_colour(temperature, colour_range)
    plot <- plotly::add_trace(
      plot, x = data$PPFD_mean, y = data$A_mean,
      type = "scatter", mode = "lines+markers", name = run,
      line = list(color = colour, width = 2),
      marker = list(
        color = rep(temperature, nrow(data)), line = list(color = colour),
        colorscale = colour_scale, reversescale = FALSE,
        cmin = colour_range[[1]], cmax = colour_range[[2]],
        showscale = index == 1L,
        colorbar = list(title = list(text = "Mean Tleaf (°C)"))
      ),
      text = sprintf("%s<br>Mean Tleaf %.2f°C<br>A %.3f<br>PPFD %.1f", run, temperature, data$A_mean, data$PPFD_mean),
      hovertemplate = "%{text}<extra></extra>", showlegend = FALSE
    )
  }
  plotly::layout(
    plot, xaxis = list(title = "Measured PPFD"), yaxis = list(title = "A"),
    margin = list(r = 105)
  )
}

make_temperature_slice_plot <- function(model_result) {
  if (is.null(model_result$model)) {
    return(plotly::plotly_empty(type = "scatter", mode = "lines"))
  }
  grid <- model_result$grid
  slices <- unique(stats::quantile(model_result$data$PPFD_mean, seq(0, 1, length.out = 6)))
  colours <- grDevices::hcl.colors(length(slices), "YlOrRd", rev = TRUE)
  light_range <- range(slices, finite = TRUE)
  if (diff(light_range) == 0) light_range <- light_range + c(-0.5, 0.5)
  plot <- plotly::plot_ly()
  for (index in seq_along(slices)) {
    light <- unique(grid$PPFD_mean)[which.min(abs(unique(grid$PPFD_mean) - slices[[index]]))]
    data <- grid[abs(grid$PPFD_mean - light) < 1e-8 & is.finite(grid$predicted_A), , drop = FALSE]
    plot <- plotly::add_trace(
      plot, x = data$Tleaf_mean, y = data$predicted_A,
      type = "scatter", mode = "lines+markers",
      name = sprintf("PPFD %.0f", slices[[index]]),
      line = list(color = colours[[index]], width = 2),
      marker = list(
        color = rep(slices[[index]], nrow(data)), size = 4,
        colorscale = "YlOrRd", cmin = light_range[[1]], cmax = light_range[[2]],
        showscale = index == 1L,
        colorbar = list(title = list(text = "Measured PPFD"), tickvals = slices)
      ),
      text = sprintf(
        "PPFD %.1f<br>Tleaf %.2f°C<br>Modeled A %.3f",
        slices[[index]], data$Tleaf_mean, data$predicted_A
      ),
      hovertemplate = "%{text}<extra></extra>", showlegend = FALSE
    )
  }
  plotly::layout(
    plot, xaxis = list(title = "Leaf temperature (°C)"),
    yaxis = list(title = "Modeled A"),
    margin = list(r = 105)
  )
}

make_raw_response_3d <- function(steps) {
  data <- steps[steps$include_model, , drop = FALSE]
  plotly::plot_ly(
    data, x = ~Tleaf_mean, y = ~PPFD_mean, z = ~A_mean,
    type = "scatter3d", mode = "markers", color = ~A_mean,
    colors = grDevices::hcl.colors(20L, "Viridis"), text = ~run_id,
    hovertemplate = "%{text}<br>Tleaf %{x:.2f}<br>PPFD %{y:.1f}<br>A %{z:.3f}<extra></extra>"
  ) |>
    plotly::layout(scene = list(
      xaxis = list(title = "Tleaf (°C)"),
      yaxis = list(title = "PPFD"), zaxis = list(title = "A")
    ))
}

make_surface_response_3d <- function(model_result) {
  if (is.null(model_result$model)) {
    return(plotly::plotly_empty(type = "scatter3d", mode = "markers"))
  }
  grid <- model_result$grid
  temperatures <- sort(unique(grid$Tleaf_mean))
  lights <- sort(unique(grid$PPFD_mean))
  z <- matrix(grid$predicted_A, nrow = length(temperatures), ncol = length(lights))
  plot <- plotly::plot_ly(
    x = temperatures, y = lights, z = z,
    type = "surface", colorscale = "Viridis", opacity = 0.78,
    name = "GAM surface", showscale = TRUE
  )
  data <- model_result$data
  plot <- plotly::add_markers(
    plot, x = data$Tleaf_mean, y = data$PPFD_mean, z = data$A_mean,
    inherit = FALSE,
    marker = list(color = "white", size = 3, line = list(color = "#263238", width = 1)),
    text = data$run_id, name = "Observed steps",
    hovertemplate = "%{text}<br>Tleaf %{x:.2f}<br>PPFD %{y:.1f}<br>A %{z:.3f}<extra></extra>"
  )
  ridge <- model_result$optima$light
  if (!is.null(ridge) && nrow(ridge) > 0L) {
    ridge_t <- suppressWarnings(as.numeric(sub(".*Tleaf ([0-9.-]+).*", "\\1", ridge$fixed_condition)))
    plot <- plotly::add_trace(
      plot, x = ridge_t, y = ridge$optimum_value, z = ridge$maximum_A,
      type = "scatter3d", mode = "lines+markers", name = "Optimum ridge",
      line = list(color = "#f4b942", width = 6), marker = list(color = "#f4b942", size = 3)
    )
  }
  plotly::layout(plot, scene = list(
    xaxis = list(title = "Tleaf (°C)"),
    yaxis = list(title = "PPFD"), zaxis = list(title = "A")
  ))
}

prepare_normalized_response_audit <- function(audit_runs) {
  empty_raw <- data.frame(
    run_id = character(), elapsed_minutes = numeric(), A_raw = numeric(),
    A_normalized = numeric(), positive_max = numeric(), stringsAsFactors = FALSE
  )
  empty_windows <- data.frame(
    run_id = character(), step_id = integer(), window_start = numeric(),
    window_end = numeric(), window_midpoint = numeric(), A_mean = numeric(),
    A_sd = numeric(), A_mean_normalized = numeric(), A_sd_normalized = numeric(),
    PPFD_mean = numeric(), coverage = numeric(), window_type = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(audit_runs) || length(audit_runs) == 0L) {
    return(list(raw = empty_raw, windows = empty_windows, warnings = character(), run_ids = character()))
  }

  raw_rows <- list()
  window_rows <- list()
  warnings <- character()
  included_runs <- character()
  for (run_id in names(audit_runs)) {
    extraction <- audit_runs[[run_id]]
    raw <- extraction$raw
    summary <- extraction$summary
    if (
      is.null(raw) || nrow(raw) == 0L ||
        !all(c("Datetime", ".elapsed_minutes", "A") %in% names(raw))
    ) {
      warnings <- c(warnings, sprintf("%s has no usable raw A timeseries.", run_id))
      next
    }
    positive <- suppressWarnings(as.numeric(raw$A))
    positive <- positive[is.finite(positive) & positive > 0]
    if (length(positive) == 0L) {
      warnings <- c(warnings, sprintf(
        "%s has no positive A maximum and cannot be max-normalized.", run_id
      ))
      next
    }
    positive_max <- max(positive)
    included_runs <- c(included_runs, run_id)
    raw_rows[[run_id]] <- data.frame(
      run_id = run_id,
      elapsed_minutes = suppressWarnings(as.numeric(raw$.elapsed_minutes)),
      A_raw = suppressWarnings(as.numeric(raw$A)),
      A_normalized = suppressWarnings(as.numeric(raw$A)) / positive_max,
      positive_max = positive_max,
      stringsAsFactors = FALSE
    )
    if (is.null(summary) || nrow(summary) == 0L) next
    start_time <- min(raw$Datetime, na.rm = TRUE)
    window_start <- as.numeric(difftime(summary$window_start, start_time, units = "mins"))
    window_end <- as.numeric(difftime(summary$window_end, start_time, units = "mins"))
    final_step <- max(summary$step_id, na.rm = TRUE)
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
      window_type = ifelse(
        summary$step_id == final_step,
        "final three minutes of terminal step",
        "three minutes before next light step"
      ),
      stringsAsFactors = FALSE
    )
  }
  raw_data <- if (length(raw_rows)) do.call(rbind, raw_rows) else empty_raw
  window_data <- if (length(window_rows)) do.call(rbind, window_rows) else empty_windows
  if (length(included_runs)) {
    raw_data$run_id <- factor(raw_data$run_id, levels = included_runs)
    window_data$run_id <- factor(window_data$run_id, levels = included_runs)
  }
  list(
    raw = raw_data, windows = window_data,
    warnings = unique(warnings), run_ids = included_runs
  )
}

normalized_response_audit_columns <- function(run_count) {
  if (run_count > 12L) 3L else 2L
}

make_normalized_response_audit_plot <- function(prepared, columns = NULL) {
  if (is.null(prepared) || nrow(prepared$raw) == 0L) {
    return(plotly::plotly_empty(type = "scatter", mode = "lines"))
  }
  run_count <- length(prepared$run_ids)
  if (is.null(columns)) columns <- normalized_response_audit_columns(run_count)
  mean_colour <- WALZ_DARK2[[1]]
  plot <- ggplot2::ggplot(
    prepared$raw,
    ggplot2::aes(x = elapsed_minutes, y = A_normalized, group = run_id)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "#a8b0aa", linewidth = 0.3) +
    ggplot2::geom_rect(
      data = prepared$windows,
      ggplot2::aes(
        xmin = window_start, xmax = window_end,
        ymin = -Inf, ymax = Inf
      ),
      inherit.aes = FALSE, fill = mean_colour, alpha = 0.055
    ) +
    ggplot2::geom_line(colour = "#56616b", linewidth = 0.38, na.rm = TRUE) +
    ggplot2::geom_segment(
      data = prepared$windows,
      ggplot2::aes(
        x = window_start, xend = window_end,
        y = A_mean_normalized, yend = A_mean_normalized
      ),
      inherit.aes = FALSE, colour = mean_colour, linewidth = 1.7,
      lineend = "round", na.rm = TRUE
    ) +
    ggplot2::geom_errorbar(
      data = prepared$windows,
      ggplot2::aes(
        x = window_midpoint,
        ymin = A_mean_normalized - A_sd_normalized,
        ymax = A_mean_normalized + A_sd_normalized
      ),
      inherit.aes = FALSE, colour = mean_colour, alpha = 0.58,
      linewidth = 0.55, width = 0.5, na.rm = TRUE
    ) +
    ggplot2::facet_wrap(
      stats::as.formula("~run_id"), ncol = columns, scales = "free_x"
    ) +
    ggplot2::labs(
      x = "Elapsed time from run start (minutes)",
      y = "A / positive run maximum"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#e7ebe7", linewidth = 0.3),
      strip.background = ggplot2::element_rect(fill = "#eef4ed", colour = NA),
      strip.text = ggplot2::element_text(colour = "#243228", face = "bold"),
      axis.title = ggplot2::element_text(colour = "#34443a"),
      axis.text = ggplot2::element_text(colour = "#4b5563")
    )
  plotly::ggplotly(plot, dynamicTicks = TRUE) |>
    plotly::layout(
      hovermode = "closest",
      margin = list(l = 70, r = 25, t = 35, b = 65)
    )
}

model_fit_warning_message <- function(model_result) {
  if (is.null(model_result) || !identical(model_result$status, "success")) {
    message <- if (is.null(model_result$message)) {
      "The fitted response is unavailable."
    } else model_result$message
    return(paste(message, "Raw and extracted observations remain available."))
  }
  diagnostics <- model_result$diagnostics
  diagnostic_value <- function(name) {
    if (is.null(diagnostics) || !name %in% names(diagnostics) || length(diagnostics[[name]]) == 0L) {
      return(NA_real_)
    }
    suppressWarnings(as.numeric(diagnostics[[name]][[1]]))
  }
  reasons <- character()
  deviance_explained <- diagnostic_value("deviance_explained")
  if (!is.finite(deviance_explained) || deviance_explained < 0.5) {
    value <- if (is.finite(deviance_explained)) sprintf("%.2f", deviance_explained) else "unavailable"
    reasons <- c(reasons, sprintf(
      "deviance explained = %s (minimum 0.50)", value
    ))
  }
  adjusted_r_squared <- diagnostic_value("adjusted_r_squared")
  if (!is.finite(adjusted_r_squared) || adjusted_r_squared < 0.5) {
    value <- if (is.finite(adjusted_r_squared)) sprintf("%.2f", adjusted_r_squared) else "unavailable"
    reasons <- c(reasons, sprintf(
      "adjusted R² = %s (minimum 0.50)", value
    ))
  }
  predictive_r_squared <- diagnostic_value("predictive_r_squared")
  if (!is.finite(predictive_r_squared) || predictive_r_squared < 0.5) {
    value <- if (is.finite(predictive_r_squared)) sprintf("%.2f", predictive_r_squared) else "unavailable"
    reasons <- c(reasons, sprintf(
      "leave-one-run-out predictive R² = %s (minimum 0.50)", value
    ))
  }
  k_p_value <- diagnostic_value("k_p_value")
  if (is.finite(k_p_value) && k_p_value < 0.05) {
    reasons <- c(reasons, sprintf(
      "GAM basis-check p-value = %.3f (minimum 0.05)", k_p_value
    ))
  }
  boundary_fraction <- diagnostic_value("boundary_optimum_proportion")
  if (is.finite(boundary_fraction) && boundary_fraction > 0.5) {
    reasons <- c(reasons, sprintf(
      "boundary optima = %.0f%% (maximum 50%%)", 100 * boundary_fraction
    ))
  }
  if (length(reasons) == 0L) {
    reasons <- "one or more model-quality checks were not adequate"
  }
  paste0(
    "Model quality is low: ",
    paste(reasons, collapse = "; "),
    ". The fitted curves, surface, and optima are still shown, but should be interpreted cautiously."
  )
}

response_model_notice_ui <- function(model_result = NULL) {
  if (is.null(model_result)) return(NULL)
  if (!identical(model_result$status, "success")) {
    return(alert_ui(model_fit_warning_message(model_result), "warning"))
  }
  if (!isTRUE(model_result$supported)) {
    return(alert_ui(model_fit_warning_message(model_result), "warning"))
  }
  NULL
}

response_optima_section_ui <- function(ns, model_result = NULL) {
  if (is.null(model_result) || !identical(model_result$status, "success")) return(NULL)
  bslib::card(
    bslib::card_header("Optima and ≥90% near-optimal ranges"),
    shiny::uiOutput(ns("optima_table"))
  )
}

response_download_buttons_ui <- function(ns, model_result = NULL) {
  fitted <- !is.null(model_result) && identical(model_result$status, "success")
  shiny::div(
    class = "download-grid",
    shiny::downloadButton(ns("download_steps"), "Extracted steps"),
    if (fitted) shiny::downloadButton(ns("download_predictions"), "Model predictions") else NULL,
    if (fitted) shiny::downloadButton(ns("download_optima"), "Optima") else NULL,
    if (fitted) {
      shiny::downloadButton(ns("download_diagnostics"), "Diagnostics")
    } else NULL
  )
}

simple_data_table_ui <- function(data, digits = 3L) {
  if (is.null(data) || nrow(data) == 0L) return(alert_ui("No rows to display.", "info"))
  display <- data
  display[] <- lapply(display, function(column) {
    if (is.numeric(column)) format(round(column, digits), trim = TRUE, scientific = FALSE) else as.character(column)
  })
  shiny::div(class = "full-metadata-scroll", shiny::tags$table(
    class = "run-metadata-table full-metadata-table",
    shiny::tags$thead(shiny::tags$tr(lapply(names(display), shiny::tags$th))),
    shiny::tags$tbody(lapply(seq_len(nrow(display)), function(row) {
      shiny::tags$tr(lapply(display[row, , drop = TRUE], function(value) {
        shiny::tags$td(ifelse(is.na(value) || !nzchar(value), "—", value))
      }))
    }))
  ))
}

response_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h5("Response Analysis selection"),
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
      shiny::selectInput(ns("species"), "Species (required)", choices = character()),
      shiny::selectizeInput(ns("plant_ids"), "Plant IDs", choices = character(), multiple = TRUE),
      shiny::checkboxGroupInput(
        ns("qualities"), "Quality assessment",
        choices = c("Good" = "good", "Medium" = "medium", "Bad" = "bad", "Unassessed" = "unassessed"),
        selected = "good"
      ),
      bslib::accordion(
        bslib::accordion_panel(
          "Advanced filters",
          shiny::dateRangeInput(ns("date_range"), "Date"),
          shiny::selectizeInput(ns("walz_number"), "WALZ number", choices = character(), multiple = TRUE),
          shiny::selectizeInput(ns("target_tcuv"), "Target Tcuv", choices = character(), multiple = TRUE),
          shiny::selectizeInput(ns("h2o_input"), "H2O input", choices = character(), multiple = TRUE),
          shiny::selectizeInput(ns("co2_input"), "CO2 input", choices = character(), multiple = TRUE),
          shiny::selectizeInput(ns("xibox_temperature"), "XiBox temperature", choices = character(), multiple = TRUE),
          shiny::selectizeInput(ns("xibox_light"), "XiBox light", choices = character(), multiple = TRUE),
          shiny::selectizeInput(ns("xibox_humidity"), "XiBox humidity", choices = character(), multiple = TRUE),
          shiny::textInput(ns("protocol_text"), "Protocol description contains")
        )
      )
    ),
    shiny::conditionalPanel(
      condition = "input.selection_mode === 'manual'",
      ns = ns,
      shiny::selectInput(
        ns("run_preset"), "Run preset",
        choices = c("Custom selection" = "", stats::setNames(
          names(WALZ_RESPONSE_PRESET_LABELS), WALZ_RESPONSE_PRESET_LABELS
        ))
      ),
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
        "The First Analysis presets reproduce the separate Oak or Beech temperature series. You can add or remove runs after applying a preset."
      ),
      shiny::uiOutput(ns("preset_status"))
    ),
    shiny::actionButton(ns("run"), "Run analysis", icon = shiny::icon("play"), class = "btn-primary")
  )
}

response_model_theory_ui <- function() {
  bslib::card(
    bslib::card_header("Model theory"),
    shiny::div(
      class = "model-theory",
      shiny::p(
        shiny::strong("Data: "),
        "Each model point is the mean of the final three minutes of a complete, stable light step. The response is net CO₂ assimilation (A); the predictors are mean leaf temperature and measured PPFD from PARtop."
      ),
      shiny::p(
        shiny::strong("Model: "),
        "The exploratory generalized additive model is ",
        shiny::tags$code("A_mean = β₀ + f(Tleaf_mean, log(1 + PPFD_mean)) + ε"),
        ". The two-dimensional tensor-product smooth is fitted with REML and automatic shrinkage, allowing a curved temperature–light interaction without imposing a fixed response shape."
      ),
      shiny::p(
        shiny::strong("Optima: "),
        "Predictions outside the sampled temperature–PPFD support are masked. Optima are maxima along slices through the fitted surface; the ≥90% range contains conditions whose predicted A is at least 90% of that maximum. A boundary optimum means the sampled range did not establish a turning point."
      ),
      shiny::p(
        shiny::strong("Interpretation: "),
        "All selected runs are pooled into one exploratory surface, without a separate run effect. Use the extraction audit and model diagnostics to judge reliability; low diagnostic scores trigger a warning but do not hide the fitted results."
      )
    )
  )
}

response_normalized_audit_section_ui <- function(ns, prepared = NULL) {
  if (is.null(prepared)) return(NULL)
  warning_ui <- if (length(prepared$warnings)) {
    alert_ui(paste(prepared$warnings, collapse = " "), "warning")
  } else NULL
  if (nrow(prepared$raw) == 0L) {
    return(bslib::card(
      bslib::card_header("Max-normalized raw A and extraction windows"),
      warning_ui,
      alert_ui("No selected run could be max-normalized.", "warning")
    ))
  }
  columns <- normalized_response_audit_columns(length(prepared$run_ids))
  rows <- ceiling(length(prepared$run_ids) / columns)
  height <- max(440L, 265L * rows + 90L)
  bslib::card(
    bslib::card_header("Max-normalized raw A and extraction windows"),
    shiny::p(
      class = "control-help",
      "Each facet is one selected run. The grey trace is raw A divided by that run's positive maximum. Pale bands mark extraction windows; thick green lines show the three-minute means and the lighter vertical bars show mean ± 1 SD. The terminal step uses its final three minutes even though no later light jump follows."
    ),
    warning_ui,
    plotly::plotlyOutput(ns("normalized_audit_plot"), height = sprintf("%dpx", height))
  )
}

response_main_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "response-analysis-tab",
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("model_notice")),
    shiny::div(
      class = "response-two-column",
      bslib::card(bslib::card_header("Observed A versus measured PPFD"), plotly::plotlyOutput(ns("light_plot"), height = "520px")),
      bslib::card(bslib::card_header("Modeled A versus Tleaf at PPFD slices"), plotly::plotlyOutput(ns("temperature_plot"), height = "520px"))
    ),
    shiny::div(
      class = "response-two-column",
      bslib::card(bslib::card_header("Raw extracted landscape"), plotly::plotlyOutput(ns("raw_3d"), height = "600px")),
      bslib::card(bslib::card_header("GAM surface within observed support"), plotly::plotlyOutput(ns("surface_3d"), height = "600px"))
    ),
    shiny::uiOutput(ns("optima_section")),
    bslib::card(
      bslib::card_header("Model diagnostics"),
      shiny::uiOutput(ns("diagnostics")),
      shiny::uiOutput(ns("download_buttons"))
    ),
    shiny::uiOutput(ns("normalized_audit_section")),
    bslib::card(
      bslib::card_header("Extraction audit"),
      shiny::uiOutput(ns("extraction_table"))
    ),
    response_model_theory_ui()
  )
}

response_analysis_server <- function(
    id,
    measurements,
    metadata,
    catalog,
    shared_prepared,
    config) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal(NULL)
    running <- shiny::reactiveVal(FALSE)
    last_error <- shiny::reactiveVal(NULL)

    shiny::observe({
      current <- catalog()
      exact <- current[current$metadata_matches == 1L, , drop = FALSE]
      species <- sort(unique(exact$species[nzchar(exact$species)]))
      shiny::updateSelectInput(session, "species", choices = c("Choose a species" = "", species))
      choices <- c("walz_number", "target_tcuv", "h2o_input", "co2_input", "xibox_temperature", "xibox_light", "xibox_humidity")
      for (column in choices) {
        values <- sort(unique(exact[[column]][nzchar(exact[[column]])]))
        shiny::updateSelectizeInput(session, column, choices = values, server = TRUE)
      }
      dates <- exact$date[!is.na(exact$date)]
      if (length(dates)) shiny::updateDateRangeInput(session, "date_range", start = min(dates), end = max(dates))

      selected <- if (is.null(input$manual_runs)) character() else input$manual_runs
      selected <- selected[selected %in% as.character(current$id)]
      shiny::updateSelectizeInput(
        session, "manual_runs", choices = response_run_choices(current),
        selected = selected, server = FALSE
      )
    })

    selected_catalog <- shiny::reactive({
      current <- catalog()
      mode <- if (is.null(input$selection_mode)) "filters" else input$selection_mode
      if (identical(mode, "manual")) {
        return(manual_response_catalog(current, input$manual_runs))
      }
      current <- current[current$metadata_matches == 1L, , drop = FALSE]
      if (is.null(input$species) || !nzchar(input$species)) return(current[0, , drop = FALSE])
      current <- current[current$species == input$species, , drop = FALSE]
      current <- filter_run_catalog(
        current, plant_ids = input$plant_ids,
        date_range = input$date_range, qualities = input$qualities
      )
      filter_columns <- c("walz_number", "target_tcuv", "h2o_input", "co2_input", "xibox_temperature", "xibox_light", "xibox_humidity")
      for (column in filter_columns) {
        selected <- as.character(input[[column]])
        selected <- selected[!is.na(selected) & nzchar(selected)]
        if (length(selected)) current <- current[current[[column]] %in% selected, , drop = FALSE]
      }
      protocol_text <- if (is.null(input$protocol_text)) "" else trimws(input$protocol_text)
      if (nzchar(protocol_text)) {
        current <- current[grepl(protocol_text, current$protocol_description, ignore.case = TRUE, fixed = TRUE), , drop = FALSE]
      }
      current
    })

    shiny::observeEvent(input$run_preset, {
      if (is.null(input$run_preset) || !nzchar(input$run_preset)) return()
      current <- catalog()
      resolved <- resolve_response_preset(input$run_preset, current)
      shiny::updateSelectizeInput(
        session, "manual_runs", choices = response_run_choices(current),
        selected = resolved$ids, server = FALSE
      )
    }, ignoreInit = TRUE)

    output$preset_status <- shiny::renderUI({
      if (is.null(input$run_preset) || !nzchar(input$run_preset)) return(NULL)
      resolved <- resolve_response_preset(input$run_preset, catalog())
      label <- unname(WALZ_RESPONSE_PRESET_LABELS[[input$run_preset]])
      if (length(resolved$missing) == 0L) {
        return(alert_ui(sprintf(
          "%s: all %d curated run(s) are available.", label, length(resolved$ids)
        ), "info"))
      }
      alert_ui(sprintf(
        "%s: %d available; %d missing from Drive (%s).",
        label, length(resolved$ids), length(resolved$missing),
        paste(resolved$missing, collapse = ", ")
      ), "warning")
    })

    shiny::observe({
      current <- catalog()
      species_current <- if (!is.null(input$species)) input$species else ""
      plants <- sort(unique(current$plant_id[
        current$species == species_current & nzchar(current$plant_id)
      ]))
      shiny::updateSelectizeInput(session, "plant_ids", choices = plants, server = TRUE)
    })

    shiny::observeEvent(input$run, {
      selected <- selected_catalog()
      last_error(NULL)
      mode <- if (is.null(input$selection_mode)) "filters" else input$selection_mode
      if (identical(mode, "filters")) {
        if (is.null(input$species) || !nzchar(input$species)) {
          last_error("Choose one species before running the analysis.")
          return()
        }
        if (length(input$qualities) == 0L) {
          last_error("Select at least one quality assessment.")
          return()
        }
      } else if (length(input$manual_runs) == 0L) {
        last_error("Choose at least one measurement run or apply a preset.")
        return()
      }
      if (nrow(selected) == 0L) {
        last_error(if (identical(mode, "filters")) {
          paste0(
            "No exact metadata/file matches meet these filters. The default is Good only; ",
            "unassessed runs are not included silently."
          )
        } else {
          "None of the manually selected runs are currently available."
        })
        return()
      }
      records <- measurements()[match(selected$id, measurements()$id), , drop = FALSE]
      shared <- shared_prepared()
      cached <- list()
      cached_extractions <- list()
      for (id_value in selected$id) {
        if (!is.null(shared[[id_value]]) && is.null(shared[[id_value]]$error)) {
          cached[[id_value]] <- shared[[id_value]]$value
        } else {
          record <- records[records$id == id_value, , drop = FALSE]
          local <- get_remote_cached("measurement", record)
          if (!is.null(local)) cached[[id_value]] <- local
        }
        record <- records[records$id == id_value, , drop = FALSE]
        extraction <- get_cached_light_steps(record)
        if (!is.null(extraction)) cached_extractions[[id_value]] <- extraction
      }
      running(TRUE)
      ensure_background_workers(config$background_workers)
      promise <- promises::future_promise({
        run_response_job(records, selected, cached, cached_extractions)
      }, seed = TRUE)
      promises::then(
        promise,
        onFulfilled = function(value) {
          if (length(value$batch_results)) merge_measurement_batch_cache(records, value$batch_results)
          if (length(value$new_extractions)) {
            for (id_value in names(value$new_extractions)) {
              record <- records[records$id == id_value, , drop = FALSE]
              cache_light_steps(record, value$new_extractions[[id_value]])
            }
          }
          result(value)
          running(FALSE)
        },
        onRejected = function(error) {
          last_error(conditionMessage(error))
          running(FALSE)
        }
      )
    }, ignoreInit = TRUE)

    normalized_audit <- shiny::reactive({
      value <- result()
      if (is.null(value)) return(NULL)
      prepare_normalized_response_audit(value$audit_runs)
    })

    output$status <- shiny::renderUI({
      if (running()) return(alert_ui("Loading selected runs and fitting the exploratory model in the background …", "info"))
      if (!is.null(last_error())) return(alert_ui(last_error(), "warning"))
      current_catalog <- catalog()
      current_metadata <- metadata()
      unmatched_files <- current_catalog$run_id[current_catalog$metadata_matches != 1L]
      missing_files <- if (is.null(current_metadata) || nrow(current_metadata) == 0L) {
        character()
      } else {
        setdiff(current_metadata$.run_id, normalize_run_id(current_catalog$run_id))
      }
      source_notes <- list()
      if (length(unmatched_files)) source_notes <- c(source_notes, list(alert_ui(sprintf(
        "%d measurement file(s) lack a unique metadata row and are unavailable to metadata-filter mode; they can still be chosen manually.",
        length(unmatched_files)
      ), "warning")))
      if (length(missing_files)) source_notes <- c(source_notes, list(alert_ui(sprintf(
        "%d metadata row(s) have no matching measurement file and are reported separately.",
        length(missing_files)
      ), "warning")))
      value <- result()
      mode <- if (is.null(input$selection_mode)) "filters" else input$selection_mode
      if (is.null(value)) return(shiny::tagList(
        alert_ui(if (identical(mode, "filters")) {
          "Choose a species and press Run analysis. Good-quality runs are selected by default."
        } else {
          "Choose runs directly or apply a First Analysis preset, then press Run analysis."
        }, "info"),
        source_notes
      ))
      model <- value$model
      if (!identical(model$status, "success")) return(alert_ui(model$message, "warning"))
      shiny::tagList(
        source_notes,
        if (length(unique(value$selection$species[nzchar(value$selection$species)])) > 1L) {
          alert_ui("The manual selection contains multiple species; the exploratory GAM pools them into one response surface.", "warning")
        },
        alert_ui(model$coverage$summary$message, "info"),
        if (isTRUE(model$supported)) alert_ui(model$diagnostics$interpretation, "info") else NULL,
        if (length(value$errors)) alert_ui(sprintf(
          "%d selected file(s) could not be loaded and are reported separately.", length(value$errors)
        ), "warning")
      )
    })

    output$extraction_table <- shiny::renderUI({
      value <- result()
      if (is.null(value) || nrow(value$steps) == 0L) return(alert_ui("Run the analysis to prepare extraction rows.", "info"))
      display <- value$steps[, c(
        "run_id", "step_id", "PPFD_mean", "PPFD_sd", "A_mean", "A_sd",
        "A_slope", "Tleaf_mean", "Tleaf_sd", "n_window", "coverage",
        "window_complete", "include_model", "warning"
      ), drop = FALSE]
      display$coverage <- round(100 * display$coverage, 1)
      simple_data_table_ui(display)
    })
    output$light_plot <- plotly::renderPlotly({
      value <- result()
      if (is.null(value)) plotly::plotly_empty(type = "scatter", mode = "lines") else make_observed_response_plot(value$steps)
    })
    output$temperature_plot <- plotly::renderPlotly({
      value <- result()
      if (is.null(value)) plotly::plotly_empty(type = "scatter", mode = "lines") else make_temperature_slice_plot(value$model)
    })
    output$raw_3d <- plotly::renderPlotly({
      value <- result()
      if (is.null(value)) plotly::plotly_empty(type = "scatter3d", mode = "markers") else make_raw_response_3d(value$steps)
    })
    output$model_notice <- shiny::renderUI({
      value <- result()
      response_model_notice_ui(if (is.null(value)) NULL else value$model)
    })
    output$optima_section <- shiny::renderUI({
      value <- result()
      response_optima_section_ui(session$ns, if (is.null(value)) NULL else value$model)
    })
    output$surface_3d <- plotly::renderPlotly({
      value <- result()
      if (is.null(value)) plotly::plotly_empty(type = "scatter3d", mode = "markers") else make_surface_response_3d(value$model)
    })
    output$optima_table <- shiny::renderUI({
      value <- result()
      if (is.null(value)) return(alert_ui("Run the analysis to estimate optima.", "info"))
      if (!identical(value$model$status, "success")) {
        return(alert_ui("Optimum estimates are unavailable because the model could not be fitted.", "warning"))
      }
      simple_data_table_ui(value$model$optima$all)
    })
    output$diagnostics <- shiny::renderUI({
      value <- result()
      if (is.null(value) || !identical(value$model$status, "success")) return(NULL)
      shiny::tagList(
        simple_data_table_ui(value$model$diagnostics),
        if (nrow(value$model$k_check)) simple_data_table_ui(value$model$k_check) else NULL
      )
    })
    output$download_buttons <- shiny::renderUI({
      value <- result()
      response_download_buttons_ui(session$ns, if (is.null(value)) NULL else value$model)
    })
    output$normalized_audit_section <- shiny::renderUI({
      response_normalized_audit_section_ui(session$ns, normalized_audit())
    })
    output$normalized_audit_plot <- plotly::renderPlotly({
      prepared <- normalized_audit()
      if (is.null(prepared)) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines"))
      }
      make_normalized_response_audit_plot(
        prepared,
        columns = normalized_response_audit_columns(length(prepared$run_ids))
      )
    })

    csv_download <- function(filename, getter) {
      shiny::downloadHandler(
        filename = function() filename,
        content = function(path) utils::write.csv(getter(), path, row.names = FALSE, na = "")
      )
    }
    output$download_steps <- csv_download("walz-extracted-steps.csv", function() {
      shiny::req(result()); result()$steps
    })
    output$download_predictions <- csv_download("walz-model-predictions.csv", function() {
      shiny::req(result(), identical(result()$model$status, "success")); result()$model$grid
    })
    output$download_optima <- csv_download("walz-optima.csv", function() {
      shiny::req(result(), identical(result()$model$status, "success")); result()$model$optima$all
    })
    output$download_diagnostics <- csv_download("walz-model-diagnostics.csv", function() {
      shiny::req(result(), identical(result()$model$status, "success")); result()$model$diagnostics
    })
  })
}
