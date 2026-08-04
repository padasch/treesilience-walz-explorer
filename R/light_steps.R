.walz_step_cache <- new.env(parent = emptyenv())

repair_transient_setpoints <- function(values, max_records = 2L, tolerance = 1e-8) {
  repaired <- suppressWarnings(as.numeric(values))
  if (length(repaired) < 3L || all(is.na(repaired))) {
    return(list(values = repaired, repaired = rep(FALSE, length(repaired))))
  }

  changed <- rep(FALSE, length(repaired))
  for (iteration in seq_len(5L)) {
    encoded <- rle(repaired)
    ends <- cumsum(encoded$lengths)
    starts <- c(1L, head(ends, -1L) + 1L)
    candidates <- which(
      seq_along(encoded$lengths) > 1L &
        seq_along(encoded$lengths) < length(encoded$lengths) &
        encoded$lengths <= max_records
    )
    fixed <- FALSE
    for (candidate in candidates) {
      left <- encoded$values[[candidate - 1L]]
      right <- encoded$values[[candidate + 1L]]
      if (
        is.finite(left) && is.finite(right) &&
          abs(left - right) <= tolerance
      ) {
        rows <- starts[[candidate]]:ends[[candidate]]
        repaired[rows] <- left
        changed[rows] <- TRUE
        fixed <- TRUE
      }
    }
    if (!fixed) break
  }
  list(values = repaired, repaired = changed)
}

safe_mean <- function(values) {
  values <- values[is.finite(values)]
  if (length(values) == 0L) NA_real_ else mean(values)
}

safe_sd <- function(values) {
  values <- values[is.finite(values)]
  if (length(values) < 2L) NA_real_ else stats::sd(values)
}

safe_slope <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 2L || length(unique(x[keep])) < 2L) return(NA_real_)
  unname(stats::coef(stats::lm(y[keep] ~ x[keep]))[[2]])
}

empty_step_summary <- function() {
  data.frame(
    run_id = character(), step_id = integer(), setpoint = numeric(),
    step_start = as.POSIXct(character()), step_end = as.POSIXct(character()),
    window_start = as.POSIXct(character()), window_end = as.POSIXct(character()),
    n_window = integer(), expected_n = integer(), coverage = numeric(),
    A_mean = numeric(), A_sd = numeric(), A_slope = numeric(),
    Tleaf_mean = numeric(), Tleaf_sd = numeric(),
    PPFD_mean = numeric(), PPFD_sd = numeric(),
    window_complete = logical(), include_model = logical(),
    warning = character(), stringsAsFactors = FALSE
  )
}

extract_light_steps <- function(
    parsed,
    run_id = "run",
    window_minutes = 3,
    max_transient_records = 2L,
    minimum_coverage = 0.8) {
  required <- c("Datetime", "White x T", "PARtop", "A", "Tleaf")
  missing <- setdiff(required, names(parsed$data))
  if (length(missing) > 0L) {
    return(list(
      summary = empty_step_summary(),
      raw = parsed$data[0, , drop = FALSE],
      issues = sprintf("Missing extraction field(s): %s.", paste(missing, collapse = ", "))
    ))
  }

  data <- parsed$data
  data$.source_row <- seq_len(nrow(data))
  data <- data[!is.na(data$Datetime), , drop = FALSE]
  data <- data[order(data$Datetime, data$.source_row), , drop = FALSE]
  if (nrow(data) == 0L) {
    return(list(
      summary = empty_step_summary(), raw = data,
      issues = "No valid timestamped rows were available for extraction."
    ))
  }

  transient <- repair_transient_setpoints(
    data[["White x T"]],
    max_records = max_transient_records
  )
  data$.light_setpoint <- transient$values
  data$.setpoint_repaired <- transient$repaired
  new_step <- c(
    TRUE,
    is.na(data$.light_setpoint[-1L]) |
      is.na(data$.light_setpoint[-nrow(data)]) |
      abs(data$.light_setpoint[-1L] - data$.light_setpoint[-nrow(data)]) > 1e-8
  )
  data$.step_id <- cumsum(new_step)
  data$.elapsed_minutes <- as.numeric(difftime(
    data$Datetime, min(data$Datetime), units = "mins"
  ))
  data$.in_extraction_window <- FALSE

  cadence <- stats::median(
    as.numeric(diff(data$Datetime), units = "secs"),
    na.rm = TRUE
  )
  if (!is.finite(cadence) || cadence <= 0) cadence <- 1
  window_seconds <- window_minutes * 60
  expected_n <- max(1L, floor(window_seconds / cadence) + 1L)
  summaries <- vector("list", max(data$.step_id))

  for (step in sort(unique(data$.step_id))) {
    rows <- which(data$.step_id == step)
    step_end <- max(data$Datetime[rows])
    step_start <- min(data$Datetime[rows])
    window_start <- step_end - window_seconds
    window_rows <- rows[data$Datetime[rows] >= window_start]
    data$.in_extraction_window[window_rows] <- TRUE

    duration <- if (length(window_rows) > 1L) {
      as.numeric(difftime(
        max(data$Datetime[window_rows]), min(data$Datetime[window_rows]),
        units = "secs"
      ))
    } else 0
    time_coverage <- min(1, duration / window_seconds)
    count_coverage <- min(1, length(window_rows) / expected_n)
    coverage <- min(time_coverage, count_coverage)
    complete <- is.finite(coverage) && coverage >= minimum_coverage
    warning <- character()
    if (!complete) warning <- c(warning, "Incomplete final-three-minute window")
    if (any(data$.setpoint_repaired[rows])) {
      warning <- c(warning, sprintf(
        "%d transient setpoint record(s) repaired",
        sum(data$.setpoint_repaired[rows])
      ))
    }
    if (any(!is.finite(data$A[window_rows]))) warning <- c(warning, "Missing A values")
    if (any(!is.finite(data$Tleaf[window_rows]))) warning <- c(warning, "Missing Tleaf values")
    if (any(!is.finite(data$PARtop[window_rows]))) warning <- c(warning, "Missing PPFD values")

    elapsed_window <- as.numeric(difftime(
      data$Datetime[window_rows], min(data$Datetime[window_rows]), units = "mins"
    ))
    summaries[[step]] <- data.frame(
      run_id = run_id,
      step_id = step,
      setpoint = safe_mean(data$.light_setpoint[rows]),
      step_start = step_start,
      step_end = step_end,
      window_start = max(step_start, window_start),
      window_end = step_end,
      n_window = length(window_rows),
      expected_n = expected_n,
      coverage = coverage,
      A_mean = safe_mean(data$A[window_rows]),
      A_sd = safe_sd(data$A[window_rows]),
      A_slope = safe_slope(elapsed_window, data$A[window_rows]),
      Tleaf_mean = safe_mean(data$Tleaf[window_rows]),
      Tleaf_sd = safe_sd(data$Tleaf[window_rows]),
      PPFD_mean = safe_mean(data$PARtop[window_rows]),
      PPFD_sd = safe_sd(data$PARtop[window_rows]),
      window_complete = complete,
      include_model = complete &&
        is.finite(safe_mean(data$A[window_rows])) &&
        is.finite(safe_mean(data$Tleaf[window_rows])) &&
        is.finite(safe_mean(data$PARtop[window_rows])),
      warning = paste(warning, collapse = "; "),
      stringsAsFactors = FALSE
    )
  }

  issues <- character()
  if (any(transient$repaired)) {
    issues <- c(issues, sprintf(
      "Repaired %d isolated light-setpoint record(s).",
      sum(transient$repaired)
    ))
  }
  result <- do.call(rbind, summaries)
  rownames(result) <- NULL
  list(summary = result, raw = data, issues = issues)
}

step_cache_key <- function(record, window_minutes = 3, max_transient_records = 2L) {
  paste(
    record$id[[1]], record$modified_iso[[1]], window_minutes,
    max_transient_records, sep = "::"
  )
}

extract_cached_light_steps <- function(
    record,
    parsed,
    window_minutes = 3,
    max_transient_records = 2L) {
  key <- step_cache_key(record, window_minutes, max_transient_records)
  if (exists(key, envir = .walz_step_cache, inherits = FALSE)) {
    return(get(key, envir = .walz_step_cache, inherits = FALSE))
  }
  value <- extract_light_steps(
    parsed,
    run_id = measurement_run_id(record$name[[1]]),
    window_minutes = window_minutes,
    max_transient_records = max_transient_records
  )
  assign(key, value, envir = .walz_step_cache)
  value
}

get_cached_light_steps <- function(
    record,
    window_minutes = 3,
    max_transient_records = 2L) {
  key <- step_cache_key(record, window_minutes, max_transient_records)
  if (!exists(key, envir = .walz_step_cache, inherits = FALSE)) return(NULL)
  get(key, envir = .walz_step_cache, inherits = FALSE)
}

cache_light_steps <- function(
    record,
    value,
    window_minutes = 3,
    max_transient_records = 2L) {
  assign(
    step_cache_key(record, window_minutes, max_transient_records),
    value,
    envir = .walz_step_cache
  )
  invisible(value)
}

clear_step_cache <- function() {
  remove(list = ls(envir = .walz_step_cache), envir = .walz_step_cache)
  invisible(TRUE)
}
