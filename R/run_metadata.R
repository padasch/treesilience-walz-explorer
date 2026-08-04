WALZ_METADATA_COLUMN_LABELS <- c(
  timestamp = "Run ID",
  .display_date = "Date",
  `TREE species` = "Tree species",
  `plant id` = "Plant ID",
  `WALZ walz number` = "WALZ number",
  `walz cuvette temp` = "WALZ cuvette temperature",
  `walz h2o input` = "WALZ H2O input",
  `walz co2 input` = "WALZ CO2 input",
  `leaf area` = "Leaf area",
  `XIBOX xibox temp` = "XiBox temperature",
  `xibox light` = "XiBox light",
  `xibox humidity` = "XiBox humidity",
  `EXTRA protocol description (e.g., light intensity comparison; updated light response with less steps and longer equilibration)` =
    "Protocol description",
  `comments (e.g., changed silica beads midway causing bumps)` = "Comments",
  `quality assessment` = "Quality assessment"
)

WALZ_QUALITY_COLUMN <- "quality assessment"
WALZ_QUALITY_SOURCE_LEVELS <- c("good", "medium", "bad")
WALZ_QUALITY_LEVELS <- c(WALZ_QUALITY_SOURCE_LEVELS, "unassessed")
WALZ_DARK2 <- c(
  "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
  "#66A61E", "#E6AB02", "#A6761D", "#666666"
)

.walz_metadata_cache <- new.env(parent = emptyenv())

metadata_sheet_url <- function(sheet_id, sheet_name) {
  sprintf(
    "https://docs.google.com/spreadsheets/d/%s/gviz/tq?tqx=out:csv&sheet=%s",
    utils::URLencode(sheet_id, reserved = TRUE),
    utils::URLencode(sheet_name, reserved = TRUE)
  )
}

normalize_run_id <- function(value) {
  value <- trimws(as.character(value))
  value[is.na(value)] <- ""
  tolower(value)
}

measurement_run_id <- function(filename) {
  tools::file_path_sans_ext(basename(filename))
}

clean_run_metadata <- function(metadata) {
  if (!is.data.frame(metadata)) {
    stop("The run metadata source did not return a table.", call. = FALSE)
  }

  names(metadata) <- trimws(names(metadata))
  # Keep named columns even when every value is blank. In particular, the
  # quality-assessment column intentionally starts empty. Only unnamed export
  # padding columns are discarded.
  keep_columns <- nzchar(names(metadata))
  metadata <- metadata[, keep_columns, drop = FALSE]

  if (!"timestamp" %in% names(metadata)) {
    stop(
      "The run metadata sheet must contain a 'timestamp' column with run IDs.",
      call. = FALSE
    )
  }

  metadata[] <- lapply(metadata, function(column) {
    values <- trimws(as.character(column))
    values[is.na(values)] <- ""
    values
  })
  metadata <- metadata[nzchar(metadata$timestamp), , drop = FALSE]
  rownames(metadata) <- NULL

  if (!WALZ_QUALITY_COLUMN %in% names(metadata)) {
    metadata[[WALZ_QUALITY_COLUMN]] <- ""
  }

  metadata$.run_id <- normalize_run_id(metadata$timestamp)
  metadata
}

load_public_run_metadata <- function(
    sheet_id,
    sheet_name,
    downloader = utils::download.file) {
  destination <- tempfile(fileext = ".csv")
  on.exit(unlink(destination), add = TRUE)

  status <- downloader(
    metadata_sheet_url(sheet_id, sheet_name),
    destination,
    mode = "wb",
    quiet = TRUE
  )
  if (!identical(status, 0L) || !file.exists(destination)) {
    stop("The public run metadata sheet could not be downloaded.", call. = FALSE)
  }
  size <- file.info(destination)$size[[1]]
  if (is.na(size) || size == 0) {
    stop("The public run metadata sheet returned an empty file.", call. = FALSE)
  }

  metadata <- tryCatch(
    utils::read.csv(
      destination,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = character(),
      fileEncoding = "UTF-8-BOM"
    ),
    error = function(error) {
      stop(
        paste("The public run metadata sheet could not be parsed:", conditionMessage(error)),
        call. = FALSE
      )
    }
  )
  clean_run_metadata(metadata)
}

load_cached_run_metadata <- function(
    sheet_id,
    sheet_name,
    force = FALSE,
    ttl_seconds = 60,
    loader = load_public_run_metadata) {
  key <- paste(sheet_id, sheet_name, sep = "::")
  now <- Sys.time()
  if (!isTRUE(force) && exists(key, envir = .walz_metadata_cache, inherits = FALSE)) {
    cached <- get(key, envir = .walz_metadata_cache, inherits = FALSE)
    age <- as.numeric(difftime(now, cached$loaded_at, units = "secs"))
    if (is.finite(age) && age <= ttl_seconds) {
      return(cached$value)
    }
  }

  value <- loader(sheet_id, sheet_name)
  assign(key, list(value = value, loaded_at = now), envir = .walz_metadata_cache)
  value
}

clear_metadata_cache <- function() {
  remove(list = ls(envir = .walz_metadata_cache), envir = .walz_metadata_cache)
  invisible(TRUE)
}

canonical_quality <- function(value) {
  value <- tolower(trimws(as.character(value)))
  missing <- is.na(value) | !nzchar(value)
  value[missing] <- "unassessed"
  invalid <- !missing & !value %in% WALZ_QUALITY_SOURCE_LEVELS
  value[invalid] <- "unassessed"
  attr(value, "invalid") <- invalid
  value
}

metadata_quality <- function(metadata, measurement_name) {
  matches <- match_run_metadata(metadata, measurement_name)
  if (nrow(matches) != 1L || !WALZ_QUALITY_COLUMN %in% names(matches)) {
    return("unassessed")
  }
  quality <- canonical_quality(matches[[WALZ_QUALITY_COLUMN]][[1]])
  attributes(quality) <- NULL
  quality[[1]]
}

match_run_metadata <- function(metadata, measurement_name) {
  if (is.null(metadata) || !is.data.frame(metadata) || nrow(metadata) == 0L) {
    if (is.data.frame(metadata)) {
      return(metadata[0, , drop = FALSE])
    }
    return(data.frame())
  }

  run_id <- normalize_run_id(measurement_run_id(measurement_name))
  metadata[metadata$.run_id == run_id, , drop = FALSE]
}

sort_run_metadata_newest <- function(metadata) {
  if (
    is.null(metadata) ||
      !is.data.frame(metadata) ||
      nrow(metadata) == 0L ||
      !"timestamp" %in% names(metadata)
  ) {
    return(metadata)
  }

  timestamp_keys <- sub(
    "^\\s*([0-9]{8})[_-]?([0-9]{4}).*$",
    "\\1\\2",
    as.character(metadata$timestamp),
    perl = TRUE
  )
  timestamp_keys <- suppressWarnings(as.numeric(timestamp_keys))
  sorted_rows <- order(
    timestamp_keys,
    decreasing = TRUE,
    na.last = TRUE,
    method = "radix"
  )

  sorted <- metadata[sorted_rows, , drop = FALSE]
  rownames(sorted) <- NULL
  sorted
}

run_datetime_from_id <- function(run_id, timezone = WALZ_TIMEZONE) {
  run_id <- trimws(as.character(run_id))
  pattern <- "^([0-9]{4})([0-9]{2})([0-9]{2})_([0-9]{2})([0-9]{2}).*$"
  matched <- grepl(pattern, run_id, perl = TRUE)
  datetime_text <- rep(NA_character_, length(run_id))
  datetime_text[matched] <- sub(
    pattern,
    "\\1\\2\\3 \\4:\\5",
    run_id[matched],
    perl = TRUE
  )

  parsed <- as.POSIXct(
    strptime(datetime_text, format = "%Y%m%d %H:%M", tz = timezone)
  )
  valid <- !is.na(parsed)
  round_trip <- rep(NA_character_, length(parsed))
  round_trip[valid] <- format(parsed[valid], "%Y%m%d %H:%M", tz = timezone)
  parsed[valid & round_trip != datetime_text] <- as.POSIXct(NA, tz = timezone)
  parsed
}

relative_run_date <- function(
    run_id,
    reference_time = Sys.time(),
    timezone = WALZ_TIMEZONE) {
  run_time <- run_datetime_from_id(run_id, timezone)
  if (length(run_time) == 0L) {
    return(character())
  }

  weekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  months <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
  valid <- !is.na(run_time)
  labels <- rep(NA_character_, length(run_time))
  if (!any(valid)) {
    return(labels)
  }

  run_dates <- as.Date(run_time, tz = timezone)
  reference_date <- as.Date(reference_time, tz = timezone)
  day_difference <- as.integer(reference_date - run_dates)
  weekday_index <- as.integer(format(run_time, "%u", tz = timezone))
  month_index <- as.integer(format(run_time, "%m", tz = timezone))
  absolute_label <- sprintf(
    "%s %d %s, %s",
    weekdays[weekday_index],
    as.integer(format(run_time, "%d", tz = timezone)),
    months[month_index],
    format(run_time, "%H:%M", tz = timezone)
  )

  relative_label <- ifelse(
    day_difference == 0L,
    "Today",
    ifelse(
      day_difference == 1L,
      "Yesterday",
      ifelse(
        day_difference == 2L,
        "Two days ago",
        ifelse(
          day_difference > 2L,
          sprintf("%d days ago", day_difference),
          ifelse(
            day_difference == -1L,
            "Tomorrow",
            ifelse(
              day_difference == -2L,
              "In two days",
              sprintf("In %d days", abs(day_difference))
            )
          )
        )
      )
    )
  )
  labels[valid] <- sprintf(
    "%s (%s)",
    relative_label[valid],
    absolute_label[valid]
  )
  labels
}

add_run_date_display <- function(
    metadata,
    reference_time = Sys.time(),
    timezone = WALZ_TIMEZONE) {
  if (is.null(metadata) || !is.data.frame(metadata) || !"timestamp" %in% names(metadata)) {
    return(metadata)
  }

  metadata$.display_date <- relative_run_date(
    metadata$timestamp,
    reference_time = reference_time,
    timezone = timezone
  )
  columns_without_date <- setdiff(names(metadata), ".display_date")
  timestamp_position <- match("timestamp", columns_without_date)
  display_columns <- append(
    columns_without_date,
    ".display_date",
    after = timestamp_position
  )
  metadata[, display_columns, drop = FALSE]
}

metadata_column_label <- function(column) {
  label <- unname(WALZ_METADATA_COLUMN_LABELS[column])
  if (length(label) == 0L || is.na(label) || !nzchar(label)) {
    column
  } else {
    label
  }
}

run_palette <- function(count) {
  if (count <= 0L) {
    return(character())
  }
  if (count <= length(WALZ_DARK2)) {
    return(WALZ_DARK2[seq_len(count)])
  }
  grDevices::colorRampPalette(WALZ_DARK2)(count)
}

stable_run_colour <- function(run_id) {
  run_id <- normalize_run_id(run_id)
  unname(vapply(run_id, function(value) {
    bytes <- utf8ToInt(value)
    if (length(bytes) == 0L) {
      return(WALZ_DARK2[[1]])
    }
    position <- 1L + (sum(bytes * seq_along(bytes) * 17L) %% length(WALZ_DARK2))
    WALZ_DARK2[[position]]
  }, character(1)))
}

stable_run_colours <- function(run_ids) {
  stats::setNames(stable_run_colour(run_ids), run_ids)
}

hex_to_rgba <- function(colour, alpha = 0.1) {
  rgb <- grDevices::col2rgb(colour)
  sprintf(
    "rgba(%d, %d, %d, %.3f)",
    rgb[[1]], rgb[[2]], rgb[[3]], alpha
  )
}
