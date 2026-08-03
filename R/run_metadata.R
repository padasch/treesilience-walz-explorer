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
  `comments (e.g., changed silica beads midway causing bumps)` = "Comments"
)

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
  keep_columns <- vapply(metadata, function(column) {
    values <- trimws(as.character(column))
    any(!is.na(values) & nzchar(values))
  }, logical(1))
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
  base <- c(
    "#28754D", "#BD5D38", "#426A8C", "#8A5B9E", "#C6922D",
    "#4F8B8B", "#B4476B", "#6F6B3F", "#2384A1", "#8B5A2B"
  )
  if (count <= length(base)) {
    return(base[seq_len(count)])
  }
  extras <- grDevices::hcl.colors(count + length(base), palette = "Dark 3")
  extras <- extras[!tolower(extras) %in% tolower(base)]
  c(base, extras[seq_len(count - length(base))])
}

hex_to_rgba <- function(colour, alpha = 0.1) {
  rgb <- grDevices::col2rgb(colour)
  sprintf(
    "rgba(%d, %d, %d, %.3f)",
    rgb[[1]], rgb[[2]], rgb[[3]], alpha
  )
}
