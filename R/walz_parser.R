split_semicolon_line <- function(line) {
  trimws(strsplit(line, ";", fixed = TRUE)[[1]])
}

read_walz_csv <- function(
    path,
    timezone = WALZ_TIMEZONE,
    expected_variables = WALZ_PLOT_VARIABLES) {
  if (!file.exists(path)) {
    stop("The downloaded measurement file does not exist.", call. = FALSE)
  }

  preview_lines <- readLines(
    path,
    n = 3,
    encoding = "latin1",
    warn = FALSE
  )

  if (length(preview_lines) < 2L) {
    stop(
      "The measurement file must contain a variable row, a units row, and data.",
      call. = FALSE
    )
  }

  if (length(preview_lines) < 3L || !nzchar(trimws(preview_lines[[3]]))) {
    stop("The measurement file contains no data rows.", call. = FALSE)
  }

  first_lines <- preview_lines[1:2]

  variable_names <- split_semicolon_line(first_lines[[1]])
  unit_values <- split_semicolon_line(first_lines[[2]])

  if (length(variable_names) != length(unit_values)) {
    stop(
      "The variable and units rows contain different numbers of columns.",
      call. = FALSE
    )
  }

  if (any(!nzchar(variable_names))) {
    stop("The variable row contains an empty column name.", call. = FALSE)
  }

  variable_names <- make.unique(variable_names, sep = "_")
  names(unit_values) <- variable_names

  data <- utils::read.table(
    path,
    sep = ";",
    header = FALSE,
    skip = 2,
    quote = "\"",
    comment.char = "",
    fill = TRUE,
    fileEncoding = "latin1",
    stringsAsFactors = FALSE,
    colClasses = "character",
    na.strings = c("", "NA", "NaN")
  )

  if (nrow(data) == 0L) {
    stop("The measurement file contains no data rows.", call. = FALSE)
  }

  if (ncol(data) != length(variable_names)) {
    stop(
      sprintf(
        "The data contain %d columns, but the header defines %d.",
        ncol(data),
        length(variable_names)
      ),
      call. = FALSE
    )
  }

  names(data) <- variable_names

  required_clock_columns <- c("Date", "Time")
  missing_clock_columns <- setdiff(required_clock_columns, names(data))
  if (length(missing_clock_columns) > 0L) {
    stop(
      sprintf(
        "The measurement file is missing clock column(s): %s.",
        paste(missing_clock_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  character_columns <- intersect(
    c("Date", "Time", "Code", "Object", "Status", "Comment"),
    names(data)
  )
  numeric_columns <- setdiff(names(data), character_columns)
  for (column in numeric_columns) {
    data[[column]] <- suppressWarnings(as.numeric(trimws(data[[column]])))
  }

  date_time_text <- paste(trimws(data$Date), trimws(data$Time))
  data$Datetime <- as.POSIXct(
    date_time_text,
    format = "%Y-%m-%d %H:%M:%S",
    tz = timezone
  )
  fallback_rows <- which(is.na(data$Datetime))
  if (length(fallback_rows) > 0L) {
    data$Datetime[fallback_rows] <- as.POSIXct(
      date_time_text[fallback_rows],
      format = "%d.%m.%Y %H:%M:%S",
      tz = timezone
    )
  }

  if (all(is.na(data$Datetime))) {
    stop(
      paste0(
        "No valid timestamps were found; expected dates like 2026-07-13 ",
        "and times like 10:23:00."
      ),
      call. = FALSE
    )
  }

  issues <- character()
  invalid_clock_count <- sum(is.na(data$Datetime))
  if (invalid_clock_count > 0L) {
    issues <- c(
      issues,
      sprintf("%d row(s) have invalid timestamps and are omitted from plots.", invalid_clock_count)
    )
  }

  missing_variables <- setdiff(expected_variables, names(data))
  if (length(missing_variables) > 0L) {
    issues <- c(
      issues,
      sprintf(
        "Missing plotted variable(s): %s.",
        paste(missing_variables, collapse = ", ")
      )
    )
  }

  list(
    data = data,
    units = unit_values,
    issues = issues,
    missing_variables = missing_variables,
    row_count = nrow(data),
    column_count = length(variable_names)
  )
}
