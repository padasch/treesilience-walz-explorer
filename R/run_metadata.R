WALZ_METADATA_COLUMN_LABELS <- c(
  timestamp = "Run ID",
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

run_line_types <- function(count) {
  if (count <= 0L) {
    return(character())
  }
  rep(c("solid", "dashed", "dotdash", "longdash", "twodash"), length.out = count)
}

hex_to_rgba <- function(colour, alpha = 0.1) {
  rgb <- grDevices::col2rgb(colour)
  sprintf(
    "rgba(%d, %d, %d, %.3f)",
    rgb[[1]], rgb[[2]], rgb[[3]], alpha
  )
}
