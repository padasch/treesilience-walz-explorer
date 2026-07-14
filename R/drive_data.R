.walz_remote_cache <- new.env(parent = emptyenv())

drive_metadata_table <- function(files) {
  if (nrow(files) == 0L) {
    return(data.frame(
      id = character(),
      name = character(),
      modified_time = as.POSIXct(character(), tz = "UTC"),
      modified_iso = character(),
      mime_type = character(),
      size = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  get_detail <- function(item, key, default = NA_character_) {
    value <- item[[key]]
    if (is.null(value) || length(value) == 0L) default else value[[1]]
  }

  modified_iso <- vapply(
    files$drive_resource,
    get_detail,
    character(1),
    key = "modifiedTime"
  )

  data.frame(
    id = files$id,
    name = files$name,
    modified_time = as.POSIXct(
      modified_iso,
      format = "%Y-%m-%dT%H:%M:%OSZ",
      tz = "UTC"
    ),
    modified_iso = modified_iso,
    mime_type = vapply(
      files$drive_resource,
      get_detail,
      character(1),
      key = "mimeType"
    ),
    size = suppressWarnings(as.numeric(vapply(
      files$drive_resource,
      get_detail,
      character(1),
      key = "size"
    ))),
    stringsAsFactors = FALSE
  )
}

find_named_drive_folder <- function(root_files, folder_name) {
  folder_mime <- "application/vnd.google-apps.folder"
  matches <- which(
    root_files$name == folder_name &
      vapply(
        root_files$drive_resource,
        function(item) identical(item$mimeType, folder_mime),
        logical(1)
      )
  )

  if (length(matches) != 1L) {
    stop(
      sprintf(
        "Expected exactly one direct child folder named '%s'; found %d.",
        folder_name,
        length(matches)
      ),
      call. = FALSE
    )
  }

  root_files$id[[matches]]
}

list_walz_drive <- function(root_id) {
  root_files <- googledrive::drive_ls(googledrive::as_id(root_id))
  measurements_id <- find_named_drive_folder(root_files, "measurements")
  protocols_id <- find_named_drive_folder(root_files, "protocols")

  measurement_files <- googledrive::drive_ls(googledrive::as_id(measurements_id))
  protocol_files <- googledrive::drive_ls(googledrive::as_id(protocols_id))

  measurements <- drive_metadata_table(measurement_files)
  protocols <- drive_metadata_table(protocol_files)

  measurements <- measurements[
    grepl("\\.csv$", measurements$name, ignore.case = TRUE),
    ,
    drop = FALSE
  ]
  protocols <- protocols[
    grepl("\\.txt$", protocols$name, ignore.case = TRUE),
    ,
    drop = FALSE
  ]

  if (nrow(measurements) > 0L) {
    measurements <- measurements[
      order(measurements$modified_time, measurements$name, decreasing = TRUE),
      ,
      drop = FALSE
    ]
    rownames(measurements) <- NULL
  }

  if (nrow(protocols) > 0L) {
    protocols <- protocols[order(protocols$name), , drop = FALSE]
    rownames(protocols) <- NULL
  }

  list(
    measurements = measurements,
    protocols = protocols,
    refreshed_at = Sys.time(),
    root_id = root_id,
    measurements_id = measurements_id,
    protocols_id = protocols_id
  )
}

resolve_selected_measurement <- function(index, selected_id) {
  if (
    is.null(index) ||
      !is.data.frame(index$measurements) ||
      nrow(index$measurements) == 0L ||
      is.null(selected_id) ||
      length(selected_id) != 1L ||
      is.na(selected_id) ||
      !nzchar(selected_id)
  ) {
    return(NULL)
  }

  record <- index$measurements[
    index$measurements$id == selected_id,
    ,
    drop = FALSE
  ]
  if (nrow(record) == 1L) record else NULL
}

remote_cache_key <- function(kind, record) {
  paste(kind, record$id[[1]], record$modified_iso[[1]], sep = "::")
}

with_remote_cache <- function(kind, record, loader) {
  key <- remote_cache_key(kind, record)
  if (exists(key, envir = .walz_remote_cache, inherits = FALSE)) {
    return(get(key, envir = .walz_remote_cache, inherits = FALSE))
  }

  value <- loader()
  assign(key, value, envir = .walz_remote_cache)
  value
}

download_drive_record <- function(
    record,
    destination,
    drive_downloader = googledrive::drive_download,
    direct_downloader = utils::download.file) {
  drive_error <- tryCatch(
    {
      drive_downloader(
        googledrive::as_id(record$id[[1]]),
        path = destination,
        overwrite = TRUE
      )
      NULL
    },
    error = function(error) error
  )
  if (is.null(drive_error)) {
    return(invisible(destination))
  }

  direct_url <- sprintf(
    "https://drive.google.com/uc?export=download&id=%s",
    utils::URLencode(record$id[[1]], reserved = TRUE)
  )
  direct_error <- tryCatch(
    {
      status <- direct_downloader(
        direct_url,
        destination,
        mode = "wb",
        quiet = TRUE
      )
      if (!identical(status, 0L) || !file.exists(destination)) {
        stop("The public download returned no file.", call. = FALSE)
      }
      size <- file.info(destination)$size[[1]]
      if (is.na(size) || size == 0) {
        stop("The public download returned an empty file.", call. = FALSE)
      }
      NULL
    },
    error = function(error) error
  )
  if (is.null(direct_error)) {
    return(invisible(destination))
  }

  stop(
    sprintf(
      paste0(
        "The file could not be downloaded through googledrive (%s) ",
        "or Google's public file URL (%s)."
      ),
      conditionMessage(drive_error),
      conditionMessage(direct_error)
    ),
    call. = FALSE
  )
}

load_remote_measurement <- function(record) {
  with_remote_cache("measurement", record, function() {
    extension <- tools::file_ext(record$name[[1]])
    destination <- tempfile(fileext = paste0(".", extension))
    on.exit(unlink(destination), add = TRUE)
    download_drive_record(record, destination)
    read_walz_csv(destination)
  })
}

load_remote_protocol <- function(record) {
  with_remote_cache("protocol", record, function() {
    destination <- tempfile(fileext = ".txt")
    on.exit(unlink(destination), add = TRUE)
    download_drive_record(record, destination)
    paste(
      readLines(destination, encoding = "latin1", warn = FALSE),
      collapse = "\n"
    )
  })
}

clear_remote_cache <- function() {
  remove(list = ls(envir = .walz_remote_cache), envir = .walz_remote_cache)
  invisible(TRUE)
}
