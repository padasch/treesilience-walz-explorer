.walz_remote_cache <- new.env(parent = emptyenv())
.walz_source_cache <- new.env(parent = emptyenv())

plain_drive_ids <- function(ids) {
  if (is.null(ids)) return(character())
  if (inherits(ids, "drive_id")) ids <- unclass(ids)
  as.character(ids)
}

drive_metadata_table <- function(files) {
  if (nrow(files) == 0L) {
    result <- data.frame(
      id = character(),
      name = character(),
      modified_time = as.POSIXct(character(), tz = "UTC"),
      modified_iso = character(),
      mime_type = character(),
      size = numeric(),
      stringsAsFactors = FALSE
    )
    result$drive_resource <- I(vector("list", 0L))
    return(result)
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

  result <- data.frame(
    # Keep googledrive's typed ID inside the package boundary only. A drive_id
    # is character-backed, but its vctrs class can fail to cast after the
    # records are serialized to a background worker.
    id = plain_drive_ids(files$id),
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
  result$drive_resource <- files$drive_resource
  result
}

source_cache_get <- function(key, ttl_seconds = 60) {
  if (!exists(key, envir = .walz_source_cache, inherits = FALSE)) {
    return(NULL)
  }
  cached <- get(key, envir = .walz_source_cache, inherits = FALSE)
  age <- as.numeric(difftime(Sys.time(), cached$loaded_at, units = "secs"))
  if (!is.finite(age) || age > ttl_seconds) {
    return(NULL)
  }
  cached$value
}

source_cache_set <- function(key, value) {
  assign(
    key,
    list(value = value, loaded_at = Sys.time()),
    envir = .walz_source_cache
  )
  value
}

sort_drive_files <- function(files, kind) {
  if (nrow(files) == 0L) {
    return(files)
  }
  if (identical(kind, "measurements")) {
    files <- files[
      order(files$modified_time, files$name, decreasing = TRUE),
      ,
      drop = FALSE
    ]
  } else {
    files <- files[order(files$name), , drop = FALSE]
  }
  rownames(files) <- NULL
  files
}

list_drive_folder_files <- function(folder_id, extension, kind) {
  files <- googledrive::drive_ls(googledrive::as_id(folder_id))
  files <- drive_metadata_table(files)
  files <- files[
    grepl(paste0("\\.", extension, "$"), files$name, ignore.case = TRUE),
    ,
    drop = FALSE
  ]
  sort_drive_files(files, kind)
}

discover_walz_folder_ids <- function(root_id) {
  root_files <- googledrive::drive_ls(googledrive::as_id(root_id))
  list(
    measurements_id = find_named_drive_folder(root_files, "measurements"),
    protocols_id = find_named_drive_folder(root_files, "protocols")
  )
}

list_measurement_drive <- function(
    folder_id,
    root_id = NULL,
    force = FALSE,
    ttl_seconds = 60) {
  key <- paste("measurements", folder_id, sep = "::")
  if (!isTRUE(force)) {
    cached <- source_cache_get(key, ttl_seconds)
    if (!is.null(cached)) return(cached)
  }

  resolved_id <- folder_id
  value <- tryCatch(
    list_drive_folder_files(resolved_id, "csv", "measurements"),
    error = function(error) {
      if (is.null(root_id) || !nzchar(root_id)) stop(error)
      resolved_id <<- discover_walz_folder_ids(root_id)$measurements_id
      list_drive_folder_files(resolved_id, "csv", "measurements")
    }
  )
  source_cache_set(key, list(
    measurements = value,
    refreshed_at = Sys.time(),
    measurements_id = resolved_id,
    root_id = root_id
  ))
}

list_protocol_drive <- function(
    folder_id,
    root_id = NULL,
    force = FALSE,
    ttl_seconds = 60) {
  key <- paste("protocols", folder_id, sep = "::")
  if (!isTRUE(force)) {
    cached <- source_cache_get(key, ttl_seconds)
    if (!is.null(cached)) return(cached)
  }

  resolved_id <- folder_id
  value <- tryCatch(
    list_drive_folder_files(resolved_id, "txt", "protocols"),
    error = function(error) {
      if (is.null(root_id) || !nzchar(root_id)) stop(error)
      resolved_id <<- discover_walz_folder_ids(root_id)$protocols_id
      list_drive_folder_files(resolved_id, "txt", "protocols")
    }
  )
  source_cache_set(key, list(
    protocols = value,
    refreshed_at = Sys.time(),
    protocols_id = resolved_id,
    root_id = root_id
  ))
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
  ids <- discover_walz_folder_ids(root_id)
  measurements <- list_drive_folder_files(
    ids$measurements_id, "csv", "measurements"
  )
  protocols <- list_drive_folder_files(ids$protocols_id, "txt", "protocols")

  list(
    measurements = measurements,
    protocols = protocols,
    refreshed_at = Sys.time(),
    root_id = root_id,
    measurements_id = ids$measurements_id,
    protocols_id = ids$protocols_id
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

resolve_selected_measurements <- function(index, selected_ids) {
  if (is.null(selected_ids) || length(selected_ids) == 0L) {
    return(NULL)
  }
  records <- lapply(
    selected_ids,
    function(selected_id) resolve_selected_measurement(index, selected_id)
  )
  records <- Filter(Negate(is.null), records)
  if (length(records) == 0L) {
    return(NULL)
  }
  result <- do.call(rbind, records)
  rownames(result) <- NULL
  result
}

remote_cache_key <- function(kind, record) {
  paste(kind, record$id[[1]], record$modified_iso[[1]], sep = "::")
}

record_as_dribble <- function(record) {
  if (!"drive_resource" %in% names(record) || length(record$drive_resource) != 1L) {
    return(googledrive::as_id(record$id[[1]]))
  }
  googledrive::as_dribble(record[, c("name", "id", "drive_resource"), drop = FALSE])
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

get_remote_cached <- function(kind, record) {
  key <- remote_cache_key(kind, record)
  if (!exists(key, envir = .walz_remote_cache, inherits = FALSE)) return(NULL)
  get(key, envir = .walz_remote_cache, inherits = FALSE)
}

download_drive_record <- function(
    record,
    destination,
    drive_downloader = googledrive::drive_download,
    direct_downloader = utils::download.file) {
  drive_error <- tryCatch(
    {
      drive_downloader(
        record_as_dribble(record),
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

cache_remote_value <- function(kind, record, value) {
  assign(remote_cache_key(kind, record), value, envir = .walz_remote_cache)
  invisible(value)
}

batch_load_remote_measurements <- function(records, progress = FALSE) {
  if (is.null(records) || nrow(records) == 0L) {
    return(list())
  }

  destination_dir <- tempfile("walz-batch-")
  dir.create(destination_dir, recursive = TRUE)
  on.exit(unlink(destination_dir, recursive = TRUE), add = TRUE)
  destinations <- file.path(
    destination_dir,
    sprintf("%03d-%s.csv", seq_len(nrow(records)), records$id)
  )
  urls <- sprintf(
    "https://drive.google.com/uc?export=download&id=%s",
    utils::URLencode(records$id, reserved = TRUE)
  )

  transfer <- tryCatch(
    curl::multi_download(urls, destinations, progress = progress),
    error = function(error) NULL
  )
  transfer_ok <- rep(FALSE, nrow(records))
  if (!is.null(transfer) && nrow(transfer) == nrow(records)) {
    transfer_ok <- isTRUE(transfer$success) | transfer$success
    transfer_ok[is.na(transfer_ok)] <- FALSE
  }

  lapply(seq_len(nrow(records)), function(index) {
    record <- records[index, , drop = FALSE]
    if ("size" %in% names(record) && is.finite(record$size[[1]]) && record$size[[1]] == 0) {
      return(list(
        id = record$id[[1]], modified_iso = record$modified_iso[[1]],
        name = record$name[[1]], value = NULL,
        error = "The Google Drive file is empty."
      ))
    }
    destination <- destinations[[index]]
    error <- NULL
    value <- NULL
    if (transfer_ok[[index]] && file.exists(destination)) {
      size <- file.info(destination)$size[[1]]
      if (is.na(size) || size == 0) transfer_ok[[index]] <- FALSE
    }
    if (!transfer_ok[[index]]) {
      error <- tryCatch(
        {
          download_drive_record(record, destination)
          NULL
        },
        error = function(error) error
      )
    }
    downloaded_in_batch <- is.null(error) && isTRUE(transfer_ok[[index]])
    if (is.null(error)) {
      value <- tryCatch(
        read_walz_csv(destination),
        error = function(parse_error) {
          error <<- parse_error
          NULL
        }
      )
    }
    # Google's direct endpoint can occasionally return a non-empty HTML page
    # with a successful status. Retry parse failures through the metadata-rich
    # googledrive record before declaring the file unusable.
    if (downloaded_in_batch && !is.null(error)) {
      retry_error <- tryCatch(
        {
          unlink(destination)
          download_drive_record(record, destination)
          value <- read_walz_csv(destination)
          NULL
        },
        error = function(retry_error) retry_error
      )
      error <- retry_error
    }
    list(
      id = record$id[[1]],
      modified_iso = record$modified_iso[[1]],
      name = record$name[[1]],
      value = value,
      error = if (is.null(error)) NULL else conditionMessage(error)
    )
  })
}

merge_measurement_batch_cache <- function(records, results) {
  for (result in results) {
    if (is.null(result$value)) next
    record <- records[
      records$id == result$id & records$modified_iso == result$modified_iso,
      ,
      drop = FALSE
    ]
    if (nrow(record) == 1L) {
      cache_remote_value("measurement", record, result$value)
    }
  }
  invisible(results)
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

clear_source_cache <- function() {
  remove(list = ls(envir = .walz_source_cache), envir = .walz_source_cache)
  invisible(TRUE)
}
