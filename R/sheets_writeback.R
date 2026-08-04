sheet_writeback_configured <- function(config) {
  nzchar(config$review_access_code) && nzchar(config$service_account_json_b64)
}

sheet_column_letter <- function(index) {
  if (!is.numeric(index) || length(index) != 1L || index < 1L) {
    stop("The Sheet column index is invalid.", call. = FALSE)
  }
  result <- character()
  index <- as.integer(index)
  while (index > 0L) {
    remainder <- (index - 1L) %% 26L
    result <- c(LETTERS[[remainder + 1L]], result)
    index <- (index - remainder - 1L) %/% 26L
  }
  paste(result, collapse = "")
}

authenticate_sheet_service_account <- function(encoded_json) {
  if (!nzchar(encoded_json)) {
    stop("The Google service-account secret is not configured.", call. = FALSE)
  }
  credentials_path <- tempfile(fileext = ".json")
  on.exit(unlink(credentials_path), add = TRUE)
  credentials <- tryCatch(
    base64enc::base64decode(encoded_json),
    error = function(error) {
      stop("The Google service-account secret is not valid base64.", call. = FALSE)
    }
  )
  writeBin(credentials, credentials_path)
  googlesheets4::gs4_auth(
    path = credentials_path,
    scopes = "https://www.googleapis.com/auth/spreadsheets"
  )
  invisible(TRUE)
}

default_sheet_client <- function(config) {
  authenticated <- FALSE
  ensure_auth <- function() {
    if (!authenticated) {
      authenticate_sheet_service_account(config$service_account_json_b64)
      authenticated <<- TRUE
    }
  }
  list(
    read = function() {
      ensure_auth()
      as.data.frame(googlesheets4::read_sheet(
        config$metadata_sheet_id,
        sheet = config$metadata_sheet_name,
        col_types = "c",
        na = "",
        .name_repair = "minimal"
      ), stringsAsFactors = FALSE, check.names = FALSE)
    },
    write = function(cell, value) {
      ensure_auth()
      googlesheets4::range_write(
        config$metadata_sheet_id,
        data = data.frame(value = value, check.names = FALSE),
        sheet = config$metadata_sheet_name,
        range = cell,
        col_names = FALSE,
        reformat = FALSE
      )
      invisible(TRUE)
    }
  )
}

write_quality_assessment <- function(
    run_id,
    new_quality,
    expected_quality,
    client) {
  allowed <- c("good", "medium", "bad", "")
  new_quality <- tolower(trimws(as.character(new_quality)[[1]]))
  if (!new_quality %in% allowed) {
    stop("Quality must be Good, Medium, Bad, or Clear.", call. = FALSE)
  }

  sheet <- client$read()
  names(sheet) <- trimws(names(sheet))
  required <- c("timestamp", WALZ_QUALITY_COLUMN)
  missing <- setdiff(required, names(sheet))
  if (length(missing) > 0L) {
    stop(sprintf(
      "The Sheet is missing required column(s): %s.",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  matches <- which(normalize_run_id(sheet$timestamp) == normalize_run_id(run_id))
  if (length(matches) != 1L) {
    stop(sprintf(
      "Write-back requires exactly one exact timestamp match; found %d.",
      length(matches)
    ), call. = FALSE)
  }

  current_raw <- trimws(as.character(sheet[[WALZ_QUALITY_COLUMN]][matches]))
  current <- unname(canonical_quality(current_raw))
  expected <- unname(canonical_quality(expected_quality))
  if (!identical(current, expected)) {
    stop(sprintf(
      "The quality value changed in Google Sheets from '%s' to '%s'. Refresh before writing.",
      expected, current
    ), call. = FALSE)
  }

  quality_column <- match(WALZ_QUALITY_COLUMN, names(sheet))
  cell <- sprintf("%s%d", sheet_column_letter(quality_column), matches + 1L)
  client$write(cell, new_quality)

  verified <- client$read()
  names(verified) <- trimws(names(verified))
  verified_matches <- which(
    normalize_run_id(verified$timestamp) == normalize_run_id(run_id)
  )
  if (length(verified_matches) != 1L) {
    stop("The written row could not be uniquely verified.", call. = FALSE)
  }
  verified_value <- trimws(as.character(
    verified[[WALZ_QUALITY_COLUMN]][verified_matches]
  ))
  if (!identical(verified_value, new_quality)) {
    stop("Google Sheets did not return the requested value after writing.", call. = FALSE)
  }

  list(
    run_id = run_id,
    quality = as.character(canonical_quality(new_quality)),
    raw_quality = new_quality,
    cell = cell,
    sheet = clean_run_metadata(verified)
  )
}
