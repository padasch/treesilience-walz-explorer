WALZ_DEFAULT_DRIVE_FOLDER_ID <- "1wC9zXLEWQe4z7jBxfBfPRiVBuPJiF8vE"
WALZ_DEFAULT_MEASUREMENTS_FOLDER_ID <- "1Um6Ppu0wRzedxbCPUw2M_4htxPnWaRJR"
WALZ_DEFAULT_PROTOCOLS_FOLDER_ID <- "1jNNWNrwo05R1b417wujYuH6WxrZw4FVW"
WALZ_DEFAULT_METADATA_SHEET_ID <- "1BlUdEIKP-iEJICpzTF8NQG4nyEVBv_7Vgywc7KQqAX0"
WALZ_DEFAULT_METADATA_SHEET_NAME <- "Walz Measurement Metadata"
WALZ_TIMEZONE <- "Europe/Zurich"
WALZ_ENABLE_DEW_POINT_TAB <- FALSE
WALZ_RESPONSE_VARIABLES <- c("A", "GH2O", "E")
WALZ_PHYSIOLOGICAL_CONSTANTS <- "Area"
WALZ_PLOT_VARIABLES <- c(
  "A", "GH2O", "Tcuv", "Tamb", "VPD", "rh", "ca", "ci", "White x T",
  "PARtop"
)

WALZ_VARIABLE_LABELS <- c(
  A = "Net CO2",
  GH2O = "GH2O",
  E = "E",
  Tleaf = "Leaf temperature",
  Tcuv = "Cuvette temperature",
  Tamb = "Ambient temperature",
  VPD = "Vapour pressure deficit",
  rh = "Relative humidity",
  ca = "Ambient CO2",
  ci = "Intercellular CO2",
  `White x T` = "Light intensity",
  PARtop = "PARtop",
  Area = "Area"
)

walz_config <- function() {
  workers <- suppressWarnings(as.integer(Sys.getenv(
    "WALZ_BACKGROUND_WORKERS",
    unset = "2"
  )))
  if (is.na(workers) || workers < 2L) {
    workers <- 2L
  }

  list(
    drive_folder_id = Sys.getenv(
      "WALZ_DRIVE_FOLDER_ID",
      unset = WALZ_DEFAULT_DRIVE_FOLDER_ID
    ),
    measurements_folder_id = Sys.getenv(
      "WALZ_MEASUREMENTS_FOLDER_ID",
      unset = WALZ_DEFAULT_MEASUREMENTS_FOLDER_ID
    ),
    protocols_folder_id = Sys.getenv(
      "WALZ_PROTOCOLS_FOLDER_ID",
      unset = WALZ_DEFAULT_PROTOCOLS_FOLDER_ID
    ),
    api_key = Sys.getenv("GOOGLE_DRIVE_API_KEY", unset = ""),
    metadata_sheet_id = Sys.getenv(
      "WALZ_METADATA_SHEET_ID",
      unset = WALZ_DEFAULT_METADATA_SHEET_ID
    ),
    metadata_sheet_name = Sys.getenv(
      "WALZ_METADATA_SHEET_NAME",
      unset = WALZ_DEFAULT_METADATA_SHEET_NAME
    ),
    review_access_code = Sys.getenv("WALZ_REVIEW_ACCESS_CODE", unset = ""),
    service_account_json_b64 = Sys.getenv(
      "WALZ_GOOGLE_SERVICE_ACCOUNT_JSON_B64",
      unset = ""
    ),
    background_workers = workers,
    source_cache_ttl_seconds = 60,
    enable_dew_point_tab = identical(
      tolower(Sys.getenv(
        "WALZ_ENABLE_DEW_POINT_TAB",
        unset = as.character(WALZ_ENABLE_DEW_POINT_TAB)
      )),
      "true"
    ),
    timezone = WALZ_TIMEZONE,
    plot_variables = WALZ_PLOT_VARIABLES
  )
}

configure_drive_access <- function(api_key = "") {
  options(googledrive_quiet = TRUE)

  if (nzchar(api_key)) {
    googledrive::drive_auth_configure(api_key = api_key)
  }

  googledrive::drive_deauth()
  invisible(TRUE)
}
