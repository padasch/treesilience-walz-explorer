WALZ_DEFAULT_DRIVE_FOLDER_ID <- "1wC9zXLEWQe4z7jBxfBfPRiVBuPJiF8vE"
WALZ_TIMEZONE <- "Europe/Zurich"
WALZ_RESPONSE_VARIABLES <- c("A", "GH2O", "E")
WALZ_PHYSIOLOGICAL_CONSTANTS <- "Area"
WALZ_PLOT_VARIABLES <- c(
  "A", "GH2O", "Tcuv", "VPD", "rh", "ca", "ci", "White x T", "PARtop"
)

WALZ_VARIABLE_LABELS <- c(
  A = "Net CO2",
  GH2O = "GH2O",
  E = "E",
  Tleaf = "Leaf temperature",
  Tcuv = "Cuvette temperature",
  VPD = "Vapour pressure deficit",
  rh = "Relative humidity",
  ca = "Ambient CO2",
  ci = "Intercellular CO2",
  `White x T` = "Light intensity",
  PARtop = "PARtop",
  Area = "Area"
)

walz_config <- function() {
  list(
    drive_folder_id = Sys.getenv(
      "WALZ_DRIVE_FOLDER_ID",
      unset = WALZ_DEFAULT_DRIVE_FOLDER_ID
    ),
    api_key = Sys.getenv("GOOGLE_DRIVE_API_KEY", unset = ""),
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
