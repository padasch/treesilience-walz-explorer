project_root <- normalizePath(file.path("..", ".."), mustWork = TRUE)

source(file.path(project_root, "R", "config.R"))
source(file.path(project_root, "R", "walz_parser.R"))
source(file.path(project_root, "R", "dew_point.R"))
source(file.path(project_root, "R", "protocol_match.R"))
source(file.path(project_root, "R", "drive_data.R"))
source(file.path(project_root, "R", "plots.R"))

fixture_path <- function(filename) {
  file.path(project_root, "tests", "testthat", "fixtures", filename)
}

dew_point_fixture <- function() {
  parsed <- read_walz_csv(fixture_path("walz_sample.csv"))
  rows <- nrow(parsed$data)
  parsed$data$wa <- seq(15000, 18000, length.out = rows)
  parsed$data$Pamb <- rep(100, rows)
  parsed$data$Tamb <- seq(18, 20, length.out = rows)
  parsed$units <- c(
    parsed$units,
    wa = "ppm",
    Pamb = "kPa",
    Tamb = "°C"
  )
  parsed
}
