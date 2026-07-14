project_root <- normalizePath(file.path("..", ".."), mustWork = TRUE)

source(file.path(project_root, "R", "config.R"))
source(file.path(project_root, "R", "walz_parser.R"))
source(file.path(project_root, "R", "protocol_match.R"))
source(file.path(project_root, "R", "drive_data.R"))
source(file.path(project_root, "R", "plots.R"))

fixture_path <- function(filename) {
  file.path(project_root, "tests", "testthat", "fixtures", filename)
}
