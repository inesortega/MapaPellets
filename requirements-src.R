# requirements-src.R

pkgs <- c(
  "shinyWidgets",
  "shinythemes",
  "shinydashboard",
  "shinyjs",
  "magrittr",
  "googlesheets4",
  "dplyr",
  "tibble",
  "leaflet",
  "stringr",
  "shiny",
  "tidygeocoder",
  "htmltools",
  "readr"
)

install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, repos = "https://cloud.r-project.org")
  }
}

invisible(lapply(pkgs, install_if_missing))
