# requirements-src.R
options(Ncpus = 16)

pkgs <- c(
    'shinyWidgets',
    'shinythemes',
    'shinydashboard',
    'shinyjs',
    'magrittr',
    'dplyr',
    'tibble',
    'stringr',
    'shiny',
    'tidygeocoder',
    'htmltools',
    'readr',
    'ggplot2',
    'shinycssloaders',
    'oce',
    'googlesheets4',
    'leaflet'
)

for (l in pkgs) {

    install.packages(l, dependencies=TRUE, repos='https://cran.rstudio.com/');

    if ( ! library(l, character.only=TRUE, logical.return=TRUE) ) {
        quit(status=1, save='no')
    }
}
