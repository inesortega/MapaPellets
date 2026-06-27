FROM rocker/r2u:22.04

ENV DEBIAN_FRONTEND=noninteractive
ENV MAKEFLAGS="-j1"
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libmysqlclient-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    netcdf-bin \
    libuv1-dev \
    cmake \
    libabsl-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    make \
    g++ \
    && rm -rf /var/lib/apt/lists/*

# Nota: r-cran-shiny 1.14.0 no repo apt non declara correctamente as súas
# dependencias de tempo de execución (httpuv, mime, jsonlite, ...), polo que
# se instalan aquí de forma explícita.
RUN apt-get update && apt-get install -y --no-install-recommends \
    r-cran-shiny \
    r-cran-httpuv \
    r-cran-mime \
    r-cran-jsonlite \
    r-cran-fontawesome \
    r-cran-r6 \
    r-cran-later \
    r-cran-promises \
    r-cran-rlang \
    r-cran-fastmap \
    r-cran-commonmark \
    r-cran-glue \
    r-cran-bslib \
    r-cran-cachem \
    r-cran-lifecycle \
    r-cran-shinywidgets \
    r-cran-shinythemes \
    r-cran-shinydashboard \
    r-cran-shinyjs \
    r-cran-magrittr \
    r-cran-googlesheets4 \
    r-cran-dplyr \
    r-cran-tibble \
    r-cran-leaflet \
    r-cran-stringr \
    r-cran-stringi \
    r-cran-httr \
    r-cran-ggplot2 \
    r-cran-shinycssloaders \
    r-cran-htmltools \
    r-cran-readr \
    r-cran-sf \
    r-cran-terra \
    r-cran-units \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    netcdf-bin \
    && rm -rf /var/lib/apt/lists/*

COPY ./requirements-src.R .

# Verificación: cárganse TODOS os paquetes que usa a app, para que calquera que
# falte rompa o build (no CI) en vez de fallar en tempo de execución.
RUN Rscript requirements-src.R && \
    R -e "library(shiny); library(leaflet); library(googlesheets4); library(shinyWidgets); library(shinyjs); library(shinydashboard); library(shinythemes); library(shinycssloaders); library(dplyr); library(tibble); library(readr); library(stringr); library(stringi); library(httr); library(htmltools); library(magrittr); library(ggplot2); library(tidygeocoder)"

COPY ./ /app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
