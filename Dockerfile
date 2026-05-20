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

RUN apt-get update && apt-get install -y --no-install-recommends \
    r-cran-shiny \
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

RUN Rscript requirements-src.R && \
    R -e "library(shiny); library(leaflet); library(googlesheets4); library(shinyWidgets); library(shinyjs); library(shinydashboard); library(dplyr); library(readr)"

COPY ./ /app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
