# load libraries
library(stringr)
library(stringi)
library(dplyr)
library(googlesheets4)
library(readr)
library(tidygeocoder)
library(httr)
set_config(config(ssl_verifypeer = 0L))

# Preprocesado da informacion -- Extraccion da xeolocalizacion a partir do nome da praia

# Function to clean values
clean_values <- function(value) {
  if (!grepl("^https?://", value)) {
    # Remove leading and trailing parentheses
    value <- gsub("^\\(|\\)$", "", value)
  }
  return(value)
}

parse_dates <- function(timestamp){
  tryCatch(
    posix_time <- as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
    , finally = {
      posix_time <- as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
    }
  )
}

# Function to extract and convert coordinates from various formats
extract_and_convert_coords <- function(coord_str) {

  # Function to clean values
  clean_values <- function(value) {
    if (!grepl("^https?://", value)) {
      # Remove leading and trailing parentheses
      value <- gsub("^\\(|\\)$", "", value)
    }
    return(value)
  }

  # Pattern for detecting and extracting coordinates in various formats
  pattern <- "\\(?\\s*(-?\\d{1,3}\\.?\\d{0,15})[°, ]*\\s*([NS])?[,\\s]*\\s*(-?\\d{1,3}\\.?\\d{0,15})[°, ]*\\s*([EW])?\\)?"
  matches <- regmatches(coord_str, gregexpr(pattern, coord_str))

  if (length(matches[[1]]) > 0) {
    parts <- strsplit(matches[[1]][1], "[^0-9.-]+")[[1]]
    lat <- as.numeric(parts[1])
    lon <- as.numeric(parts[2])

    # Adjust for N/S and E/W if present
    if (length(parts) > 2 && !is.na(parts[3])) {
      if (toupper(parts[3]) == "S") lat <- -lat
      if (toupper(parts[3]) == "W") lon <- -lon
    }
    return(c(lat, lon))
  }
  return(NULL)
}

#Preprocesado da informacion -- Extraccion da xeolocalizacion a partir do nome da praia

get_data <- function(update_all = FALSE){

  #Read google sheets data into R
  options(gargle_oauth_cache =".secrets")
  json_key <- Sys.getenv("GOOGLE_SHEETS_JSON_KEY") #name of the file on .secrets
  googlesheets4::gs4_auth(email = "pelletmap@earnest-vine-377812.iam.gserviceaccount.com", path=json_key)

  ss <- 'https://docs.google.com/spreadsheets/d/1E7K92pX4aS7CmGJWjYavEL8menX2gBkHoxtT3YTXwoc/'
  data <- googlesheets4::read_sheet(ss)

  names(data) <- make.names(names(data))

  hour <- as.POSIXct(data$Hora, tz = "UTC")
  data$Hora <- format(hour, format = "%H")

  data$Marca.temporal <- sapply(data$Marca.temporal, parse_dates)
  data$Marca.temporal <- as.POSIXct(data$Marca.temporal, origin = "1970-01-01", tz = "UTC")

  # data already retrieved
  if (file.exists("praias.csv")) {
    praias <- read_csv("praias.csv")
    if(nrow(praias > 0)){
      praias$Marca.temporal <- sapply(praias$Marca.temporal, parse_dates)
      praias$Marca.temporal <- as.POSIXct(praias$Marca.temporal, origin = "1970-01-01", tz = "UTC")

      max_date <- max(as.POSIXct(praias$Marca.temporal)) # Latest register processed in praias.csv

      if(update_all == FALSE){
        data <- data %>% filter(as.POSIXct(data$Marca.temporal) > max_date)
      }
      else{
        # Get registers from current day
        message("Updating today's data...")
        today <- format(Sys.Date(), format = "%Y-%m-%d")
        data <- data %>% filter(format(data$Marca.temporal, format = "%Y-%m-%d") == today)
      }
    }
  }

  message(paste("Processing ", nrow(data), " rows"))
  for (idx in seq_len(nrow(data))) {

    coord_str <- data$`Xeolocalización`[idx]

    coord_str <- sapply(coord_str, clean_values)

    value <- extract_and_convert_coords(coord_str)

    if (is.null(value)) {
      place_name <- data$Nome.da.praia..Concello[idx]
      geo_result <- geo(place_name, method = "osm", full_results = FALSE)

      if(is.na(geo_result$lat)){
        # probar so co nome da praia...
        praia_concello <- sapply(place_name, function(x) strsplit(x, ", ")[[1]], USE.NAMES=FALSE)
        geo_result <- geo(praia_concello[1], method = "osm", full_results = FALSE)
        if(is.na(geo_result$lat) & !is.null(praia_concello[2])){
          geo_result <- geo(praia_concello[2], method = "osm", full_results = FALSE) #only concello if available
        }
        else if(is.null(praia_concello[2])){
          # get concello from var name
          geo_result <- geo(data$Concello[idx], method = "osm", full_results = FALSE)
        }
      }
      data$lat[idx] <- geo_result$lat
      data$lon[idx] <- geo_result$long
    } else {
      data$lat[idx] <- value[1]
      data$lon[idx] <- value[2]
    }
  }

  if(file.exists("praias.csv") & nrow(praias) > 0){
    file.remove("praias.csv")
    data <- rbind(data, praias)
  }
  write_csv(data, "praias.csv")
}
