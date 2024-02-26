# load libraries
library(stringr)
library(stringi)
library(dplyr)
library(googlesheets4)
library(readr)
library(tidygeocoder)
library(httr)
set_config(config(ssl_verifypeer = 0L))

# Function to clean values
clean_values <- function(value) {
  if (!grepl("^https?://", value)) {
    # Remove leading and trailing parentheses
    value <- gsub("^\\(|\\)$", "", value)
    value <- gsub("(\\d),(\\d)", "\\1.\\2", value, perl = TRUE)
    return(value)
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

validate_coordinates <- function(lat, lon) {
  if(is.na(lat) || is.na(lon)){
    message("Invalid coordinates.")
    return(NULL)
  }
  if (!is.numeric(lat) || !is.numeric(lon)) {
    message("Invalid coordinates: Non-numeric values.")
    return(NULL)
  }

  if (lat < -90 || lat > 90 || lon < -180 || lon > 180) {
    message("Invalid coordinates: Out of range.")
    return(NULL)
  }
  return(0)
}

# Function to extract and convert coordinates from various formats
extract_and_convert_coords <- function(coord_str) {

  if(is.na(coord_str)[[1]]) return(NULL)
  # Pattern for detecting and extracting coordinates in various formats
  pattern <- "\\(?\\s*(-?\\d{1,3}\\.?\\d{0,15})[°, ]*\\s*([NS])?[,\\s]*\\s*(-?\\d{1,3}\\.?\\d{0,15})[°, ]*\\s*([EW])?\\)?"
  matches <- regmatches(coord_str, gregexpr(pattern, coord_str))

  if (length(matches[[1]]) > 0) {
    parts <- strsplit(matches[[1]], ",")[[1]]

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

# Function to apply URL encoding to a vector to prevent XSS or other input attacks
url_encode_vector <- function(x) {
  if(!is.na(x)) URLencode(x)
}

#Preprocesado da informacion -- Extraccion da xeolocalizacion a partir do nome da praia

get_data <- function(update_all = FALSE, update_all_dataset = FALSE){

  #Read google sheets data into R
  tryCatch(
    {
      options(gargle_oauth_cache =".secrets")
      json_key <- Sys.getenv("GOOGLE_SHEETS_JSON_KEY") #name of the file on .secrets
      googlesheets4::gs4_auth(path=json_key)

      ss <- 'https://docs.google.com/spreadsheets/d/1E7K92pX4aS7CmGJWjYavEL8menX2gBkHoxtT3YTXwoc/'
      data <- googlesheets4::read_sheet(ss)
      data$lat <- NA
      data$lon <- NA
      message(paste("Rows in cloud dataset = ", nrow(data)))

    },
    error = function(e) {
      message(paste("Error o cargar os datos: "), e)
      return()
    }
  )

  names(data) <- make.names(names(data))

  hour <- as.POSIXct(data$Hora, tz = "UTC")
  data$Hora <- format(hour, format = "%H")

  data$Marca.temporal <- sapply(data$Marca.temporal, parse_dates)
  data$Marca.temporal <- as.POSIXct(data$Marca.temporal, origin = "1970-01-01", tz = "UTC")

  # data already retrieved
  if (file.exists("./data/praias.csv")) {
    praias <- read_csv("./data/praias.csv",  show_col_types = FALSE)

    missing_columns <- setdiff(names(data), names(praias))

    for(col in missing_columns){
      # if new cols are included in the updated dataset, add them to the historical to not break dataset rbind
      praias[[col]] <- NA
    }

    if(nrow(praias) > 0){

      praias$Marca.temporal <- sapply(praias$Marca.temporal, parse_dates)
      praias$Marca.temporal <- as.POSIXct(praias$Marca.temporal, origin = "1970-01-01", tz = "UTC")

      if(update_all == TRUE){
        # Get registers from current day
        message("Updating today's data...")
        today <- format(Sys.Date(), format = "%Y-%m-%d")
        indexes <- which(format(data$Marca.temporal, format = "%Y-%m-%d") == today)
      }
      else if(update_all_dataset == TRUE){
        # Update entire dataset from the cloud
        message("Retrieving entire dataset...")
        indexes <- as.numeric(rownames(data))
      }
      else if(update_all_dataset == FALSE && update_all == FALSE){
        # Get only new data
        max_date <- max(as.POSIXct(praias$Marca.temporal)) # Latest register processed in ./data/praias.csv
        indexes <- which(as.POSIXct(data$Marca.temporal) > max_date)
      }

      # assign already learned or processed values to data
      current_indexes <- as.numeric(rownames(praias))
      data[current_indexes, ]$lat <- praias[current_indexes, ]$lat
      data[current_indexes, ]$lon <- praias[current_indexes, ]$lon

    }
  }
  else{
    praias <- data
    indexes <- as.numeric(rownames(praias))
  }

  message(paste("Processing ", length(indexes), " rows"))

  ### Update required data ####
  for (idx in indexes) {

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
      if(!is.null(validate_coordinates(geo_result$lat, geo_result$long))){
        data$lat[idx] <- geo_result$lat
        data$lon[idx] <- geo_result$long
      }
    } else {
      data$lat[idx] <- value[1]
      data$lon[idx] <- value[2]
    }
  }

  if(file.exists("./data/praias.csv")){
    file.remove("./data/praias.csv")
  }
  write_csv(data, "./data/praias.csv")
}
