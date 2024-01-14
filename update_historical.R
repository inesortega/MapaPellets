# load libraries
source("data_loader.R")


message("Starting update of dataset in background......")

last_fast_update <- Sys.time()
last_historical_update <- Sys.time()

repeat{
  current_time <- Sys.time()
  monitor_fast <- as.numeric(difftime(current_time, last_fast_update, units = "mins"))
  monitor_historical <- as.numeric(difftime(current_time, last_historical_update, units = "mins"))

  if (monitor_fast >= 3) { # Trigger fast update
    last_fast_update <- current_time  # Update the last run time
    message(paste("Actualizando datos..."))
    tryCatch({
      get_data()
    },
    error = function(e) {
      message("Error cargando datos...")
    })
  }
  if(monitor_historical >= 5){ # Trigger update of today's data every hour
    last_historical_update <- current_time  # Update the last run time
    message(paste("Actualizando datos diarios..."))
    tryCatch({
      get_data(update_all = TRUE)
    },
    error = function(e) {
      message("Error actualizando datos diarios...")
    })
  }
}

repeat{
  Sys.sleep(200)  # sleep for 6hr 21600
  message("Updating dataset...")
  tryCatch({
      get_data(update_all_dataset = TRUE)
  },
  error = function(e) {
    message(paste("Error actualizando os datos...", e))
  })
}
