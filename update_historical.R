source("/app/data_loader.R")

message("Starting update of dataset in background......")

last_fast_update <- Sys.time()
last_historical_update <- Sys.time()

repeat{
  current_time <- Sys.time()
  current_hour <- as.numeric(format(current_time, "%H"))

  monitor_fast <- as.numeric(difftime(current_time, last_fast_update, units = "mins"))
  monitor_historical <- as.numeric(difftime(current_time, last_historical_update, units = "mins"))
  
  if (monitor_fast >= 5) { # Trigger fast update
    last_fast_update <- current_time  # Update the last run time
    message(paste("Actualizando datos..."))
    tryCatch({
      get_data()
    },
    error = function(e) {
      message(paste("Error actualizando datos..."), e)
    })
    message(paste("--------------------------------------------------------------------------------------"))
  }
  if(monitor_historical >= 60){ # Trigger update of today's data every hour
    last_historical_update <- current_time  # Update the last run time
    message(paste("Actualizando datos diarios..."))
    tryCatch({
      get_data(update_all = TRUE)
    },
    error = function(e) {
      message(paste("Error actualizando datos diarios..."), e)
    })
    message(paste("--------------------------------------------------------------------------------------"))
  }
  if (current_hour == 3) {
    paste("Son as 03:00 AM. Toca actualización completa")    
    message(paste("Actualizando datos históricos..."))
    tryCatch({
      get_data(update_all_dataset = TRUE)
    },
    error = function(e) {
      message(paste("Error actualizando datos históricos..."), e)
    })
    message(paste("--------------------------------------------------------------------------------------"))
  }
}
