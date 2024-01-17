source("/app/data_loader.R")

message("Starting update of dataset in background......")

last_fast_update <- Sys.time()
last_historical_update <- Sys.time()
last_daily_update <- NULL

repeat{
  current_time <- Sys.time()
  current_hour <- as.numeric(format(current_time, "%H"))
  
  monitor_fast <- as.numeric(difftime(current_time, last_fast_update, units = "mins"))
  monitor_historical <- as.numeric(difftime(current_time, last_historical_update, units = "mins"))
  
  if(!is.null(last_daily_update)){
    monitor_daily <- as.numeric(difftime(current_time, last_daily_update, units = "hours"))
  }
  else{
    monitor_daily <- NULL # NULL until firts run at 3AM
  }

  if (monitor_fast >= 5) { # Trigger fast update
    last_fast_update <- current_time  # Update the last run time
    message(paste("Actualizando datos...", Sys.time()))
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
    message(paste("Actualizando datos diarios - ", Sys.time()))
    tryCatch({
      get_data(update_all = TRUE)
    },
    error = function(e) {
      message(paste("Error actualizando datos diarios..."), e)
    })
    message(paste("--------------------------------------------------------------------------------------"))
  }
  if (current_hour == 3)  {
    update_time = FALSE #Check condition
    
    if(is.null(monitor_daily)){ # first time reaching 3am
      update_time = TRUE
    }
    else{
      # A daily update has been triggered... check if 24 hr have passed
      if(monitor_daily >= 24){
        update_time = TRUE
      }
    }

    if(update_time){
      message("Son as 03:00 AM. Actualización completa...")
      message(paste("Actualizando datos históricos...", Sys.time()))
      tryCatch({
        get_data(update_all_dataset = TRUE)
        last_daily_update <- Sys.time()
      },
      error = function(e) {
        message(paste("Error actualizando datos históricos..."), e)
      })
      message(paste("--------------------------------------------------------------------------------------"))
    }
  }
}
