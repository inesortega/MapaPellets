# Actualización de datos dun só disparo (substitúe ao bucle infinito de
# update_historical.R). Arranca, actualiza o CSV e sae, liberando a memoria.
#
# Lánzase dende cron, dentro do contedor de Shiny xa en execución:
#   Rscript /app/update_once.R [fast|today|full]
#
#   fast   -> só rexistros novos                 (get_data())                cada 5 min
#   today  -> rexistros do día de hoxe           (get_data(update_all))      cada hora
#   full   -> dataset completo dende a nube       (get_data(update_all_dataset)) ás 03:00

source("/app/data_loader.R")

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[[1]] else "fast"

message(sprintf("[update_once] modo=%s  inicio=%s", mode, format(Sys.time())))

result <- tryCatch({
  switch(mode,
    fast  = get_data(),
    today = get_data(update_all = TRUE),
    full  = get_data(update_all_dataset = TRUE),
    stop(sprintf("Modo descoñecido: '%s' (usa fast|today|full)", mode))
  )
  "OK"
}, error = function(e) {
  message(sprintf("[update_once] ERRO: %s", conditionMessage(e)))
  "ERROR"
})

message(sprintf("[update_once] modo=%s  fin=%s  estado=%s",
                mode, format(Sys.time()), result))

# Código de saída non cero en caso de erro (útil para cron / logs)
if (identical(result, "ERROR")) quit(status = 1)
