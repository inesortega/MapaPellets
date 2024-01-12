# load required packages
library(magrittr)
library(dplyr)
library(tibble)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(htmltools)
library(readr)

source("data_loader.R")

start_date <- as.Date("2024-01-06")
end_date <- Sys.Date()

values <- as.list(seq(start_date, end_date, by = "days"))

ui <- dashboardPage(skin = "black",
  dashboardHeader(
    title = "Unha Vez Máis",
    titleWidth = 320
    ),
  dashboardSidebar(
    width = 320,
    sliderInput(
      inputId = "dateRange",
      label = "Filtrado por data:",
      min = as.Date("2024-01-06"),
      max = Sys.Date(),
      value = c(Sys.Date() -1, Sys.Date())),
    checkboxGroupInput("legendFilter", "Filtrar por tipo de actualización:",
                       choices = c(
                         "Hai pellets na praia" = "Hai pellets na praia",
                         "Non hai pellets na praia" = "Non hai pellets na praia",
                         "Xa non hai (a praia quedaba limpa cando se encheu o formulario)" = "Xa non hai (a praia quedaba limpa cando se encheu o formulario)",
                         "Convocatoria de xornada de limpeza" = "Convocatoria de xornada de limpeza"
                       ),
                       selected = c(
                         "Hai pellets na praia",
                         "Non hai pellets na praia",
                         "Xa non hai (a praia quedaba limpa cando se encheu o formulario)",
                         "Convocatoria de xornada de limpeza"
                       )),
    conditionalPanel(
      collapsed = TRUE,
      condition = "input.mymap_marker_click",
      uiOutput("sidebarContent")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.skin-blue .main-header .logo {background-color: #3c8dbc;}.skin-blue .main-header .logo:hover {background-color: #3c8dbc;}'))),
    # Adjust the size of the map container
    tags$style(HTML(".leaflet { height: calc(100vh - 200px) !important; }")),  # Set map height to 100% viewport height
    fluidRow(
      column(12, h4("Mapa actualizado da situación das praias galegas")),
      column(12, paste("Seguemento cidadán do estado das praias galegas despois do vertido de pellets. O panel lateral mostra a última actualización dos datos e información adicional sobre cada punto rexistrado.")),
      column(12, HTML(paste("Todos os datos recóllense de xeito colaborativo por voluntarios e entidades colaborativas a través deste ", "<a href='https://docs.google.com/forms/u/1/d/e/1FAIpQLScHqNH3yxk5yBKhOMZ0mVk0Wl-bNLCowqW9UFr0mo2Hj7klGA/formResponse' target='_blank'>formulario</a>"))),
      column(12, leafletOutput("mymap"))  # Adjust the width of the map
    ),
    fluidRow(
      column(12,
             tags$footer(
               style = "text-align: center; padding: 10px; background-color: #f0f0f0;",
               "Author: ", tags$a("Inés Ortega Fernández", href = "https://inesortega.github.io/"),
               " | Contact: ", tags$a("datospellets@gmail.com", href = "datospellets@gmail.com")
             )
      ),
      column(12,
             tags$footer(
               style = "text-align: center; padding: -10px; background-color: #f0f0f0;",
               HTML(paste("Este proxecto é posible grazas a colaboración e recollida de datos de voluntarias, de Noia Limpa, e a cesión e recursos por parte de ", "<a href='https://gradiant.org' target='_blank'>Gradiant</a>"))
             )
      )
    ),
    useShinyjs(),
  ),
  tags$head(
    tags$style(HTML(".main-sidebar {background-color:  #424b57 !important; color: #000000 !important}"))
  )
)


color_palette <- c("Hai pellets na praia" = "#e31a1c",
                   "Non hai pellets na praia" = "#33a02c",
                   "Convocatoria de xornada de limpeza" = "#701796",
                   "Xa non hai (a praia quedaba limpa cando se encheu o formulario)"= "#1f78b4")


server <- function(input, output, session) {

  updateTimestamp <- reactiveValues(time = Sys.time())
  isFirstRun <- reactiveVal(TRUE)

  # Create a reactiveValues object to store the clicked marker information
  markerInfo <- reactiveValues(clickedMarker = NULL)

  observe({
    invalidateLater(300000) # 300000 = 5min
    showNotification(paste("Actualizando datos..."), duration = 10)
    tryCatch({
      get_data()
    },
    error = function(e) {
      showNotification("Error cargando datos...", duration = NULL, type = "error" )
    })
  })

  last_run_time <- Sys.time() # Initialize the last run time
  observe({
    invalidateLater(600) # check every second if update needed...
    current_time <- Sys.time()
    time_since_last_run <- as.numeric(difftime(current_time, last_run_time, units = "hours"))
    if (time_since_last_run >= 1) {  # Check if 1 hours have passed
      last_run_time <- current_time  # Update the last run time
      showNotification(paste("Actualizando datos históricos..."), duration = 60)
      tryCatch({
        get_data(update_all = TRUE)
      },
      error = function(e) {
        showNotification("Error cargando datos...", duration = NULL, type = "error" )
      })
    }
  })

  data <- reactive({
    updateTimestamp$time <- Sys.time()

    all_data <- read_csv("praias.csv")
    all_data$Marca.temporal <- as.Date(all_data$Marca.temporal)

    # Filter data based on selected date range
    if (!is.null(input$dateRange)) {
      start_date <- input$dateRange[1]
      end_date <- input$dateRange[2]
      filtered_data <- all_data %>%
        filter(Marca.temporal >= start_date & Marca.temporal <= end_date)
    } else {
      filtered_data <- all_data
    }

    if(nrow(filtered_data) > 0){
      filtered_data$id <- seq.int(nrow(filtered_data))
    }

    filtered_data

  })

  # Create a reactive object for each type
  data_hai_pellets <- reactive({
    data() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Hai pellets na praia")
  })

  data_non_hai_pellets <- reactive({
    data() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Non hai pellets na praia")
  })

  data_convocatoria <- reactive({
    data() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Convocatoria de xornada de limpeza")
  })

  data_xa_non_hai <- reactive({
    data() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Xa non hai (a praia quedaba limpa cando se encheu o formulario)")
  })

  color_factor <- reactive({
    colorFactor(palette = color_palette, na.color = "grey",
                levels = c("Hai pellets na praia", "Non hai pellets na praia", "Convocatoria de xornada de limpeza", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)"))
  })

  # Render map

  proxy_map <- leafletProxy("mymap")

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -8.1, lat = 42.5, zoom = 7) %>%
      addLegend(
        position = "bottomright",
        colors = c("#e31a1c", "#33a02c", "#701796", "#1f78b4"),  # Colors for the four categories
        labels = c(paste("Hai pellets na praia (", nrow(data_hai_pellets()), ")"),
                   paste("Non hai pellets na praia (", nrow(data_non_hai_pellets()), ")"),
                   paste("Convocatoria de xornada de limpeza(", nrow(data_convocatoria()), ")"),
                   paste("Xa non hai (quedou limpa) (", nrow(data_xa_non_hai()), ")")),
        title = "Tipo de información e reconto por tipo"
      )
  })

  observe({
    # Check if the data is not empty
    if (!is.null(data()) && nrow(data()) > 0) {
      proxy_map <- leafletProxy("mymap")
      proxy_map %>% clearMarkers()

      # Determine the selected filters
      selected_filters <- input$legendFilter

      # Add markers based on the selected filters
      if ("Todos" %in% selected_filters) {
        proxy_map %>%
          addCircleMarkers(
            data = data(),
            lng = ~lon,
            lat = ~lat,
            label = ~Nome.da.praia..Concello,
            layerId = ~id,
            radius = 10,
            color = ~color_factor()(Tipo.de.actualización.que.nos.queres.facer.chegar),
            stroke = FALSE, fillOpacity = 0.5
          )
      } else {
        for (selected_filter in selected_filters) {
          if (selected_filter == "Hai pellets na praia" & nrow(data_hai_pellets()) > 0) {
            proxy_map %>%
              addCircleMarkers(
                data = data_hai_pellets(),
                lng = ~lon,
                lat = ~lat,
                label = ~Nome.da.praia..Concello,
                layerId = ~id,
                radius = 10,
                color = "#e31a1c",
                popup = ~paste(
                  paste("<strong>Praia: </strong>", htmlEscape(Nome.da.praia..Concello), "<br>"),
                  paste("<strong>Data da actualización: </strong>", htmlEscape(Marca.temporal), "<br>"),
                  paste("<strong>Está avisado o 112?</strong>", htmlEscape(Está.avisado.o.112.), "<br>"),
                  paste("<strong>Hai animais mortos</strong>", htmlEscape(Atopaches.animáis.mortos.), "<br>"),
                  paste("<strong>Por onde están espallados: </strong>", htmlEscape(Por.onde.están.espallados.os.pellets), "<br>"),
                  paste("<strong>Quen está recollendo os pellets?</strong>", htmlEscape(Quen.está.recollendo.os.pellets.), "<br>"),
                  paste("<strong>Como se están a recoller?</strong>", htmlEscape(Como.estase.a.recoller.os.pellets.), "<br>"),
                  paste("<strong>Onde se depositan?</strong>", htmlEscape(Onde.se.depositan.os.pellets.), "<br>"),
                  paste("<strong>Hai sacos de pellets?</strong>", htmlEscape(Hai.sacos.de.pellets.), "<br>"),
                  paste("<strong>Cantidade de pellets: </strong>", htmlEscape(Cantidade.de.pellets), "<br>"),
                  paste("<strong>Concello: </strong>", htmlEscape(Concello), "<br>")
                ),
                stroke = FALSE, fillOpacity = 0.5
              )

          } else if (selected_filter == "Non hai pellets na praia" & nrow(data_non_hai_pellets()) > 0) {
            proxy_map %>%
              addCircleMarkers(
                data = data_non_hai_pellets(),
                lng = ~lon,
                lat = ~lat,
                label = ~Nome.da.praia..Concello,
                layerId = ~id,
                radius = 10,
                color = "#33a02c",
                popup = ~paste(
                  paste("<strong>Praia: </strong>", htmlEscape(Nome.da.praia..Concello), "<br>"),
                  paste("<strong>Data da actualización: </strong>", htmlEscape(Marca.temporal), "<br>"),
                  paste("<strong>Hai animais mortos</strong>", htmlEscape(Atopaches.animáis.mortos.), "<br>")
                ),
                stroke = FALSE, fillOpacity = 0.5
              )
          } else if (selected_filter == "Convocatoria de xornada de limpeza" & nrow(data_convocatoria()) > 0) {
            proxy_map %>%
              addCircleMarkers(
                data = data_convocatoria(),
                lng = ~lon,
                lat = ~lat,
                label = ~Nome.da.praia..Concello,
                layerId = ~id,
                radius = 10,
                color = "#701796",
                popup = ~paste(
                  paste("<strong>Data da convocatoria: </strong>", htmlEscape(as.Date(Data)), " ", htmlEscape(Hora), "<br>"),
                  paste("<strong>Praia: </strong>", htmlEscape(Nome.da.praia..Concello), "<br>"),
                  paste("<strong>Lugar de encontro: </strong>", htmlEscape(Lugar.de.encontro), "<br>"),
                  paste("<strong>Quen organiza a iniciativa? </strong>", htmlEscape(Quen.organiza.a.iniciativa), "<br>"),
                  paste(tags$a("Ligazón", href = htmlEscape(Ligazón)), "<br>")
                ),
                stroke = FALSE, fillOpacity = 0.5
              )
          } else if (selected_filter == "Xa non hai (a praia quedaba limpa cando se encheu o formulario)" & nrow(data_xa_non_hai()) > 0) {
            proxy_map %>%
              addCircleMarkers(
                data = data_xa_non_hai(),
                lng = ~lon,
                lat = ~lat,
                label = ~Nome.da.praia..Concello,
                layerId = ~id,
                radius = 10,
                popup = ~paste(
                  paste("<strong>Praia: </strong>", htmlEscape(Nome.da.praia..Concello), "<br>"),
                  paste("<strong>Data da limpeza: </strong>", htmlEscape(Marca.temporal), "<br>"),
                  paste("<strong>Hai animais mortos</strong>", htmlEscape(Atopaches.animáis.mortos.), "<br>"),
                  paste("<strong>Por onde están espallados: </strong>", htmlEscape(Por.onde.están.espallados.os.pellets), "<br>"),
                  paste("<strong>Quen está recollendo os pellets?</strong>", htmlEscape(Quen.está.recollendo.os.pellets.), "<br>"),
                  paste("<strong>Como se están a recoller?</strong>", htmlEscape(Como.estase.a.recoller.os.pellets.), "<br>"),
                  paste("<strong>Onde se depositan?</strong>", htmlEscape(Onde.se.depositan.os.pellets.), "<br>"),
                  paste("<strong>Hai sacos de pellets?</strong>", htmlEscape(Hai.sacos.de.pellets.), "<br>"),
                  paste("<strong>Cantidade de pellets: </strong>", htmlEscape(Cantidade.de.pellets), "<br>"),
                  paste("<strong>Concello: </strong>", htmlEscape(Concello), "<br>")
                ),
                color = "#1f78b4",
                stroke = FALSE, fillOpacity = 0.5
              )
          }
        }
      }
    } else {
      # Clear any existing markers and popups
      proxy_map %>%
        clearMarkers() %>%
        clearPopups()
      # Optionally, you can add a popup or some other notification
      # to indicate that there is no data to display
      # Example: Displaying a popup at a default location
      proxy_map %>%
        addPopups(lng = -8.1, lat = 42.5, popup = "Non hai datos para mostrar con estes filtros.")
    }
  })

  # Update the clicked marker information when a marker is clicked
  observe({
    marker_id <- input$mymap_marker_click$id
    if (!is.null(marker_id)) {
      info <- data()[marker_id, ]
      #### add content to sidebar

      tipo <- info$Tipo.de.actualización.que.nos.queres.facer.chegar

      if(tipo != "Convocatoria de xornada de limpeza"){

        links <- c()

        if(tipo == "Hai pellets na praia"){
          if(!is.na(info$Imaxe.dos.pellets.no.lugar.ou.da.xornada.de.limpeza)){
            links <- sapply(info$Imaxe.dos.pellets.no.lugar.ou.da.xornada.de.limpeza, function(x) strsplit(x, ", "), USE.NAMES=FALSE)[[1]]
          }
        }
        else if(tipo ==  "Non hai pellets na praia"){
          if(!is.na(info$Imaxes.adicionais)){
            links <- sapply(info$Imaxes.adicionais, function(x) strsplit(x, ", "), USE.NAMES=FALSE)[[1]]
          }
        }

        output$sidebarContent <- renderUI({
          sidebar <- fluidRow(
            HTML("<div style='padding-left: 20px;'>"),
            column(12, h3("Información adicional:")),
            column(12, HTML(paste(
              "<strong>Praia: </strong>", htmlEscape(info$Nome.da.praia..Concello), "<br>",
              "<strong>Data da limpeza: </strong>", htmlEscape(info$Marca.temporal), "<br>",
              "<strong>Concello: </strong>", htmlEscape(info$Concello), "<br>",
              "<strong>Información adicional: </strong>", htmlEscape(info$Información.adicional), "<br>"
            ))),
            column(12, h4("Imaxes dispoñibles:")),
            column(12, HTML(paste(
              lapply(1:length(links), function(i) {
                if(!is.null(links[i])){
                  HTML(paste(
                    "<a href='", htmlEscape(url_encode_vector(links[i])), "' target='_blank'>Imaxe ", i, "</a><br>"
                  ))
                }else{
                  paste("")
                }
              }),
              collapse = ""
            ))),
            if (!is.null(updateTimestamp$time)) {
              column(12, h5(paste("Última  Actualización: ", updateTimestamp$time)))
            }
          )
          return(sidebar)
        })
      }
      else{
        output$sidebarContent <- renderUI({
          sidebar <- fluidRow(
            HTML("<div style='padding-left: 20px;'>"),
            column(12, h3("Convocatoria de Limpeza:")),
            column(12, HTML(paste(
              "<strong>Data e hora: </strong>", htmlEscape(info$Data), " ", htmlEscape(info$Hora), "<br>",
              "<strong>Lugar de encontro: </strong>", htmlEscape(info$Lugar.de.encontro), "<br>",
              "<strong>Quen organiza a iniciativa: </strong>", htmlEscape(info$Quen.organiza.a.iniciativa), "<br>",
              "<strong>Contacto: </strong>", htmlEscape(info$Contacto), "<br>",
              tags$a("Cartaz", href = htmlEscape(url_encode_vector(info$Cartaz))), "<br>",
              tags$a("Ligazón", href = htmlEscape(url_encode_vector(info$Ligazón))), "<br>"))),
            if (!is.null(updateTimestamp$time)) {
              column(12, h5(paste("Última  Actualización: ", updateTimestamp$time)))
            }
          )
          return(sidebar)
        })

      }
    }
  })

  # Display the marker information in the sidebar
  output$markerInfoText <- renderPrint({
    markerInfo$clickedMarker
  })

}

message(Sys.getenv("ENV"))
if(Sys.getenv("ENV") == "docker"){
  options(shiny.host = '0.0.0.0')
  options(shiny.port = 3838)
}

shinyApp(ui, server)
