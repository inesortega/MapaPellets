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

source("load_GDrive.R")

ui <- dashboardPage(skin = "black",
                    dashboardHeader(
                      title = "Unha Vez Máis",
                      titleWidth = 320
                    ),
                    dashboardSidebar(
                      width = 320,
                      dateRangeInput("dateRange",
                                     label = "Filtrado por data:",
                                     start = Sys.Date() - 2, # default start date
                                     end = Sys.Date()), # default end date# Added sidebar content
                      selectInput("legendFilter", "Filtrar por tipo de actualización:",
                                  choices = c("Todos" = "Todos",
                                              "Hai pellets na praia" = "Hai pellets na praia",
                                              "Non hai pellets na praia" = "Non hai pellets na praia",
                                              "Xa non hai (a praia quedaba limpa cando se encheu o formulario)" = "Xa non hai (a praia quedaba limpa cando se encheu o formulario)",
                                              "Convocatoria de xornada de limpeza" = "Convocatoria de xornada de limpeza"),
                                  selected = "Todos"),
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
                        column(12, paste("Seguemento cidadán do estado das praias galegas despois do vertido de pellets. ")),
                        column(12, HTML(paste("Podes colaborar cubrindo o seguinte", "<a href='YOUR_FORM_URL' target='_blank'>formulario</a>", "cos datos das últimas recollidas"))),
                        column(12, leafletOutput("mymap"))  # Adjust the width of the map
                      ),
                      fluidRow(
                        column(12,
                               tags$footer(
                                 style = "text-align: center; padding: 10px; background-color: #f0f0f0;",
                                 "Author: ", tags$a("Inés Ortega Fernández", href = "https://inesortega.github.io/"),
                                 " | Contact: ", tags$a("ines.ortega@uvigo.gal", href = "mailto:ines.ortega@uvigo.gal")
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

  data <- reactive({

    invalidateLater(900000) # 15min
    # showNotification(paste("Cargando datos...", Sys.time()), duration = 60)
    # get_data()
    updateTimestamp$time <- Sys.time()

    all_data <- read.csv("praias.csv")
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

    filtered_data$id <- seq.int(nrow(filtered_data))

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

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -8.1, lat = 42.5, zoom = 7)
  })

  proxy_map <- leafletProxy("mymap")

  observe({
    # Check if the data is not empty
    if (!is.null(data()) && nrow(data()) > 0) {
      proxy_map <- leafletProxy("mymap")
      proxy_map %>% clearMarkers()

      # Determine the selected filter
      selected_filter <- input$legendFilter

      # Add markers based on the selected filter
      if (selected_filter == "Todos") {
        proxy_map %>%
          addCircleMarkers(
            data = data_hai_pellets(),
            lng = ~lon,
            lat = ~lat,
            label = ~Nome.da.praia..Concello,
            layerId = ~id,
            radius = 10,
            color = "#e31a1c",
            stroke = FALSE, fillOpacity = 0.5
          ) %>%
          addCircleMarkers(
            data = data_non_hai_pellets(),
            lng = ~lon,
            lat = ~lat,
            label = ~Nome.da.praia..Concello,
            layerId = ~id,
            radius = 10,
            color = "#33a02c",
            stroke = FALSE, fillOpacity = 0.5
          ) %>%
          addCircleMarkers(
            data = data_convocatoria(),
            lng = ~lon,
            lat = ~lat,
            label = ~Nome.da.praia..Concello,
            layerId = ~id,
            radius = 10,
            color = "#701796",
            stroke = FALSE, fillOpacity = 0.5
          ) %>%
          addCircleMarkers(
            data = data_xa_non_hai(),
            lng = ~lon,
            lat = ~lat,
            label = ~Nome.da.praia..Concello,
            layerId = ~id,
            radius = 10,
            color = "#1f78b4",
            stroke = FALSE, fillOpacity = 0.5
          )
      }
      else if (selected_filter == "Hai pellets na praia") {
        proxy_map %>%
          addCircleMarkers(
            data = data_hai_pellets(),
            lng = ~lon,
            lat = ~lat,
            label = ~Nome.da.praia..Concello,
            layerId = ~id,
            radius = 10,
            color = "#e31a1c",
            stroke = FALSE, fillOpacity = 0.5
          )
      } else if (selected_filter == "Non hai pellets na praia") {
        proxy_map %>%
          addCircleMarkers(
            data = data_non_hai_pellets(),
            lng = ~lon,
            lat = ~lat,
            label = ~Nome.da.praia..Concello,
            layerId = ~id,
            radius = 10,
            color = "#33a02c",
            stroke = FALSE, fillOpacity = 0.5
          )
      } else if (selected_filter == "Convocatoria de xornada de limpeza") {
        proxy_map %>%
          addCircleMarkers(
            data = data_convocatoria(),
            lng = ~lon,
            lat = ~lat,
            label = ~Nome.da.praia..Concello,
            layerId = ~id,
            radius = 10,
            color = "#701796",
            stroke = FALSE, fillOpacity = 0.5
          )
      } else if (selected_filter == "Xa non hai (a praia quedaba limpa cando se encheu o formulario)") {
        proxy_map %>%
          addCircleMarkers(
            data = data_xa_non_hai(),
            lng = ~lon,
            lat = ~lat,
            label = ~Nome.da.praia..Concello,
            layerId = ~id,
            radius = 10,
            color = "#1f78b4",
            stroke = FALSE, fillOpacity = 0.5
          )
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
      output$sidebarContent <- renderUI({
        sidebar <- fluidRow(
          HTML("<div style='padding-left: 20px;'>"),
          column(12, HTML(paste(
            "<strong>Praia: </strong>", info$Nome.da.praia..Concello, "<br>",
            "<strong>Data da limpeza: </strong>", info$Marca.temporal, "<br>",
            "<strong>Información adicional: </strong>", info$Información.adicional, "<br>",
            "<strong>Tipo de información: </strong>", info$Tipo.de.actualización.que.nos.queres.facer.chegar, "<br>",
            "<strong>Está avisado o 112? </strong>", info$Está.avisado.o.112., "<br>",
            "<strong>Quen está recollendo os pellets? </strong>", info$Quen.está.recollendo.os.pellets., "<br>",
            "<strong>Como estades a recoller? </strong>", info$Como.estades.a.recoller.os.pellets., "<br>",
            "<strong>Onde se depositan os pellets? </strong>", info$Onde.se.depositan.os.pellets., "<br>"))),
          column(12, h4("Datos GPS:")),
          column(12, HTML(paste(
            "<strong>Latitude: </strong>", info$lat, "<br>",
            "<strong>Lonxitude: </strong>", info$lon, "<br>",
            "<strong>Concello: </strong>", info$Concello, "<br>")
          )),
          if(!is.null(updateTimestamp$time)){
            column(12, h5(paste("Última  Actualización: ", updateTimestamp$time)))
          }
        )
        return(sidebar)
      })
    }
  })

  # Display the marker information in the sidebar
  output$markerInfoText <- renderPrint({
    markerInfo$clickedMarker
  })

}

shinyApp(ui, server)
