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
      column(12, paste("Seguemento cidadán do estado das praias galegas despois do vertido de pellets. ")),
      column(12, HTML(paste("Lembrar que todos os datos recollense de forma colaborativa por voluntarios a través deste ", "<a href='YOUR_FORM_URL' target='_blank'>formulario</a>"))),
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


  observe({
    invalidateLater(900000) # 15min
    showNotification(paste("Actualizando datos...", Sys.time()), duration = 60)
    get_data()
  })

  data <- reactive({
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
        labels = c("Hai pellets na praia", "Non hai pellets na praia", "Convocatoria de xornada de limpeza", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)"),
        title = "Tipo de información"
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
                  paste("<strong>Data da actualización: </strong>", Marca.temporal, "<br>"),
                  paste("<strong>Está avisado o 112?</strong>", Está.avisado.o.112., "<br>"),
                  paste("<strong>Hai animais mortos</strong>", Atopaches.animáis.mortos., "<br>"),
                  paste("<strong>Por onde están espallados: </strong>", Por.onde.están.espallados.os.pellets, "<br>"),
                  paste("<strong>Quen está recollendo os pellets?</strong>", Quen.está.recollendo.os.pellets., "<br>"),
                  paste("<strong>Como se están a recoller?</strong>", Como.estase.a.recoller.os.pellets., "<br>"),
                  paste("<strong>Onde se depositan?</strong>", Onde.se.depositan.os.pellets., "<br>"),
                  paste("<strong>Hai sacos de pellets?</strong>", Hai.sacos.de.pellets., "<br>"),
                  paste("<strong>Cantidade de pellets: </strong>", Cantidade.de.pellets, "<br>"),
                  paste("<strong>Concello: </strong>", Concello, "<br>")
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
                  paste("<strong>Data da actualización: </strong>", Marca.temporal, "<br>"),
                  paste("<strong>Hai animais mortos</strong>", Atopaches.animáis.mortos., "<br>")
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
                  paste("<strong>Data da convocatoria: </strong>", as.Date(Data), " ", Hora, "<br>"),
                  paste("<strong>Lugar de encontro: </strong>", Lugar.de.encontro, "<br>"),
                  paste("<strong>Quen organiza a iniciativa? </strong>", Quen.organiza.a.iniciativa, "<br>"),
                  paste(tags$a("Ligazón", href = Ligazón), "<br>")
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
                  paste("<strong>Data da limpeza: </strong>", Marca.temporal, "<br>"),
                  paste("<strong>Hai animais mortos</strong>", Atopaches.animáis.mortos., "<br>"),
                  paste("<strong>Por onde están espallados: </strong>", Por.onde.están.espallados.os.pellets, "<br>"),
                  paste("<strong>Quen está recollendo os pellets?</strong>", Quen.está.recollendo.os.pellets., "<br>"),
                  paste("<strong>Como se están a recoller?</strong>", Como.estase.a.recoller.os.pellets., "<br>"),
                  paste("<strong>Onde se depositan?</strong>", Onde.se.depositan.os.pellets., "<br>"),
                  paste("<strong>Hai sacos de pellets?</strong>", Hai.sacos.de.pellets., "<br>"),
                  paste("<strong>Cantidade de pellets: </strong>", Cantidade.de.pellets, "<br>"),
                  paste("<strong>Concello: </strong>", Concello, "<br>")
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
        output$sidebarContent <- renderUI({
          sidebar <- fluidRow(
            HTML("<div style='padding-left: 20px;'>"),
            column(12, h3("Información adicional:")),
            column(12, HTML(paste(
              "<strong>Praia: </strong>", info$Nome.da.praia..Concello, "<br>",
              "<strong>Data da limpeza: </strong>", info$Marca.temporal, "<br>",
              "<strong>Concello: </strong>", info$Concello, "<br>",
              "<strong>Información adicional: </strong>", info$Información.adicional, "<br>"))),
            column(12, h4("Datos GPS dispoñibles:")),
            column(12, HTML(paste(
              "<strong>Latitude: </strong>", info$lat, "<br>",
              "<strong>Lonxitude: </strong>", info$lon, "<br>")
            )),
            if(!is.null(updateTimestamp$time)){
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
              "<strong>Data e hora: </strong>", info$Data," ", info$Hora, "<br>",
              "<strong>Lugar de encontro: </strong>", info$Lugar.de.encontro, "<br>",
              "<strong>Quen organiza a iniciativa: </strong>", info$Quen.organiza.a.iniciativa, "<br>",
              "<strong>Contacto: </strong>", info$Contacto, "<br>",
              tags$a("Ligazón", href = info$Ligazón), "<br>"))),
            column(12, h4("Datos GPS dispoñibles:")),
            column(12, HTML(paste(
              "<strong>Latitude: </strong>", info$lat, "<br>",
              "<strong>Lonxitude: </strong>", info$lon, "<br>")
            )),
            if(!is.null(updateTimestamp$time)){
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

shinyApp(ui, server)
