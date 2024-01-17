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
library(ggplot2)
library(shinycssloaders)

source("data_loader.R")

sidebar <- dashboardSidebar(
  width = 400,
  tags$head(tags$style(HTML(".sidebar { height: auto; overflow-y}" ))),
  tags$style(HTML(".sidebar-menu { margin-bottom: 20px; }")),  # Add margin to sidebar-menu
  sidebarMenu(id = "sidebarID",
    menuItem("Mapa en tempo real", tabName = "map"),
    menuItem("Datos e estadísticas", tabName = "info", badgeLabel = "Novo!", badgeColor = "green"),
    #menuItem("Formulario de recollida de datos", icon = icon("info"), newtab = TRUE, href = "https://docs.google.com/forms/u/1/d/e/1FAIpQLScHqNH3yxk5yBKhOMZ0mVk0Wl-bNLCowqW9UFr0mo2Hj7klGA/formResponse"),
    menuItem("Filtrado de información", tabName = "filter",
             tags$style(HTML(".sidebar-menu .treeview-menu { padding-top: 5px; padding-bottom: 5px }")),  # Add margin to shiny-input-container
             sliderInput(
               inputId = "dateRange",
               label = "Filtrado por data:",
               min = as.Date("2024-01-06"),
               max = Sys.Date() + 2,
               value = c(Sys.Date() - 2, Sys.Date() + 1)
             ),
             pickerInput("legendFilter",
                         "Filtrar por tipo de actualización:",
                         choices = c(
                           "Hai pellets na praia" = "Hai pellets na praia",
                           "Non hai pellets na praia" = "Non hai pellets na praia",
                           "Xa non hai (a praia quedaba limpa cando se encheu o formulario)" = "Xa non hai (a praia quedaba limpa cando se encheu o formulario)",
                           "Convocatoria de xornada de limpeza" = "Convocatoria de xornada de limpeza",
                           "Outras Convocatorias" = "Outras Convocatorias"
                         ), multiple = TRUE,
                         selected = c(
                           "Hai pellets na praia",
                           "Non hai pellets na praia",
                           "Xa non hai (a praia quedaba limpa cando se encheu o formulario)",
                           "Convocatoria de xornada de limpeza",
                           "Outras Convocatorias"
                         )),
             selectInput(inputId = "select_concello", label = "Concello", multiple = TRUE, choices=c(), selected="")
             ),
    menuItem("Información adicional", tabName = "info_add", startExpanded = TRUE,
             uiOutput("sidebarContent")),
    menuItem("Agradecementos", tabName = "ack")
    )
)
body <- dashboardBody(
  tags$head(tags$style(HTML('.skin-blue .main-header .logo {background-color: #3c8dbc;} .skin-blue .main-header .logo:hover {background-color: #3c8dbc;}'))),
  tags$style(HTML(".leaflet { height: calc(100vh - 250px) !important; }")),
  tags$style(HTML(".treeview-menu>li>a { padding-left: 10px; }")),  # Add padding to sidebarMenu icons
  tags$style(HTML(".box-header { font-size: 15px; }")),
  tabItems(
    tabItem(tabName = "ack",
            column(12,
                   HTML(paste("Este proxecto é posible grazas a colaboración e recollida de datos de voluntarias; de Ana e Miguel das Chas; das voluntarias de Noia Limpa; e a cesión de recursos por parte de ", "<a href='https://gradiant.org' target='_blank'>Gradiant</a>"))
                   )
            ),
    tabItem(tabName = "map",
        column(12, h4("Mapa actualizado da situación das praias galegas")),
        #column(12, paste("Seguemento cidadán do estado das praias galegas despois do vertido de pellets.")),
        column(12, paste("O mapa reflexa as actualizacións do estado das praias enviadas polos voluntarios. Polo momento, non ofrece información agregada do número de praias afectadas.")),
        column(12, leafletOutput("mymap"))
    ),
    tabItem(tabName = "info",
            tags$style(HTML(".box-header { font-size: 10px; }")),  # Adjust the font size as needed
        column(12, h4("Datos e estadísticas")),
        column(12, HTML(paste("Todos os datos recóllense de xeito colaborativo por voluntarios e entidades colaboradoras a través deste ", "<a href='https://docs.google.com/forms/u/1/d/e/1FAIpQLScHqNH3yxk5yBKhOMZ0mVk0Wl-bNLCowqW9UFr0mo2Hj7klGA/formResponse' target='_blank'>formulario</a>."))),
        column(12, HTML(paste("Os seguintes datos e estadísticas calcúlanse de forma automática - para os filtros seleccionados no panel lateral - e poden contar erros debido ao reporte non estandarizado do nome da praia por parte dos usuarios do formulario."))),
        column(12, HTML("<br>")),
        fluidRow(
          column(12,
            box(status = "primary", HTML("<div style='font-size:15px'>Total Concellos</div>"),
                withSpinner(valueBoxOutput("n_concellos")),
                width = 3),
            box(status = "primary", HTML("<div style='font-size:15px'>Total praias</div>"), withSpinner(valueBoxOutput("n_praias")), width = 3),
            box(status = "primary", HTML("<div style='font-size:15px'>Animais mortos</div>"),
                withSpinner(valueBoxOutput("n_animais")),
                width = 3),
            box(status = "primary", HTML("<div style='font-size:15px'>Notificacións 112</div>"),
                withSpinner(valueBoxOutput("n_112")),
                width = 3)
          )
        ),
        fluidRow(
          column(12,
                 box(status = "primary", HTML("<div style='font-size:15px'>Con Pellets</div>"), withSpinner(valueBoxOutput("n_pellets")), width = 3),
                 box(status = "primary", HTML("<div style='font-size:15px'>Sen Pellets</div>"), withSpinner(valueBoxOutput("non_pellets")), width = 3),
                 box(status = "primary", HTML("<div style='font-size:15px'>Xa non hai</div>"), withSpinner(valueBoxOutput("n_limpas")), width = 3),
                 box(status = "primary", HTML("<div style='font-size:15px'>Limpezas realizadas</div>"), withSpinner(valueBoxOutput("n_limpezas")), width = 3)
          )
        ),
        fluidRow(
          column(12,
                 box(status = "primary",
                     HTML("<div style='font-size:15px'>Evolución diaria - Actualizacións Recibidas </div>"),
                     withSpinner(plotOutput("cum_praias")), width = 6),
                 box(status = "primary",  HTML("<div style='font-size:15px'>Top 5 Concellos</div>"), withSpinner(plotOutput("top5_concellos")), width = 6)
                 )
        )
    )
  ),
  useShinyjs()
)

ui <- tagList(dashboardPage(skin = "black",
      dashboardHeader(
        title = "Unha Vez Máis",
        titleWidth = 400
      ),
      sidebar = sidebar,
      body = body
  ),
  tags$footer(
    style = "text-align: center; padding: 10px; background-color: #fff; height:auto",
    "Author: ", tags$a("Inés Ortega Fernández", href = "https://inesortega.github.io/"),
    " | Contact: ", tags$a("datospellets@gmail.com", href = "datospellets@gmail.com")
  )
)
server <- function(input, output, session) {

  updateTimestamp <- reactiveValues(time = Sys.time())
  last_run_time <- reactiveValues(time = Sys.time())
  isFirstRun <- reactiveVal(TRUE)

  # Create a reactiveValu object to store the clicked marker information
  clickedMarker <- reactiveVal(NULL)

  data <- reactive({
    invalidateLater(350000)
    tryCatch({
      all_data <- read_csv("./data/praias.csv", show_col_types = FALSE)
      all_data <- all_data %>%
        mutate(
          Data.Norm = case_when(
            Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("Convocatoria de xornada de limpeza", "Outras Convocatorias") ~ as.Date(Data, "%Y-%m-%dd", tz="UTC"),
            TRUE ~ as.Date(Marca.temporal, "%Y-%m-%dd", tz="UTC"))  # Default case
          )

      all_data <- all_data %>%
        mutate(Nome.da.praia..Concello = ifelse(str_detect(Nome.da.praia..Concello, ","),
                                                Nome.da.praia..Concello,
                                                paste(Nome.da.praia..Concello, Concello, sep = ', ')))


      all_data$Data.Norm <- as.Date(all_data$Data.Norm)
      all_data$id <- seq.int(nrow(all_data))
    }, error = function(e){
      message("Error loading data...")
    })

    # Filter data based on selected date range
    max <- max(as.Date(all_data$Data.Norm))
    min <- min(as.Date(all_data$Data.Norm))
    updateSliderInput(session, inputId = "dateRange", min=min, max=max)

    if (!is.null(input$dateRange)) {
      start_date <- input$dateRange[1]
      end_date <- input$dateRange[2]
      filtered_data <- all_data %>%
        filter(Data.Norm >= start_date & Data.Norm <= end_date)
    } else {
      filtered_data <- all_data
    }

    # Filter data based on selected date range
    updateSelectInput(session,"select_concello",choices=unique(all_data$Concello), select = input$select_concello)

    if(!is.null(input$select_concello)){
      filtered_data <- filtered_data %>% filter(Concello %in% input$select_concello)
    }

    filtered_data <- filtered_data %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% input$legendFilter)

    filtered_data
  })

  output$n_concellos <- renderValueBox({
    distinct_counts <- data() %>%
      count(Concello)
    n_concellos <- length(distinct_counts$Concello)
    customValueBox(n_concellos)
  })
  output$n_praias <- renderValueBox({
    praias_elem = c("Hai pellets na praia","Non hai pellets na praia","Xa non hai (a praia quedaba limpa cando se encheu o formulario)")
    distinct_counts <- data() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% praias_elem) %>%
      count(Nome.da.praia..Concello, Concello)
    n_praias <- length(distinct_counts$Nome.da.praia..Concello)
    customValueBox(n_praias)
  })
  output$n_animais <- renderValueBox({
    distinct_counts <- data() %>%
      count(Atopaches.animáis.mortos.) %>% filter(Atopaches.animáis.mortos. == "Si")
    customValueBox(distinct_counts)
  })
  output$n_112 <- renderValueBox({
    distinct_counts <- data() %>%
      count(Está.avisado.o.112.) %>% filter(Está.avisado.o.112. == "Si")
    customValueBox(distinct_counts)
  })

  reconto <- reactive({
    distinct_counts <- data() %>% count(Tipo.de.actualización.que.nos.queres.facer.chegar)
  })

  output$n_pellets <- renderValueBox({
    count <- reconto() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Hai pellets na praia")
    customValueBox(count)
  })

  output$non_pellets <- renderValueBox({
    count <- reconto() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Non hai pellets na praia")
    customValueBox(count)
  })

  output$n_limpas <- renderValueBox({
    count <- reconto() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Xa non hai (a praia quedaba limpa cando se encheu o formulario)")
    customValueBox(count)
  })

  output$n_limpezas <- renderValueBox({
    count <- reconto() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Convocatoria de xornada de limpeza")
    customValueBox(count)
  })

  output$top5_concellos <- renderPlot({
    top5 <- data() %>%
      count(Concello) %>%
      arrange(desc(n)) %>%
      head(5)
    ggplot(top5, aes(x = factor(Concello), y = n, fill = n)) +
      geom_col(stat = "n",  show.legend = FALSE) +
      geom_text(aes(label = Concello, y = n), position = position_stack(vjust = 0.5), color = "white") +
      scale_fill_gradient(low = "#428BCA", high = "#428BCA") +
      labs(x = NULL, y = NULL) + coord_flip() +
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.5),
        axis.text.x = element_text(color = "black", size = 8),
        legend.position = "none",
        axis.ticks.length = unit(0, "null"),
        axis.ticks.margin = unit(0, "null"),
        legend.margin = unit(0, "null")
      )
  })

  output$cum_praias <- renderPlot({
    distinct_counts <- data() %>%
      count(Nome.da.praia..Concello, Concello, Data.Norm)

    summarized_data <- distinct_counts %>%
      group_by(Data.Norm) %>%
      summarize(sum_n = sum(n))

    ggplot(summarized_data, aes(x = Data.Norm, y = sum_n)) +
      geom_line(linewidth = 1, lineend = "round", linejoin = "mitre", colour = "#428BCA") +
      labs(x = "Data", y = "Número de Praias afectadas")
  })

  # Update data and sidebar when date filter is changed
  observeEvent(input$dateRange, {
    # Check if the clicked marker is still valid based on the filtered data
    if (!is.null(clickedMarker())){
      if(nrow(subset(data(), id == clickedMarker()$id)) == 0){
        # not valid
        clickedMarker(NULL)
        output$sidebarContent <- NULL
      }
    }
  })

  # Update the clicked marker information when a marker is clicked
  observeEvent(input$mymap_marker_click, {
    marker_info <- input$mymap_marker_click
    clickedMarker(marker_info)
  })

  # Update sidebar content if valid marker is clicked
  observe({
    if(!is.null(clickedMarker())){
      marker_id <- clickedMarker()$id
      info <- data() %>%
        filter(id == clickedMarker()$id)
      #### add content to sidebar
      if(nrow(info) > 0){
        tipo <- info$Tipo.de.actualización.que.nos.queres.facer.chegar
        if(!(tipo %in% c("Convocatoria de xornada de limpeza", "Outras Convocatorias"))){

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
              tags$style(HTML(".skin-black .sidebar-menu>li>.treeview-menu {padding-left: 15px;}")),  # Add margin to shiny-input-container
              column(12, HTML(paste(
                "<br><strong>Praia: </strong>", htmlEscape(info$Nome.da.praia..Concello), "<br>",
                "<strong>Data da limpeza: </strong>", htmlEscape(info$Marca.temporal), "<br>",
                "<strong>Concello: </strong>", htmlEscape(info$Concello), "<br>",
                "<strong>Información adicional: </strong><div>", htmlEscape(info$Información.adicional), "</div><br>"
              ))),
              column(12, h5("Imaxes dispoñibles:")),
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
              column(12, HTML(paste(
                "<br><strong>Tipo: </strong>", htmlEscape(info$Tipo.de.actualización.que.nos.queres.facer.chegar), "<br>",
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

    }
  })

  # Create a reactive object for each type
  data_hai_pellets <- reactive({
    data() %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Hai pellets na praia")
  })

  data_non_hai_pellets <- reactive({
    data() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Non hai pellets na praia")
  })

  data_convocatoria <- reactive({
    data() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Convocatoria de xornada de limpeza")
  })

  data_evento <- reactive({
    data() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Outras Convocatorias")
  })

  data_xa_non_hai <- reactive({
    data() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Xa non hai (a praia quedaba limpa cando se encheu o formulario)")
  })

  color_factor <- reactive({
    colorFactor(palette = color_palette, na.color = "grey",
                levels = c("Hai pellets na praia", "Non hai pellets na praia", "Convocatoria de xornada de limpeza", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)", "Outras Convocatorias"))
  })

  # Render map
  proxy_map <- leafletProxy("mymap")

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -8.1, lat = 42.5, zoom = 7) %>%
      addLegend(
        position = "bottomright",
        colors = c("#e31a1c", "#33a02c", "#F5A618", "#490a73","#1f78b4"),  # Colors for the four categories
        labels = c(paste("Hai pellets na praia (", nrow(data_hai_pellets()), ")", sep = ""),
                   paste("Non hai pellets na praia (", nrow(data_non_hai_pellets()), ")", sep = ""),
                   paste("Convocatoria de xornada de limpeza (", nrow(data_convocatoria()), ")", sep = ""),
                   paste("Outras Convocatorias (", nrow(data_evento()), ")", sep = ""),
                   paste("Xa non hai (quedou limpa), (", nrow(data_xa_non_hai()), ")", sep = "")),
        title = "Tipo de actualización e reconto por tipo"
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
      for (selected_filter in selected_filters) {
        if (selected_filter == "Hai pellets na praia" & nrow(data_hai_pellets()) > 0) {
          proxy_map %>%
            addCircleMarkers(
              data = data() %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Hai pellets na praia"),
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
              data = data() %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Non hai pellets na praia"),
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
              data = data() %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Convocatoria de xornada de limpeza"),
              lng = ~lon,
              lat = ~lat,
              label = ~Nome.da.praia..Concello,
              layerId = ~id,
              radius = 10,
              color = "#F5A618",
              popup = ~paste(
                paste("<strong>Data da convocatoria: </strong>", htmlEscape(as.Date(Data)), " ", htmlEscape(Hora), "<br>"),
                paste("<strong>Praia: </strong>", htmlEscape(Nome.da.praia..Concello), "<br>"),
                paste("<strong>Lugar de encontro: </strong>", htmlEscape(Lugar.de.encontro), "<br>"),
                paste("<strong>Quen organiza a iniciativa? </strong>", htmlEscape(Quen.organiza.a.iniciativa), "<br>"),
                paste(tags$a("Ligazón", href = htmlEscape(Ligazón)), "<br>")
              ),
              stroke = FALSE, fillOpacity = 0.5
            )
        } else if (selected_filter == "Outras Convocatorias" & nrow(data_evento()) > 0) {
          proxy_map %>%
            addCircleMarkers(
              data = data() %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Outras Convocatorias"),
              lng = ~lon,
              lat = ~lat,
              label = ~Nome.da.praia..Concello,
              layerId = ~id,
              radius = 10,
              color = "#490a73",
              popup = ~paste(
                paste("<strong>Data da convocatoria: </strong>", htmlEscape(as.Date(Data)), " ", htmlEscape(Hora), "<br>"),
                paste("<strong>Lugar de encontro: </strong>", htmlEscape(Lugar.de.encontro), "<br>"),
                paste("<strong>Quen organiza a iniciativa? </strong>", htmlEscape(Quen.organiza.a.iniciativa), "<br>"),
                paste(tags$a("Ligazón", href = htmlEscape(Ligazón)), "<br>"),
                paste(tags$a("Cartaz", href = htmlEscape(Cartaz)), "<br>")
              ),
              stroke = FALSE, fillOpacity = 0.5
            )
        } else if (selected_filter == "Xa non hai (a praia quedaba limpa cando se encheu o formulario)" & nrow(data_xa_non_hai()) > 0) {
          proxy_map %>%
            addCircleMarkers(
              data = data() %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Xa non hai (a praia quedaba limpa cando se encheu o formulario)"),
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

}


# Custom valueBox function
customValueBox <- function(value) {
  if(class(value)[1] == "tbl_df"){
    if(nrow(value) == 0){
      value = 0
    }
    else{
      value = value$n
    }
  }
  tags$div(
    style = "background-color: #f5f5f5; padding: 10px",
    tags$p(
      style = "margin: 0;",
      tags$strong(
        style = "font-size: 35px; color: black; text-align: center;",
        value
      )
    )
  )
}


if(Sys.getenv("ENV") == "docker"){
  options(shiny.host = '0.0.0.0')
  options(shiny.port = 3838)
}

shinyApp(ui, server)
