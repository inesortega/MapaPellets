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
               min = as.Date("2024-01-01"),
               max = Sys.Date() + 2,
               value = c(Sys.Date() - 7, Sys.Date() + 1)
             ),
             pickerInput("legendFilter",
                         "Filtrar por tipo de actualización:",
                         choices = c(
                           "Pellets" = "Hai pellets na praia",
                           "Biosoportes" = "Hai biosoportes",
                           "Chapapote" = "Hai chapapote",
                           "Praia limpa" = "A praia está limpa",
                           "Xa non hai (a praia quedaba limpa)" = "Xa non hai (a praia quedaba limpa cando se encheu o formulario)",
                           "Convocatoria de limpeza" = "Convocatoria de xornada de limpeza",
                           "Outras Convocatorias" = "Outras Convocatorias"
                         ), multiple = TRUE,
                         selected = c(
                           "Hai biosoportes",
                           "Hai chapapote",
                           "A praia está limpa",
                           "Xa non hai (a praia quedaba limpa cando se encheu o formulario)",
                           "Convocatoria de xornada de limpeza",
                           "Outras Convocatorias"
                         )),
             selectInput(inputId = "select_provincia", label = "Provincia", multiple = TRUE, choices=c(), selected=""),
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
        column(12, h4("Mapa en tempo real da situación das praias galegas")),
        #column(12, paste("Seguemento cidadán do estado das praias galegas despois do vertido de pellets.")),
        column(12, paste("O mapa reflexa as actualizacións do estado das praias enviadas polos voluntarios. Podes atopar información estadística sobre as praias afectadas no menú Datos e estadísticas do panel lateral.")),
        column(12, paste("Emprega o panel lateral para filtrar a información por tipo de vertido, concello, provincia ou data de actualización. Por defecto, amósase a información da última semana. ")),
        column(12, HTML("<br>")),
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
            box(status = "primary", HTML("<div style='font-size:15px'>Num. Concellos</div>"), HTML("<div style='font-size:10px'>con actualizacións recibidas</div>"),
                withSpinner(valueBoxOutput("n_concellos")),
                width = 3),
            box(status = "primary", HTML("<div style='font-size:15px'>Num. Praias</div>"), HTML("<div style='font-size:10px'>con actualizacións recibidas</div>"),
                withSpinner(valueBoxOutput("n_praias")), width = 3),
            box(status = "primary", HTML("<div style='font-size:15px'>Total actualizacións</div>"), HTML("<div style='font-size:10px'>recibidas no formulario</div>"),
                withSpinner(valueBoxOutput("n_update")),
                width = 3),
            box(status = "primary", HTML("<div style='font-size:15px'>Notificacións 112</div>"), HTML("<div style='font-size:10px'>Total actualizacións recibidas</div>"),
                withSpinner(valueBoxOutput("n_112")),
                width = 3)
          )
        ),
        #column(12, h4("Reconto por tipo de actualización:")),
        fluidRow(
          column(12,
                 box(status = "primary", HTML("<div style='font-size:15px'>Praias con Residuos</div>"), HTML("<div style='font-size:10px'>Total actualizacións recibidas</div>"),
                     withSpinner(valueBoxOutput("n_pellets")), width = 4),
                 box(status = "primary", HTML("<div style='font-size:15px'>Praia limpa</div>"), HTML("<div style='font-size:10px'>Total actualizacións recibidas</div>"),
                     withSpinner(valueBoxOutput("n_limpas")), width = 4),
                 box(status = "primary", HTML("<div style='font-size:15px'>Outros eventos</div>"), HTML("<div style='font-size:10px'>Total actualizacións recibidas</div>"),
                     withSpinner(valueBoxOutput("n_limpezas")), width = 4)
          )
        ),
        fluidRow(
          column(12,
                 box(status = "primary",
                     HTML("<div style='font-size:15px'>Evolución diaria - Actualizacións recibidas por tipo de residuo </div>"),
                     withSpinner(plotOutput("cum_residuos")), width = 12)
                 #box(status = "primary",  HTML("<div style='font-size:15px'>Top 5 Concellos</div>"), withSpinner(plotOutput("top5_concellos")), width = 6)
          )
        ),
        fluidRow(
          column(12,
                 box(status = "primary",
                     HTML("<div style='font-size:15px'>Evolución diaria - Actualizacións Recibidas </div>"),
                     withSpinner(plotOutput("cum_praias")), width = 12)
                 #box(status = "primary",  HTML("<div style='font-size:15px'>Top 5 Concellos</div>"), withSpinner(plotOutput("top5_concellos")), width = 6)
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


  ########### REACTIVE ELEMENTS ##############
  # Create a reactiveValu object to store the clicked marker information
  clickedMarker <- reactiveVal(NULL)

  data <- reactive({
    invalidateLater(350000)
    tryCatch({
      all_data <- read_csv("./data/praias.csv", show_col_types = FALSE)
      all_data <- all_data %>%
        mutate(
          Data.Norm = case_when(
            Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("Convocatoria de xornada de limpeza", "Outras Convocatorias") ~ as.Date(Data.da.Convocatoria, "%Y-%m-%dd", tz="UTC"),
            TRUE ~ as.Date(Marca.temporal, "%Y-%m-%dd", tz="UTC"))  # Default case
        ) %>%
        mutate(
          Residuo = case_when(
            Tipo.de.actualización.que.nos.queres.facer.chegar  == "Hai pellets na praia" ~ "Pellets",
            Tipo.de.actualización.que.nos.queres.facer.chegar  == "Hai biosoportes" ~ "Biosoportes",
            Tipo.de.actualización.que.nos.queres.facer.chegar  == "Hai chapapote" ~ "Chapapote")  # Default case
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

    # Filter data based on selected provincia
    updateSelectInput(session,"select_provincia",choices=setdiff(unique(filtered_data$Provincia), "FALSE"), select = input$select_provincia)

    if(!is.null(input$select_provincia)){
      filtered_data <- filtered_data %>% filter(Provincia %in% input$select_provincia)
    }

    # Filter data based on selected concello
    updateSelectInput(session,"select_concello",choices=unique(filtered_data$Concello), select = input$select_concello)

    if(!is.null(input$select_concello)){
      filtered_data <- filtered_data %>% filter(Concello %in% input$select_concello)
    }

    filtered_data <- filtered_data %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% input$legendFilter)

    filtered_data
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

          if(tipo  %in% c("Hai pellets na praia", "Hai chapapote", "Hai biosoportes")){
            if(!is.na(info$Imaxe.dos.residuos.no.lugar.ou.da.xornada.de.limpeza)){
              links <- sapply(info$Imaxe.dos.residuos.no.lugar.ou.da.xornada.de.limpeza, function(x) strsplit(x, ", "), USE.NAMES=FALSE)[[1]]
            }
          }
          else if(tipo %in% c("Non hai pellets", "A praia está limpa", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)")){
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
                "<strong>Data e hora: </strong>", htmlEscape(info$Data.da.Convocatoria), " ", htmlEscape(info$Hora), "<br>",
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

  ###### RENDER Statisticsc ################
  output$n_concellos <- renderValueBox({
    distinct_counts <- data() %>%
      count(Concello)
    n_concellos <- length(unique(distinct_counts$Concello))
    customValueBox(n_concellos)
  })

  output$n_praias <- renderValueBox({
    praias_tipos <- c("Hai pellets na praia", "Hai chapapote", "Hai biosoportes", "Non hai pellets na praia", "A praia está limpa", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)")
    distinct_counts <- data() %>%
      count(Nome.da.praia..Concello, Concello, Tipo.de.actualización.que.nos.queres.facer.chegar) %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% praias_tipos)
    customValueBox(length(unique(distinct_counts$Nome.da.praia..Concello)))

  })

  output$n_update <- renderValueBox({
    customValueBox(nrow(data()))
  })

  # output$n_animais <- renderValueBox({
  #   distinct_counts <- data() %>%
  #     count(Atopaches.animáis.mortos.) %>% filter(Atopaches.animáis.mortos. == "Si")
  #   customValueBox(distinct_counts)
  # })

  output$n_112 <- renderValueBox({
    distinct_counts <- data() %>%
      count(Está.avisado.o.112.) %>% filter(Está.avisado.o.112. == "Si")
    customValueBox(distinct_counts)
  })

  reconto <- reactive({
    distinct_counts <- data() %>% count(Nome.da.praia..Concello, Concello, Tipo.de.actualización.que.nos.queres.facer.chegar)
  })

  output$n_pellets <- renderValueBox({
    count <- reconto() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("Hai pellets na praia", "Hai chapapote", "Hai biosoportes"))
    customValueBox(sum(count$n))
  })

  output$n_limpas <- renderValueBox({
    count <- reconto() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("A praia está limpa", "Non hai pellets na praia", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)"))
    customValueBox(sum(count$n))
  })

  output$n_limpezas <- renderValueBox({
    count <- reconto() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("Convocatoria de xornada de limpeza", "Outras Convocatorias"))
    customValueBox(sum(count$n))
  })

  output$top5 <- renderPlot({
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
        #panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.ticks = element_blank(),
        axis.text = element_blank(),
        #axis.title = element_blank(),
        axis.line = element_blank(),
        axis.line.x = element_line(color = "black", size = 1),
        axis.text.x = element_text(color = "black", size = 10),
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
      labs(x = "Data", y = "Número de Actualizacións") +
      theme(
        axis.text.x = element_text(color = "black", size = 10)
      )
  })



  output$cum_residuos <- renderPlot({
    distinct_counts <- data() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("Hai pellets na praia", "Hai chapapote", "Hai biosoportes")) %>%
      count(Residuo, Data.Norm)

    # Group by both Data.Norm and Residuo to calculate sums per Residuo type
    summarized_data <- distinct_counts %>%
      group_by(Data.Norm, Residuo) %>%
      summarize(sum_n = sum(n), .groups = 'drop')

    # Plot each Residuo as a separate line
    ggplot(summarized_data, aes(x = Data.Norm, y = sum_n, color = Residuo, group = Residuo)) +
      geom_line(linewidth = 1, lineend = "round", linejoin = "mitre") +
      labs(x = "Data", y = "Número de Actualizacións", color = "Residuo") +
      theme(
        axis.text.x = element_text(color = "black", size = 10),
        legend.position = "bottom" # Optional: adjust the legend position
      )
  })

  ###### RENDER MAP ################
  # Create a reactive object for each type
  data_hai_pellets <- reactive({
    data() %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("Hai pellets na praia", "Hai chapapote", "Hai biosoportes"))
  })

  data_non_hai_pellets <- reactive({
    data() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("Non hai pellets na praia", "A praia está limpa", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)"))
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
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("A praia está limpa", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)"))
  })

  color_factor <- reactive({
    colorFactor(palette = color_palette, na.color = "grey",
                levels = c("Hai pellets na praia", "Non hai pellets na praia", "Convocatoria de xornada de limpeza", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)", "Outras Convocatorias"))
  })

  # Render map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -8.1, lat = 42.5, zoom = 7) %>%
      addLegend(
        position = "bottomright",
        colors = c("#e31a1c", "#33a02c", "#F5A618", "#490a73"),  # Colors for the four categories
        labels = c(paste("Hai residuos na praia (", nrow(data_hai_pellets()), ")", sep = ""),
                   paste("Non hai residuos na praia (", nrow(data_non_hai_pellets()), ")", sep = ""),
                   paste("Convocatoria de xornada de limpeza (", nrow(data_convocatoria()), ")", sep = ""),
                   paste("Outras Convocatorias (", nrow(data_evento()), ")", sep = "")),
        title = "Tipo de actualización e reconto por tipo"
      )
  })

  observe({
    req(input$sidebarID == "map") # Only display if tab is 'map'
    proxy_map <- leafletProxy("mymap")
    # Check if the data is not empty
    if (!is.null(data()) && nrow(data()) > 0) {
      proxy_map %>% clearMarkers()
      # Determine the selected filters
      selected_filters <- input$legendFilter

      # Add markers based on the selected filters
      for (selected_filter in selected_filters) {
        if ((selected_filter %in% c("Hai pellets na praia", "Hai biosoportes", "Hai chapapote")) & nrow(data_hai_pellets()) > 0) {
          proxy_map %>%
            addCircleMarkers(
              data = data() %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("Hai pellets na praia", "Hai chapapote", "Hai biosoportes")),
              lng = ~lon,
              lat = ~lat,
              label = ~paste(Residuo, " - ", Nome.da.praia..Concello),
              layerId = ~id,
              radius = 10,
              color = "#e31a1c",
              popup = ~paste(
                paste("<strong>Praia: </strong>", htmlEscape(Nome.da.praia..Concello), "<br>"),
                paste("<strong>Tipo de residuo: </strong>", htmlEscape(Residuo), "<br>"),
                paste("<strong>Data da actualización: </strong>", htmlEscape(Marca.temporal), "<br>"),
                paste("<strong>Está avisado o 112?</strong>", htmlEscape(Está.avisado.o.112.), "<br>"),
                paste("<strong>Hai animais mortos</strong>", htmlEscape(Atopaches.animáis.mortos.), "<br>"),
                paste("<strong>Por onde están espallados os residuos: </strong>", htmlEscape(Por.onde.están.espallados.os.residuos), "<br>"),
                paste("<strong>Quen está recollendo os residuos?</strong>", htmlEscape(Quen.está.recollendo.os.residuos.), "<br>"),
                paste("<strong>Como se están a recoller?</strong>", htmlEscape(Como.se.están.a.recoller.os.residuos.), "<br>"),
                paste("<strong>Onde se depositan?</strong>", htmlEscape(Onde.se.depositan.os.residuos.), "<br>"),
                paste("<strong>Hai sacos de pellets ou outro tipo de residuos?</strong>", htmlEscape(Hai.sacos.de.pellets.ou.outro.tipo.de.residuos.), "<br>"),
                paste("<strong>Cantidade de residuos: </strong>", htmlEscape(Cantidade.de.residuos), "<br>"),
                paste("<strong>Concello: </strong>", htmlEscape(Concello), "<br>")
              ),
              stroke = FALSE, fillOpacity = 0.5
            )

        }
        else if (selected_filter == "Convocatoria de xornada de limpeza" & nrow(data_convocatoria()) > 0) {
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
                paste("<strong>Data da convocatoria: </strong>", htmlEscape(as.Date(Data.da.Convocatoria)), " ", htmlEscape(Hora), "<br>"),
                paste("<strong>Praia: </strong>", htmlEscape(Nome.da.praia..Concello), "<br>"),
                paste("<strong>Lugar de encontro: </strong>", htmlEscape(Lugar.de.encontro), "<br>"),
                paste("<strong>Quen organiza a iniciativa? </strong>", htmlEscape(Quen.organiza.a.iniciativa), "<br>"),
                paste(tags$a("Ligazón", href = htmlEscape(Ligazón)), "<br>")
              ),
              stroke = FALSE, fillOpacity = 0.5
            )
        }
        else if (selected_filter == "Outras Convocatorias" & nrow(data_evento()) > 0) {
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
                paste("<strong>Data da convocatoria: </strong>", htmlEscape(as.Date(Data.da.Convocatoria)), " ", htmlEscape(Hora), "<br>"),
                paste("<strong>Lugar de encontro: </strong>", htmlEscape(Lugar.de.encontro), "<br>"),
                paste("<strong>Quen organiza a iniciativa? </strong>", htmlEscape(Quen.organiza.a.iniciativa), "<br>"),
                paste(tags$a("Ligazón", href = htmlEscape(Ligazón)), "<br>"),
                paste(tags$a("Cartaz", href = htmlEscape(Cartaz)), "<br>")
              ),
              stroke = FALSE, fillOpacity = 0.5
            )
        }
        else if (selected_filter %in% c("Non hai pellets na praia", "A praia está limpa", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)") & nrow(data_xa_non_hai()) > 0) {
          proxy_map %>%
            addCircleMarkers(
              data = data() %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("Non hai pellets na praia", "A praia está limpa", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)")),
              lng = ~lon,
              lat = ~lat,
              label = ~paste(Tipo.de.actualización.que.nos.queres.facer.chegar, " - ", Nome.da.praia..Concello),
              layerId = ~id,
              radius = 10,
              popup = ~paste(
                paste("<strong>Praia: </strong>", htmlEscape(Nome.da.praia..Concello), "<br>"),
                paste("<strong>Estado da praia: </strong>", htmlEscape(Tipo.de.actualización.que.nos.queres.facer.chegar), "<br>"),
                paste("<strong>Data da actualización: </strong>", htmlEscape(Marca.temporal), "<br>"),
                paste("<strong>Está avisado o 112?</strong>", htmlEscape(Está.avisado.o.112.), "<br>"),
                paste("<strong>Hai animais mortos</strong>", htmlEscape(Atopaches.animáis.mortos.), "<br>"),
                paste("<strong>Por onde están espallados os residuos: </strong>", htmlEscape(Por.onde.están.espallados.os.residuos), "<br>"),
                paste("<strong>Quen está recollendo os residuos?</strong>", htmlEscape(Quen.está.recollendo.os.residuos.), "<br>"),
                paste("<strong>Como se están a recoller?</strong>", htmlEscape(Como.se.están.a.recoller.os.residuos.), "<br>"),
                paste("<strong>Onde se depositan?</strong>", htmlEscape(Onde.se.depositan.os.residuos.), "<br>"),
                paste("<strong>Hai sacos de pellets ou outro tipo de residuos?</strong>", htmlEscape(Hai.sacos.de.pellets.ou.outro.tipo.de.residuos.), "<br>"),
                paste("<strong>Cantidade de residuos: </strong>", htmlEscape(Cantidade.de.residuos), "<br>"),
                paste("<strong>Concello: </strong>", htmlEscape(Concello), "<br>")
              ),
              color = "#33a02c",
              stroke = FALSE, fillOpacity = 0.5
            )
        }
      }
    } else {
      # Clear any existing markers and popups
      proxy_map %>%
        clearMarkers() %>%
        clearPopups()
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
