# load required packages
library(magrittr)
library(dplyr)
library(tibble)
if(!require("leaflet")) install.packages("leaflet")
library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(htmltools)
library(readr)
library(ggplot2)
library(shinycssloaders)

source("data_loader.R")

# ---- Tema visual (identidade de inesortega.github.io) -----------------------
app_theme <- bs_theme(
  version = 5,
  bg = "#f7fbfb",
  fg = "#11201f",
  primary = "#136f6f",
  secondary = "#4f5f5e",
  success = "#2f9e44",
  warning = "#e8920c",
  danger  = "#d6342a",
  base_font = font_collection("Inter", "system-ui", "-apple-system",
                              "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif"),
  "border-color" = "#dcebeb"
)

# ---- Panel lateral de filtros (mesmos inputId que a versión orixinal) -------
filters <- sidebar(
  width = 340,
  sliderInput(
    inputId = "dateRange",
    label = "Filtrado por data",
    min = as.Date("2024-01-01"),
    max = Sys.Date() + 2,
    value = c(Sys.Date() - 30, Sys.Date() + 1)
  ),
  checkboxGroupButtons(
    inputId = "legendFilter",
    label = "Tipo de actualización",
    individual = TRUE,
    size = "sm",
    choiceNames = list(
      HTML("<span class='sw' style='background:#d6342a'></span>Pellets"),
      HTML("<span class='sw' style='background:#d6342a'></span>Biosoportes"),
      HTML("<span class='sw' style='background:#d6342a'></span>Chapapote"),
      HTML("<span class='sw' style='background:#2f9e44'></span>Praia limpa"),
      HTML("<span class='sw' style='background:#2f9e44'></span>Xa non hai"),
      HTML("<span class='sw' style='background:#e8920c'></span>Limpeza"),
      HTML("<span class='sw' style='background:#6b3fa0'></span>Outras")
    ),
    choiceValues = c(
      "Hai pellets na praia",
      "Hai biosoportes",
      "Hai chapapote",
      "A praia está limpa",
      "Xa non hai (a praia quedaba limpa cando se encheu o formulario)",
      "Convocatoria de xornada de limpeza",
      "Outras Convocatorias"
    ),
    selected = c(
      "Hai pellets na praia",
      "Hai biosoportes",
      "Hai chapapote",
      "A praia está limpa",
      "Xa non hai (a praia quedaba limpa cando se encheu o formulario)",
      "Convocatoria de xornada de limpeza",
      "Outras Convocatorias"
    )
  ),
  selectInput(inputId = "select_provincia", label = "Provincia", multiple = TRUE, choices = c(), selected = ""),
  selectInput(inputId = "select_concello", label = "Concello", multiple = TRUE, choices = c(), selected = ""),
  hr(),
  uiOutput("sidebarContent")
)

# ---- Tarxeta KPI auxiliar ---------------------------------------------------
kpi_box <- function(outputId, title, subtitle = NULL, accent = "primary") {
  value_box(
    title = tags$span(class = "kpi-title", title,
                      if (!is.null(subtitle)) tags$span(class = "kpi-sub", subtitle)),
    value = textOutput(outputId, inline = TRUE),
    class = paste0("kpi-card kpi-", accent)
  )
}

# ---- UI ---------------------------------------------------------------------
ui <- page_navbar(
  title = tags$span(class = "brand",
    tags$span(class = "brand-mark", HTML('<svg width="20" height="20" viewBox="0 0 24 24" fill="none"><path d="M2 15c2.2 0 2.2-2 4.4-2s2.2 2 4.4 2 2.2-2 4.4-2 2.2 2 4.4 2" stroke="#cfeeee" stroke-width="1.7" stroke-linecap="round"/><path d="M2 19c2.2 0 2.2-2 4.4-2s2.2 2 4.4 2 2.2-2 4.4-2 2.2 2 4.4 2" stroke="#ffffff" stroke-width="1.7" stroke-linecap="round"/><circle cx="17" cy="7.5" r="2.4" fill="#ffffff"/></svg>')),
    tags$span(class = "brand-title", "Unha Vez Máis")
  ),
  id = "sidebarID",            # conserva input$sidebarID (== "map") que usa o server
  theme = app_theme,
  fillable = FALSE,
  sidebar = filters,
  header = tagList(
    useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))
  ),

  nav_panel(
    title = "Mapa en tempo real",
    value = "map",
    div(class = "intro",
        h4("Mapa en tempo real da situación das praias galegas"),
        p("O mapa reflicte as actualizacións do estado das praias enviadas polo voluntariado. ",
          "Atoparás información estatística na pestana «Datos e estatísticas»."),
        p("Emprega o panel lateral para filtrar a información por tipo de vertido, concello, ",
          "provincia ou data de actualización. Por defecto amósase a última semana.")
    ),
    card(
      full_screen = TRUE,
      class = "map-card",
      leafletOutput("mymap", height = "calc(100vh - 250px)")
    )
  ),

  nav_panel(
    title = "Datos e estatísticas",
    div(class = "stats-wrap",
      div(class = "lead",
        h4("Datos e estatísticas"),
        p(HTML(paste0(
          "Os datos recóllense de xeito colaborativo a través deste ",
          "<a href='https://docs.google.com/forms/u/1/d/e/1FAIpQLScHqNH3yxk5yBKhOMZ0mVk0Wl-bNLCowqW9UFr0mo2Hj7klGA/formResponse' target='_blank'>formulario</a>. ",
          "As estatísticas calcúlanse automaticamente para os filtros seleccionados e poden ",
          "conter erros polo reporte non estandarizado do nome da praia."))
        )
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        kpi_box("n_concellos", "Concellos", "con actualizacións"),
        kpi_box("n_praias", "Praias", "con actualizacións"),
        kpi_box("n_update", "Actualizacións", "recibidas no formulario"),
        kpi_box("n_112", "Notificacións 112", "total recibidas", accent = "danger")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        kpi_box("n_pellets", "Praias con residuos", accent = "danger"),
        kpi_box("n_limpas", "Praia limpa", accent = "success"),
        kpi_box("n_limpezas", "Outros eventos", accent = "warning")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Evolución diaria — por tipo de residuo"),
             withSpinner(plotOutput("cum_residuos"))),
        card(card_header("Evolución diaria — actualizacións recibidas"),
             withSpinner(plotOutput("cum_praias")))
      )
    )
  ),

  nav_panel(
    title = "Agradecementos",
    div(class = "ack",
        HTML(paste("Este proxecto é posible grazas á colaboración e recollida de datos de voluntarias; de Ana e Miguel das Chas; das voluntarias de Noia Limpa; e á cesión de recursos por parte de ", "<a href='https://gradiant.org' target='_blank'>Gradiant</a>"))
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class = "live-pill", tags$span(class = "live-dot"), "En directo")),

  footer = tags$footer(class = "app-footer",
    "Autoría: ", tags$a("Inés Ortega-Fernández", href = "https://inesortega.github.io/"),
    " · Contacto: ", tags$a("datospellets@gmail.com", href = "mailto:datospellets@gmail.com")
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
  output$n_concellos <- renderText({
    distinct_counts <- data() %>%
      count(Concello)
    n_concellos <- length(unique(distinct_counts$Concello))
    box_value(n_concellos)
  })

  output$n_praias <- renderText({
    praias_tipos <- c("Hai pellets na praia", "Hai chapapote", "Hai biosoportes", "Non hai pellets na praia", "A praia está limpa", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)")
    distinct_counts <- data() %>%
      count(Nome.da.praia..Concello, Concello, Tipo.de.actualización.que.nos.queres.facer.chegar) %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% praias_tipos)
    box_value(length(unique(distinct_counts$Nome.da.praia..Concello)))

  })

  output$n_update <- renderText({
    box_value(nrow(data()))
  })

  output$n_112 <- renderText({
    distinct_counts <- data() %>%
      count(Está.avisado.o.112.) %>% filter(Está.avisado.o.112. == "Si")
    box_value(distinct_counts)
  })

  reconto <- reactive({
    distinct_counts <- data() %>% count(Nome.da.praia..Concello, Concello, Tipo.de.actualización.que.nos.queres.facer.chegar)
  })

  output$n_pellets <- renderText({
    count <- reconto() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("Hai pellets na praia", "Hai chapapote", "Hai biosoportes"))
    box_value(sum(count$n))
  })

  output$n_limpas <- renderText({
    count <- reconto() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("A praia está limpa", "Non hai pellets na praia", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)"))
    box_value(sum(count$n))
  })

  output$n_limpezas <- renderText({
    count <- reconto() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("Convocatoria de xornada de limpeza", "Outras Convocatorias"))
    box_value(sum(count$n))
  })

  output$top5 <- renderPlot({
    top5 <- data() %>%
      count(Concello) %>%
      arrange(desc(n)) %>%
      head(5)
    ggplot(top5, aes(x = factor(Concello), y = n, fill = n)) +
      geom_col(stat = "n",  show.legend = FALSE) +
      geom_text(aes(label = Concello, y = n), position = position_stack(vjust = 0.5), color = "white") +
      scale_fill_gradient(low = "#136f6f", high = "#136f6f") +
      labs(x = NULL, y = NULL) + coord_flip() +
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.line.x = element_line(color = "black", size = 1),
        axis.text.x = element_text(color = "black", size = 10),
        legend.position = "none"
      )
  })

  output$cum_praias <- renderPlot({
    distinct_counts <- data() %>%
      count(Nome.da.praia..Concello, Concello, Data.Norm)

    summarized_data <- distinct_counts %>%
      group_by(Data.Norm) %>%
      summarize(sum_n = sum(n))

    ggplot(summarized_data, aes(x = Data.Norm, y = sum_n)) +
      geom_area(fill = "#136f6f", alpha = 0.15) +
      geom_line(linewidth = 1, lineend = "round", linejoin = "mitre", colour = "#136f6f") +
      labs(x = "Data", y = "Número de Actualizacións") +
      theme_minimal(base_size = 13) +
      theme(plot.background = element_rect(fill = "transparent", colour = NA))
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
      scale_color_manual(values = c("Pellets" = "#d6342a", "Biosoportes" = "#e8920c", "Chapapote" = "#6b3fa0")) +
      labs(x = "Data", y = "Número de Actualizacións", color = "Residuo") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom",
            plot.background = element_rect(fill = "transparent", colour = NA))
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
      addProviderTiles(providers$CartoDB.Positron) %>%
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

# Formatea o valor dunha tarxeta KPI (admite un número ou un tibble de count)
box_value <- function(value) {
  if (inherits(value, "tbl_df")) {
    value <- if (nrow(value) == 0) 0 else value$n
  }
  format(value, big.mark = " ", trim = TRUE)
}

if(Sys.getenv("ENV") == "docker"){
  options(shiny.host = '0.0.0.0')
  options(shiny.port = 3838)
}

shinyApp(ui, server)
