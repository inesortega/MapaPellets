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
  "border-color" = "#dcebeb",
  "font-size-base" = "0.9rem"   # texto base ~14px, como o mockup
)

# ---- Panel lateral de filtros (mesmos inputId que a versión orixinal) -------
filters <- sidebar(
  width = 330,
  dateRangeInput(
    inputId = "dateRange",
    label = "Filtrado por data",
    start = Sys.Date() - 30,
    end = Sys.Date() + 1,
    min = as.Date("2024-01-01"),
    max = Sys.Date() + 2,
    separator = " — ",
    format = "yyyy-mm-dd",
    language = "es",
    weekstart = 1
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
  selectInput(inputId = "select_concello", label = "Concello", multiple = TRUE, choices = c(), selected = "")
)

# ---- Tarxeta KPI auxiliar ---------------------------------------------------
kpi_box <- function(outputId, title, subtitle = NULL, accent = "primary", trendId = NULL) {
  value_box(
    title = tags$span(class = "kpi-title", title,
                      if (!is.null(subtitle)) tags$span(class = "kpi-sub", subtitle)),
    value = textOutput(outputId, inline = TRUE),
    if (!is.null(trendId)) tags$span(class = "kpi-trend", textOutput(trendId, inline = TRUE)),
    class = paste0("kpi-card kpi-", accent)
  )
}

# ---- UI ---------------------------------------------------------------------
ui <- page_navbar(
  title = tags$span(class = "brand",
    tags$span(class = "brand-mark", HTML('<svg width="22" height="22" viewBox="0 0 24 24" fill="none"><path d="M2 15c2.2 0 2.2-2 4.4-2s2.2 2 4.4 2 2.2-2 4.4-2 2.2 2 4.4 2" stroke="#cfeeee" stroke-width="1.7" stroke-linecap="round"/><path d="M2 19c2.2 0 2.2-2 4.4-2s2.2 2 4.4 2 2.2-2 4.4-2 2.2 2 4.4 2" stroke="#ffffff" stroke-width="1.7" stroke-linecap="round"/><circle cx="17" cy="7.5" r="2.4" fill="#ffffff"/></svg>')),
    tags$span(class = "brand-text",
      tags$span(class = "brand-title", "Unha Vez Máis"),
      tags$span(class = "brand-sub", "Monitorización cidadá das praias de Galicia")
    )
  ),
  # O `title` é HTML (marca + SVG); sen `window_title` bslib serializaría ese
  # HTML como título da pestana do navegador e mostraría "<svg ...>".
  window_title = "Unha Vez Máis - Monitorización cidadá das praias de Galicia",
  id = "sidebarID",            # conserva input$sidebarID (== "map") que usa o server
  theme = app_theme,
  fillable = FALSE,
  header = tagList(
    useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))
  ),

  nav_panel(
    title = tagList(
      HTML('<svg class="nav-ico" width="15" height="15" viewBox="0 0 24 24" fill="none"><path d="M12 21s7-6.3 7-11a7 7 0 1 0-14 0c0 4.7 7 11 7 11Z" stroke="currentColor" stroke-width="1.8"/><circle cx="12" cy="10" r="2.4" stroke="currentColor" stroke-width="1.8"/></svg>'),
      "Mapa en tempo real"
    ),
    value = "map",
    layout_sidebar(
      sidebar = filters,
      card(
        full_screen = TRUE,
        class = "map-card",
        leafletOutput("mymap", height = "calc(100vh - 140px)")
      )
    )
  ),

  nav_panel(
    title = tagList(
      HTML('<svg class="nav-ico" width="15" height="15" viewBox="0 0 24 24" fill="none"><path d="M4 19V5m0 14h16M8 16v-5m4 5V8m4 8v-3" stroke="currentColor" stroke-width="1.8" stroke-linecap="round"/></svg>'),
      "Datos e estatísticas"
    ),
    div(class = "stats-wrap",
      div(class = "lead",
        h4("Datos e estatísticas"),
        p(HTML(paste0(
          "Resumo sobre todos os datos rexistrados. Recóllense de xeito colaborativo a través deste ",
          "<a href='https://docs.google.com/forms/u/1/d/e/1FAIpQLScHqNH3yxk5yBKhOMZ0mVk0Wl-bNLCowqW9UFr0mo2Hj7klGA/formResponse' target='_blank'>formulario</a> ",
          "e poden conter erros polo reporte non estandarizado do nome da praia."))
        )
      ),
      uiOutput("week_note"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        kpi_box("n_concellos", "Concellos", "con actualizacións", trendId = "n_concellos_trend"),
        kpi_box("n_praias", "Praias", "con actualizacións", trendId = "n_praias_trend"),
        kpi_box("n_update", "Actualizacións", "recibidas no formulario", trendId = "n_update_trend"),
        kpi_box("n_112", "Notificacións 112", "total recibidas", accent = "danger", trendId = "n_112_trend")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        kpi_box("n_pellets", "Praias con residuos", accent = "danger"),
        kpi_box("n_limpas", "Praias limpas", accent = "success"),
        kpi_box("n_limpezas", "Outros eventos", accent = "warning")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Evolución por tipo de residuo (últimos 3 meses)"),
             withSpinner(plotOutput("cum_residuos"))),
        card(card_header("Actualizacións recibidas por día (histórico)"),
             withSpinner(plotOutput("cum_praias")))
      )
    )
  ),

  nav_panel(
    title = tagList(
      HTML('<svg class="nav-ico" width="15" height="15" viewBox="0 0 24 24" fill="none"><path d="M12 20s-7-4.5-7-10a4 4 0 0 1 7-2.4A4 4 0 0 1 19 10c0 5.5-7 10-7 10Z" stroke="currentColor" stroke-width="1.8" stroke-linejoin="round"/></svg>'),
      "Agradecementos"
    ),
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

  isFirstRun <- reactiveVal(TRUE)

  # Datos base (lectura + transformacións), SEN filtros. Compártense entre o
  # mapa (filtrado polo panel) e as estatísticas (datos completos).
  all_data_r <- reactive({
    invalidateLater(350000)
    all_data <- NULL
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
    all_data
  })

  # Datos filtrados polo panel lateral (só para o mapa).
  data <- reactive({
    all_data <- all_data_r()
    # Filter data based on selected date range
    max <- max(as.Date(all_data$Data.Norm))
    min <- min(as.Date(all_data$Data.Norm))
    if (isFirstRun()) {
      # Por defecto: último mes con datos dispoñibles
      updateDateRangeInput(session, inputId = "dateRange",
                           start = max - 30, end = max, min = min, max = max)
      isFirstRun(FALSE)
    } else {
      updateDateRangeInput(session, inputId = "dateRange", min = min, max = max)
    }

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

  ###### Estatísticas (datos completos, sen filtros) ################
  fulldata <- reactive(all_data_r())

  residuo_tipos <- c("Hai pellets na praia", "Hai chapapote", "Hai biosoportes")
  praia_tipos   <- c("Hai pellets na praia", "Hai chapapote", "Hai biosoportes",
                     "Non hai pellets na praia", "A praia está limpa",
                     "Xa non hai (a praia quedaba limpa cando se encheu o formulario)")
  limpa_tipos   <- c("A praia está limpa", "Non hai pellets na praia",
                     "Xa non hai (a praia quedaba limpa cando se encheu o formulario)")
  evento_tipos  <- c("Convocatoria de xornada de limpeza", "Outras Convocatorias")

  # Data de referencia = última con datos (en produción ≈ hoxe). As gráficas
  # (3 meses) áncoranse a ela para que sempre amosen actividade recente.
  ref_date <- reactive({
    d <- fulldata()
    if (is.null(d) || !nrow(d)) Sys.Date() else max(d$Data.Norm, na.rm = TRUE)
  })

  # Xanela "esta semana": relativa á SEMANA ACTUAL (últimos 7 días dende hoxe).
  # Se non hai datos nesa xanela, retrocédese á última semana CON datos e
  # sinálase con `fallback = TRUE` para amosar un aviso ao usuario.
  week_window <- reactive({
    d <- fulldata()
    recent <- Sys.Date() - 7
    has_recent <- !is.null(d) && nrow(d) && any(d$Data.Norm >= recent, na.rm = TRUE)
    if (has_recent) list(start = recent, end = Sys.Date(), fallback = FALSE)
    else            list(start = ref_date() - 7, end = ref_date(), fallback = TRUE)
  })
  last_week <- reactive(week_window()$start)

  # Aviso cando os incrementos non son da semana actual senón da última con datos
  output$week_note <- renderUI({
    w <- week_window()
    if (!isTRUE(w$fallback)) return(NULL)
    div(class = "stats-note",
      HTML(paste0(
        "⚠️ Non se rexistraron datos novos esta semana. Os incrementos amosados ",
        "(\"esta semana\") son relativos á última semana con datos dispoñibles (",
        format(w$start, "%d/%m/%Y"), " – ", format(w$end, "%d/%m/%Y"), ").")))
  })

  # Tendencia: actividade na xanela semanal
  trend_chip <- function(n) {
    if (is.na(n) || n == 0) "sen novidades esta semana"
    else paste0("▲ ", format(n, big.mark = " ", trim = TRUE), " esta semana")
  }

  output$n_concellos <- renderText(box_value(length(unique(fulldata()$Concello))))
  output$n_concellos_trend <- renderText(
    trend_chip(length(unique((fulldata() %>% filter(Data.Norm >= last_week()))$Concello))))

  output$n_praias <- renderText(
    box_value(length(unique((fulldata() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% praia_tipos))$Nome.da.praia..Concello))))
  output$n_praias_trend <- renderText(
    trend_chip(length(unique((fulldata() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% praia_tipos,
             Data.Norm >= last_week()))$Nome.da.praia..Concello))))

  output$n_update <- renderText(box_value(nrow(fulldata())))
  output$n_update_trend <- renderText(
    trend_chip(nrow(fulldata() %>% filter(Data.Norm >= last_week()))))

  output$n_112 <- renderText(
    box_value(sum(fulldata()$Está.avisado.o.112. == "Si", na.rm = TRUE)))
  output$n_112_trend <- renderText(
    trend_chip(sum((fulldata() %>% filter(Data.Norm >= last_week()))$Está.avisado.o.112. == "Si", na.rm = TRUE)))

  output$n_pellets <- renderText(
    box_value(sum(fulldata()$Tipo.de.actualización.que.nos.queres.facer.chegar %in% residuo_tipos)))
  output$n_limpas <- renderText(
    box_value(sum(fulldata()$Tipo.de.actualización.que.nos.queres.facer.chegar %in% limpa_tipos)))
  output$n_limpezas <- renderText(
    box_value(sum(fulldata()$Tipo.de.actualización.que.nos.queres.facer.chegar %in% evento_tipos)))

  # Evolución por tipo de residuo — últimos 3 meses
  output$cum_residuos <- renderPlot({
    d <- fulldata() %>%
      filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% residuo_tipos,
             Data.Norm >= ref_date() - 90) %>%
      count(Residuo, Data.Norm) %>%
      group_by(Data.Norm, Residuo) %>%
      summarize(sum_n = sum(n), .groups = "drop")
    ggplot(d, aes(x = Data.Norm, y = sum_n, color = Residuo, group = Residuo)) +
      geom_line(linewidth = 1, lineend = "round", linejoin = "mitre") +
      scale_color_manual(values = c("Pellets" = "#d6342a", "Biosoportes" = "#e8920c", "Chapapote" = "#6b3fa0")) +
      labs(x = NULL, y = "Actualizacións", color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom",
            plot.background = element_rect(fill = "transparent", colour = NA))
  })

  # Actualizacións recibidas por día — histórico completo
  output$cum_praias <- renderPlot({
    d <- fulldata() %>%
      count(Nome.da.praia..Concello, Concello, Data.Norm) %>%
      group_by(Data.Norm) %>%
      summarize(sum_n = sum(n), .groups = "drop")
    ggplot(d, aes(x = Data.Norm, y = sum_n)) +
      geom_area(fill = "#136f6f", alpha = 0.15) +
      geom_line(linewidth = 1, lineend = "round", linejoin = "mitre", colour = "#136f6f") +
      labs(x = NULL, y = "Actualizacións") +
      theme_minimal(base_size = 13) +
      theme(plot.background = element_rect(fill = "transparent", colour = NA))
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
              data = data() %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("Hai pellets na praia", "Hai chapapote", "Hai biosoportes")) %>% filter(is.finite(lon), is.finite(lat)),
              lng = ~lon,
              lat = ~lat,
              label = ~paste(Residuo, " - ", Nome.da.praia..Concello),
              layerId = ~id,
              radius = 10,
              color = "#e31a1c",
              popup = ~ popup_card(
                state = "Hai residuos na praia", accent = "#d6342a",
                title = Nome.da.praia..Concello, subtitle = Concello,
                fields = list(
                  "Tipo de residuo"  = Residuo,
                  "Data"             = Marca.temporal,
                  "Avisado o 112"    = Está.avisado.o.112.,
                  "Animais mortos"   = Atopaches.animáis.mortos.,
                  "Espallados por"   = Por.onde.están.espallados.os.residuos,
                  "Recollendo"       = Quen.está.recollendo.os.residuos.,
                  "Como recoller"    = Como.se.están.a.recoller.os.residuos.,
                  "Onde depositan"   = Onde.se.depositan.os.residuos.,
                  "Sacos"            = Hai.sacos.de.pellets.ou.outro.tipo.de.residuos.,
                  "Cantidade"        = Cantidade.de.residuos
                ),
                foot = img_links(Imaxe.dos.residuos.no.lugar.ou.da.xornada.de.limpeza)
              ),
              stroke = FALSE, fillOpacity = 0.5
            )

        }
        else if (selected_filter == "Convocatoria de xornada de limpeza" & nrow(data_convocatoria()) > 0) {
          proxy_map %>%
            addCircleMarkers(
              data = data() %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Convocatoria de xornada de limpeza") %>% filter(is.finite(lon), is.finite(lat)),
              lng = ~lon,
              lat = ~lat,
              label = ~Nome.da.praia..Concello,
              layerId = ~id,
              radius = 10,
              color = "#F5A618",
              popup = ~ popup_card(
                state = "Convocatoria de limpeza", accent = "#e8920c",
                title = Nome.da.praia..Concello, subtitle = Concello,
                fields = list(
                  "Data e hora"       = paste(as.Date(Data.da.Convocatoria), Hora),
                  "Lugar de encontro" = Lugar.de.encontro,
                  "Organiza"          = Quen.organiza.a.iniciativa
                ),
                foot = ifelse(is.na(Ligazón) | Ligazón == "", "",
                              paste0("<a href='", htmlEscape(Ligazón), "' target='_blank'>Ligazón →</a>"))
              ),
              stroke = FALSE, fillOpacity = 0.5
            )
        }
        else if (selected_filter == "Outras Convocatorias" & nrow(data_evento()) > 0) {
          proxy_map %>%
            addCircleMarkers(
              data = data() %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar == "Outras Convocatorias") %>% filter(is.finite(lon), is.finite(lat)),
              lng = ~lon,
              lat = ~lat,
              label = ~Nome.da.praia..Concello,
              layerId = ~id,
              radius = 10,
              color = "#490a73",
              popup = ~ popup_card(
                state = "Outras convocatorias", accent = "#6b3fa0",
                title = Nome.da.praia..Concello, subtitle = Concello,
                fields = list(
                  "Data e hora"       = paste(as.Date(Data.da.Convocatoria), Hora),
                  "Lugar de encontro" = Lugar.de.encontro,
                  "Organiza"          = Quen.organiza.a.iniciativa
                ),
                foot = paste0(
                  ifelse(is.na(Ligazón) | Ligazón == "", "",
                         paste0("<a href='", htmlEscape(Ligazón), "' target='_blank'>Ligazón →</a> ")),
                  ifelse(is.na(Cartaz) | Cartaz == "", "",
                         paste0("<a href='", htmlEscape(Cartaz), "' target='_blank'>Cartaz →</a>"))
                )
              ),
              stroke = FALSE, fillOpacity = 0.5
            )
        }
        else if (selected_filter %in% c("Non hai pellets na praia", "A praia está limpa", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)") & nrow(data_xa_non_hai()) > 0) {
          proxy_map %>%
            addCircleMarkers(
              data = data() %>% filter(Tipo.de.actualización.que.nos.queres.facer.chegar %in% c("Non hai pellets na praia", "A praia está limpa", "Xa non hai (a praia quedaba limpa cando se encheu o formulario)")) %>% filter(is.finite(lon), is.finite(lat)),
              lng = ~lon,
              lat = ~lat,
              label = ~paste(Tipo.de.actualización.que.nos.queres.facer.chegar, " - ", Nome.da.praia..Concello),
              layerId = ~id,
              radius = 10,
              popup = ~ popup_card(
                state = "Praia limpa / sen residuos", accent = "#2f9e44",
                title = Nome.da.praia..Concello, subtitle = Concello,
                fields = list(
                  "Estado"         = Tipo.de.actualización.que.nos.queres.facer.chegar,
                  "Data"           = Marca.temporal,
                  "Avisado o 112"  = Está.avisado.o.112.,
                  "Animais mortos" = Atopaches.animáis.mortos.,
                  "Cantidade"      = Cantidade.de.residuos
                ),
                foot = img_links(Imaxes.adicionais)
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

# Constrúe os enlaces ás imaxes (campo cunha lista de URLs separadas por comas).
# Vectorizado: devolve un vector de HTML, un por fila.
img_links <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(s)) return("")
    urls <- trimws(strsplit(s, ",\\s*")[[1]])
    urls <- urls[nzchar(urls)]
    if (!length(urls)) return("")
    enc <- vapply(urls, function(u) utils::URLencode(u), character(1), USE.NAMES = FALSE)
    paste0("<span class='pc-imgs-label'>Imaxes:</span> ",
           paste(paste0("<a href='", htmlEscape(enc),
                        "' target='_blank'>", seq_along(urls), "</a>"),
                 collapse = " · "))
  }, character(1), USE.NAMES = FALSE)
}

# Tarxeta de popup do mapa ao estilo do mockup (vectorizada para leaflet).
# 'fields' = lista nomeada de vectores (etiqueta -> valor); 'foot' = HTML opcional.
popup_card <- function(state, accent, title, subtitle, fields = list(), foot = NULL) {
  rows <- ""
  for (lab in names(fields)) {
    v <- as.character(fields[[lab]])
    v <- ifelse(is.na(v) | v == "" | v == "NA", "—", htmlEscape(v))
    rows <- paste0(rows, "<div class='pc-row'><dt>", htmlEscape(lab), "</dt><dd>", v, "</dd></div>")
  }
  foot_html <- if (is.null(foot)) "" else
    ifelse(foot == "", "", paste0("<div class='pc-foot'>", foot, "</div>"))
  paste0(
    "<div class='pc' style='--pc:", accent, "'>",
      "<span class='pc-state'>", htmlEscape(state), "</span>",
      "<div class='pc-title'>", htmlEscape(title), "</div>",
      "<div class='pc-sub'>", htmlEscape(subtitle), "</div>",
      "<dl class='pc-dl'>", rows, "</dl>",
      foot_html,
    "</div>"
  )
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
