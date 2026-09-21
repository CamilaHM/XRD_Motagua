library(tidyverse)
library(leaflet)
library(leaflet.minicharts)

dataXlsx <- readxl::read_xlsx("id_jade_xrd_desy.xlsx",sheet = "Sheet1") 
dataXlsx <- dataXlsx %>% filter(!code.crt %in% c("geo11", "mot202"),type %in% c("mot","vdc") )

dataSitios <- readxl::read_xlsx("id_jade_xrd_desy.xlsx",sheet = "ubi")
dataSitios$lat <- as.numeric(str_replace(dataSitios$lat, ",", "."))
dataSitios$long<- as.numeric(str_replace(dataSitios$long, ",", "."))

datos_limpios <- dataXlsx %>%
    dplyr::select(site, zone, min_1, min_2, min_3, min_4) %>%
    pivot_longer(
        cols = starts_with("min_"), 
        names_to = "phase",       
        values_to = "mineral",   
        values_drop_na = TRUE   
    ) %>%
    mutate(mineral = trimws(mineral)) %>%
    filter(mineral != "")%>%
    filter(mineral != "other")


datos_mapa <- datos_limpios %>%
    group_by(site, mineral) %>%
    summarise(frecuencia = n(), .groups = "drop") %>%
    pivot_wider(
        names_from = mineral, 
        values_from = frecuencia, 
        values_fill = 0 ) 

datos_mapa <- datos_mapa %>%
    dplyr::select(site, where(~ is.numeric(.) && sum(.) > 1))


datos_finales <- left_join(dataSitios, datos_mapa, by = "site")
unique(datos_finales$site)

cols_used <- setdiff(names(datos_finales), c("site", "lat", "long"))

frecuencia_minerales <- colSums(datos_finales[cols_used], na.rm = TRUE)

cols_minerales <- names(sort(frecuencia_minerales, decreasing = TRUE))
# Paleta de colores
palette_completa <- setNames(
    colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7", "#9edae5", "#393b79"))(length(cols_minerales)),
    cols_minerales)



library(shiny)

ui <- fluidPage(
    titlePanel("Mineral distribution in ancient Workshops from the Motagua Valley"),
    sidebarLayout(
        sidebarPanel(
            width = 4,
            h4("Filter Minerals"),
            p("Select the minerals to view their specific distribution."),
            div(
                style = "margin-bottom: 15px;",
                actionButton("select_all", "Select all", class = "btn-sm btn-primary"),
                actionButton("deselect_all", "Reset", class = "btn-sm btn-default")
            ),
            checkboxGroupInput("selected_minerals", "Minerals", 
                               choiceNames = cols_minerales, 
                               choiceValues = cols_minerales,
                               selected = cols_minerales[1:4])),
        mainPanel(
            leafletOutput("mapa"))
    )
)

server <- function(input, output, session) {
    
    # Select all button
    observeEvent(input$select_all, {
        updateCheckboxGroupInput(
            session = session,
            inputId = "selected_minerals",
            selected = cols_minerales  )
    })
    
    # Reset button
    observeEvent(input$deselect_all, {
        updateCheckboxGroupInput(
            session = session,
            inputId = "selected_minerals",
            selected = character(0)
        )
    })
    
    #Minerales seleccionados
    datos_reactivos <- reactive({
        req(input$selected_minerals)
        cols_actuales <- input$selected_minerals
        
        cols_a_usar <- c("site", "lat", "long", cols_actuales)
        datos_filtrados <- datos_finales %>% dplyr::select(all_of(cols_a_usar))
        
        # Reemplazar NA por 0 en minerales seleccionados
        datos_filtrados[cols_actuales] <- lapply(datos_filtrados[cols_actuales], function(x) ifelse(is.na(x), 0, x))
        
        #total x sitio
        total_general <- rowSums(datos_finales %>% dplyr::select(all_of(cols_minerales)), na.rm = TRUE)
        
        #lista de minerales seleccionados x sitio
        minerales_detalle <- apply(datos_filtrados[cols_actuales], 1, function(row) {
            presentes <- row[row > 0]
            if (length(presentes) == 0) {
                return("None")
            } else {
                paste(names(presentes), presentes, sep = ": ", collapse = ", ")
            }
        })
        
        # texto del Popup
        datos_filtrados$popup_text <- paste0(
            "<b>", datos_filtrados$site, "</b><br>",
            "<b>Total number of samples (site):</b> ", total_general, "<br>",
            "<b>Selected minerals:</b> ", minerales_detalle
        )
        
        return(datos_filtrados)
    })
    
    #Mapa
    output$mapa <- renderLeaflet({
        datos_act <- datos_reactivos()
        cols_act <- input$selected_minerals
        
        palette_act <- palette_completa[names(palette_completa) %in% cols_act]
        
        leaflet(datos_act) %>%
            addProviderTiles(providers$Esri.WorldShadedRelief) %>%
            addMinicharts(
                lng = datos_act$long, 
                lat = datos_act$lat,
                chartdata = datos_act %>% dplyr::select(all_of(cols_act)),
                type = "pie",
                colorPalette = unname(palette_act),
                popup = popupArgs(html = datos_act$popup_text)
            )
    })
    
}
# Ejecutar
shinyApp(ui, server)
