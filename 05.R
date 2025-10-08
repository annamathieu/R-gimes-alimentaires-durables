### Impact environnemental
server <- function(input, output, session) {
  
  continent_map <- c(
    "Africa"     = "AFR",
    "Americas"   = "AMR",
    "Asia"       = "SEA",
    "Europe"     = "EUR",
    "Oceania"    = "WPR"
  )
  
  
  # Filtrage des données 'conti' selon le continent sélectionné
  
  filtered_data <- reactive({
    req(input$continent)
    
    # Obtenir les codes de `conti` à partir des continents sélectionnés
    continent_codes <- continent_map[input$continent]
    continent_codes <- na.omit(continent_codes)  # on enlève les NA (ex: Antarctica)
    
    # Filtrage de conti
    conti[conti$continent %in% continent_codes, ]
  })
  
  # Mise à jour dynamique des pays et régions économiques
  observe({
    req(filtered_data())
    
    updatePickerInput(session, "country",
                      choices = sort(unique(filtered_data()$nom_pays)))
    
    updatePickerInput(session, "reg_eco",
                      choices = sort(unique(filtered_data()$r_eco)))
  })
  
  # Données sélectionnées en fonction des filtres
  selected_data <- reactive({
    studiedcountries(
      conti = conti,
      contin = input$continent,
      country = input$country,
      reg_eco = input$reg_eco
    )
  })
  
  # Affichage de la carte
  output$map <- renderLeaflet({
    selected_data(leaflet(options = leafletOptions(minZoom = 2, maxZoom = 7)) %>%
                    addTiles(options = tileOptions(noWrap = TRUE)) %>%
                    setView(lng = 0, lat = 20, zoom = 2))
  })
}





library(tidyverse)
library(plotly)
library(dplyr)


sante <- read.table(file = "data_csv/sante.csv", header = T, sep =";", stringsAsFactors = T, dec = ",")

str(sante)

plot_health_by_diet <- function(data, country, variable, disease_filter = NULL) {
  # Filtrage des données
  data_filtered <- data %>%
    filter(code_pays == country)
  
  if (!is.null(disease_filter)) {
    data_filtered <- data_filtered %>%
      filter(disease == disease_filter)
  }
  
  # Vérification que la variable est valide
  if (!(variable %in% names(data))) {
    stop("Variable non valide.")
  }
  
  # Graphique interactif
  plot_ly(
    data_filtered,
    x = ~diet.scenario,
    y = as.formula(paste0("~", variable)),
    type = 'bar',
    color = ~diet.scenario
  ) %>%
    layout(
      title = paste0("Effet des scénarios alimentaires sur ", variable, " en ", country),
      xaxis = list(title = "Scénario alimentaire"),
      yaxis = list(title = variable)
    )
}

plot_health_by_diet(sante, country = "FRA", variable = "obese", disease_filter = "all-c")


