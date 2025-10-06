#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram


server <- function(input, output, session) {
  
  continent_map <- c(
    "Africa"     = "AFR",
    "Americas"   = "AMR",
    "Asia"       = "SEA",  # ← à adapter selon ce que tu veux (SEA ? WPR ? EMR ?)
    "Europe"     = "EUR",
    "Oceania"    = "WPR",  # ← à adapter
    "Antarctica" = NA      # pas dans `conti`, donc on ignore
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
    selected_data()
  })
  
  
  
  
  
  ###############################################
  
  # Données sélectionnées en fonction des filtres
  sel_data_nutri <- reactive({
    req(input$nutriments_nutri, input$paysnutri, input$diets_nutri)
    
    partition.bydiet_f(
      nutriments = input$nutriments_nutri,
      country = input$paysnutri,
      regimes = input$diets_nutri,
      ncol = input$ncolnutri
    )
  })
  
  # 
  # observe({
  #   req(sel_data_nutri())
  #   
  #   updatePickerInput(session, "paysnutri",
  #                     choices = sort(unique(filtered_data()$nom_pays)))
  #   
  #   updatePickerInput(session, "reg_eco",
  #                     choices = sort(unique(filtered_data()$r_eco)))
  # })
  
  # Affichage de la carte
  output$plot <- renderPlot({
    sel_data_nutri() 
    # theme(legend.text = element_text(size = 15), 
    #       legend.title = element_text(size = 15, face = "bold"), 
    #       legend.position = "right")
    
  })
  
  
  
  # sélection des columns 
  # select_coldatatable <- reactive({
  #   # req(input$columnsdatanutri)
  #   
  #   print.nutri ()
  #     # col = input$columnsdatanutri
  #   # )
  #   
  # })

  ###############################################################
  # Onglet ENVIRONMENTAL ASPECTS
  ###############################################################
  
  sel_data_env <- reactive({
    req(input$indicateurs_env, input$paysenv, input$diets_env)
    
    env_partition(
      domains = input$indicateurs_env,
      country = input$paysenv,
      regimes = input$diets_env,
      ncol = input$ncolenv
    )
  })
  
  output$plot_env <- renderPlot({
    sel_data_env()
  })

  
  # print data table
  output$datatablenutri <- DT::renderDataTable(
    print.nutri (), 
    server = T
    
  )
  
  # output graph nutriments %rec 
    sel_data_nutri <- reactive({
    req(input$nutriments_nutri, input$paysnutri, input$diets_nutri)
    
    partition.bydiet_f(
      nutriments = input$nutriments_nutri,
      country = input$paysnutri,
      regimes = input$diets_nutri,
      ncol = input$ncolnutri
    )
  })
  
  
    ###############################################################
    # Onglet ENVIRONMENTAL ASPECTS
    ###############################################################
    
    sel_data_env <- reactive({
      req(input$indicateurs_env, input$paysenv, input$diets_env)
      
      env_partition(
        domains = input$indicateurs_env,
        country = input$paysenv,
        regimes = input$diets_env,
        ncol = input$ncolenv
      )
    })
    
    output$plot_env <- renderPlot({
      sel_data_env()
    })
    
}
    
  
  


