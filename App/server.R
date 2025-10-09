
  ###############################################################
  ### Onglet CARTE
  ###############################################################
server <- function(input, output, session) {
  
  continent_map <- c(
    "Africa"     = "AFR",
    "Americas"   = "AMR",
    "Asia"       = c("SEA", "SEAR"),
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
  
  
  ################################ data table nutri ###################################
  
  # sélection des columns 
  # select_coldatatable <- reactive({
  #   req(input$columnsdatanutri)
  #   req(length(input$columnsdatanutri) > 0)
  #   
  #   
  #   print.nutri (nutri_new[,input$columnsdatanutri, drop = F]) # on sélectionne les colonnes selectionnées par l'utilisateur
  #   # Quand une seule colonne est sélectionné=> drop = F évite d'avoir un résultat sous forme de vecteur 
  #   
  # })
  
  output$datatablenutri <- DT::renderDataTable({
    print.nutri(nutri_new) 
    
  },  server = T)
  
  
  
  ###############################################################
  # PCA nutri 
  ###############################################################
  sel_country_n <- reactive ({
    req(input$country_pca_nutri)
    
    pca.nutri(nutri_new, 
              country=input$country_pca_nutri)
  })

  output$plot1 <- renderPlot({
    sel_country_n()[[1]] 
  })
  
  output$plot2 <- renderPlot({
    sel_country_n()[[2]] 
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
    
    
    
    # ---- ENVIRONMENTAL DATA TABLE ----
    output$datatable_env <- DT::renderDataTable({
      print.env(env_new)
    })
    
    
    # ---- PCA ENVIRONMENTAL ----
    output$pca_env_plot <- renderPlot({
      env_pca_f(data = env_new, country = input$country_env)
    })


  ###############################################################
    # Onglet HEALTH ASPECTS
    ###############################################################
    
    output$mortality_plot <- renderPlot({
      
      # On récupère les inputs
      selected_countries <- input$selected_countries
      selected_risk_factors <- input$selected_risk_factors
      selected_scenarios <- input$selected_scenarios
      selected_disease <- input$selected_disease
      
      req(selected_countries, selected_risk_factors, selected_scenarios, selected_disease) 
      
      stack_bars <- input$stack_bars

      # Appel de la fonction avec les paramètres
      plot_mortality_prc(
        data = sante_new,
        selected_risk_factors = selected_risk_factors,
        selected_scenarios = selected_scenarios,
        selected_countries = selected_countries,
        selected_disease = selected_disease,
        stack_bars = stack_bars
      )
    }, 
    # Ajuster les marges
    res = 96 # Résolution standard
    )
    
    
    # ---- HEALTH DATA TABLE ----
    output$datatable_sante <- DT::renderDataTable({
      print.health(sante_new)
    })
    
    
    ###############################################################
    # GLOBAL ANALYSIS - MFA
    ###############################################################
    
    #Calcul de la MFA une seule fois
    res_mfa <- reactive({
      req(input$country_mfa)
      
      mfa_simple(country= input$country_mfa)
      
      # 
      # data.mfa <- cbind(
      #   nutri_new %>%
      #     filter(item == "abs", code_pays == input$country_mfa, diet.scenario != "BMK") %>%
      #     select(-item, -code_pays, -grp_diet),
      #   env_new %>%
      #     filter(item == "abs", code_pays == input$country_mfa, socio.econ.scenario == "SSP2", diet.scenario != "BMK") %>%
      #     select(-item, -code_pays, -socio.econ.scenario, -grp_diet, -diet.scenario),
      #   sante_new %>%
      #     filter(parameter == "deaths_avd", disease == "all-c", code_pays == input$country_mfa) %>%
      #     select(-grp_diet, -diet.scenario, -disease, -parameter, -code_pays, -all-rf)
      # )
      # 
      # # Vérification
      # if (nrow(data.mfa) == 0) return(NULL)
      # 
      # MFA(data.mfa,
      #     group = c(1, 24, 5, 9),
      #     type = c("n", "s", "s", "s"),
      #     name.group = c("diet", "nutritional", "environmental", "health"),
      #     num.group.sup = 1)
    })
    
    
    
    output$plot_mfa_ind <- renderPlot({
      res_mfa()[[1]] 
    })
    
    output$plot_mfa_group <- renderPlot({
      res_mfa()[[2]] 
    })    
    
    output$plot_mfa_score <- renderPlot({
      res_mfa()[[3]] 
    })    
    
    output$plot_mfa_partial <- renderPlot({
      res_mfa()[[4]] 
    })
    
    
    # 
    # # Graphique 1 : Individuals
    # output$plot_ind <- renderPlot({
    #   req(res_mfa())
    #   fviz_mfa_ind(res_mfa(), repel = TRUE) +
    #     labs(title = paste("Individuals factor map -", input$country_mfa)) +
    #     theme_minimal(base_size = 13) +
    #     theme(plot.title = element_text(face = "bold", hjust = 0.5))
    # })
    # 
    # # Graphique 2 : Groups representation
    # output$plot_groups <- renderPlot({
    #   req(res_mfa())
    #   fviz_mfa_var(res_mfa(), "group") +
    #     labs(title = "Groups representation") +
    #     theme_minimal(base_size = 13) +
    #     theme(plot.title = element_text(face = "bold", hjust = 0.5))
    # })
    # 
    # # Graphique 3 : Correlation circle
    # output$plot_corr <- renderPlot({
    #   req(res_mfa())
    #   fviz_mfa_var(res_mfa(), "quanti.var", repel = TRUE) +
    #     labs(title = "Correlation circle (quantitative variables)") +
    #     theme_minimal(base_size = 13) +
    #     theme(plot.title = element_text(face = "bold", hjust = 0.5))
    # })
    # 
    # # Graphique 4 : Individuals with partial groups
    # output$plot_partial <- renderPlot({
    #   req(res_mfa())
    #   fviz_mfa_ind(res_mfa(), partial = "all", repel = TRUE) +
    #     labs(title = "Individuals with partial groups") +
    #     theme_minimal(base_size = 13) +
    #     theme(plot.title = element_text(face = "bold", hjust = 0.5))
    # })
    # 
    # 
    
    
    
    
}
    
  
  


