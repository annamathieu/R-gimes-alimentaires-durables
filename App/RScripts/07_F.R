#####----------------------------------------------------------------------#####
##    FUNCTION : PLOT DATA HEALTH
#####----------------------------------------------------------------------#####


plot_mortality_prc <- function(
    data,
    selected_countries = c("all-r"),
    selected_risk_factors = c("all-rf"),
    selected_scenarios = c("ani-25", "ani-50", "ani-75", "ani-100", 
                           "kcal-25", "kcal-50", "kcal-75", "kcal-100",
                           "FLX", "PSC", "VEG", "VGN"),
    selected_disease = "all-c", 
    stack_bars = TRUE 
) {
  
  # --- Définition des constantes et des palettes ---
  
  # Ordre des scénarios
  ordre_scenarios <- c("ani-25", "ani-50", "ani-75", "ani-100", 
                       "kcal-25", "kcal-50", "kcal-75", "kcal-100",
                       "FLX", "PSC", "VEG", "VGN")
  
  # Palette de couleurs pour les facteurs de risque
  colors_for_factors <- c(
    "obese" = "darkslategray",
    "overweight" = "lightgray",
    "underweight" = "gray",
    "r+p_meat" = "darkred",
    "fish" = "darkcyan",
    "legumes" = "yellow",
    "nuts_seeds" = "darkgreen",
    "fruits" = "indianred",
    "vegetables" = "lightgreen"
  )
  
  # Couleur pour "all-rf"
  color_all_rf <- "navy"
  
  facteurs_individuels <- names(colors_for_factors)
  
  # Liste de tous les facteurs individuels (pour le pivotage et l'empilement)
  facteurs_individuels <- names(colors_for_factors)
  
  # --- Vérifications et filtrage des données ---
  
  # Filtrage : Paramètre, Pays, selected_disease, Scénarios
  data_filtree <- data %>%
    filter(
      parameter == "%deaths_avd_prm/all",
      code_pays == selected_countries,
      disease == selected_disease,
      diet.scenario %in% selected_scenarios
    ) %>%
    # Vérifier l'ordre des scénarios
    mutate(diet.scenario = factor(diet.scenario, levels = ordre_scenarios))
  
  if (nrow(data_filtree) == 0) {
    stop("Aucune donnée trouvée pour les critères de sélection (pays, selected_disease, scénarios).")
  }
  
  # Vérification de la sélection de risque
  has_all_rf <- "all-rf" %in% selected_risk_factors
  facteurs_a_analyser <- intersect(selected_risk_factors, c("all-rf", facteurs_individuels))
  
  # La variable 'all-rf' ne doit pas être empilée avec les risques individuels.
  if (has_all_rf && any(selected_risk_factors %in% facteurs_individuels)) {
    message(paste0("Attention: La variable 'all-rf' sera traitée séparément (comme un point) et ne sera pas empilée avec les risques individuels. Seuls les facteurs individuels (<= ", length(facteurs_individuels), " sont empilés."))
  }
  
  # --- Préparation des données pour la visualisation ---
  
  # Séparation des facteurs individuels et de 'all-rf'
  
  data_all_rf <- data_filtree %>%
    select(diet.scenario, `all-rf`) %>%
    filter(!is.na(`all-rf`))
  
  data_individuels <- data_filtree %>%
    select(diet.scenario, all_of(facteurs_individuels)) %>%
    # Pivoter les facteurs individuels
    pivot_longer(
      cols = all_of(facteurs_individuels),
      names_to = "facteur_risque",
      values_to = "contribution"
    ) %>%
    # Filtrer uniquement les facteurs individuels demandés
    filter(facteur_risque %in% intersect(selected_risk_factors, facteurs_individuels)) %>%
    filter(!is.na(contribution))
  
  # Déterminer le mode d'affichage
  nb_risques_individuels <- length(unique(data_individuels$facteur_risque))
  
  # --- Création du graphique ---
  
  # Nom complet du pays
  nom_pays <- unique(data_filtree$pays)
  
  # Déterminer si on utilise un graphique empilé ou séparé
  if (nb_risques_individuels > 0) {
    
    graphique <- ggplot(data_individuels, aes(x = diet.scenario, y = contribution))
    
    if (stack_bars == TRUE || nb_risques_individuels > 8) {
      # Mode Empilement (ou une seule barre)
      graphique <- graphique +
        geom_bar(
          aes(fill = facteur_risque), 
          stat = "identity", 
          position = "stack", 
          alpha = 0.8
        ) +
        labs(y = "% Reduction in Premature Mortality")
      
      # Si empilé, on ne met la légende qu'une seule fois
      graphique <- graphique +
        scale_fill_manual(
          values = colors_for_factors[unique(data_individuels$facteur_risque)],
          name = "Risk Factor"
        )
      
    } else {
      # Mode Facet Wrap
      graphique <- graphique +
        geom_bar(
          aes(fill = facteur_risque),
          stat = "identity", 
          alpha = 0.8
        ) +
        # Un graphique par facteur de risque
        facet_wrap(~ facteur_risque, scales = "free_y", ncol = 3) +
        labs(y = "% Reduction in Premature Mortality (Scale by Factor)") +
        scale_fill_manual(
          values = colors_for_factors[unique(data_individuels$facteur_risque)]
        ) +
        guides(fill = "none") # Supprimer la légende 'fill'
    }
    
    # Ajout de la valeur 'all-rf' (comme point)
    if (has_all_rf && nb_risques_individuels > 8) {
      
      # Si on est en mode Facet Wrap, on ajoute les points à TOUS les facets
      # Sinon, on les ajoute au graphique principal
      data_all_rf_for_plot <- if (stack_bars == FALSE && nb_risques_individuels > 1) {
        # Dupliquer la ligne 'all-rf' pour chaque facet afin d'afficher le point
        data_all_rf %>%
          crossing(facteur_risque = unique(data_individuels$facteur_risque))
      } else {
        data_all_rf
      }
      
      graphique <- graphique +
        geom_point(
          data = data_all_rf_for_plot,
          aes(y = `all-rf`),
          color = color_all_rf,
          size = 4,
          shape = 18 # Losange pour 'all-rf'
        ) +
        geom_text(
          data = data_all_rf_for_plot,
          aes(y = `all-rf`, label = round(`all-rf`, 1)),
          vjust = -1,
          color = color_all_rf,
          size = 3
        )
      
    }
    
    # Thème et titres généraux
    titre_graphe <- paste0(
      "% Reduction in Premature Mortality (", 
      selected_disease, 
      ") by Diet \n", 
        "Country: ", nom_pays, " (", selected_countries, ")"
    )
    
    graphique <- graphique +
      labs(
        title = titre_graphe,
        x = "Diet",
        caption = ifelse(
          has_all_rf && nrow(data_all_rf) > 0,
          paste0("The dots ", color_all_rf, " (diamonds) represent the total reduction ('all-rf')."),
          ""
        )
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
    
    return(graphique)
    
  } else if (has_all_rf && nrow(data_all_rf) > 0) {
    # Cas où seul 'all-rf' est demandé
    graphique <- ggplot(data_all_rf, aes(x = diet.scenario, y = `all-rf`)) +
      geom_bar(stat = "identity", fill = color_all_rf, alpha = 0.8) +
      geom_text(aes(label = round(`all-rf`, 1)), vjust = -1, color = "black") +
      labs(
        title = paste0("% Reduction in Premature Mortality (", selected_disease, ") - Facteurs Agrégés ('all-rf')\nPays: ", nom_pays, " (", selected_countries, ")"),
        x = "Diet",
        y = "% Reduction in Premature Mortality ('all-rf')"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
    
    return(graphique)
    
  } else {
    warning("Aucune donnée pertinente n'a pu être préparée pour le graphique avec les facteurs sélectionnés.")
    return(NULL)
  }
}





# facteur_risques <- c("vegetables", "fruits", "nuts_seeds", "legumes", "fish", "r+p_meat", "underweight", "overweight", "obese")
# 
# facteur_risques <- c("fruits", "nuts_seeds", "legumes", "fish", "r+p_meat", "underweight", "overweight", "obese")
# 

# 
# plot_mortality_prc(
#   data = sante_new, 
#   selected_countries = "FRP",
#   selected_risk_factors = c(facteur_risques, "all-rf"),# Scénarios choisis
#   stack_bars = F
# )
# 
# plot_mortality_prc(
#   data = sante_new, 
#   selected_countries = c("USA"),
#   selected_risk_factors = c("underweight", "overweight", "obese"),
#   selected_disease = "all-c",
#   stack_bars = F
# )
