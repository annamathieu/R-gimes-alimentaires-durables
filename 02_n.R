

summary(env_new)

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)


##############################################################################
# PARTITION ENVIRONNEMENT
##############################################################################


env_partition <- function(domains = c("GHGe", "land", "water", "nitr", "phos"),  # indicateurs environnementaux
                          country = "FRP",                                       # pays
                          regimes = levels(env_new$diet.scenario),               # régimes à inclure
                          item = "pct",                                          # type de valeur : abs / pct
                          ncol = 3,                                              # nombre de colonnes d'affichage
                          title = "Comparaison du % de changement de chaque indicateur environnemental par régime alimentaire") {
  
  # ordre d'affichage fixe
  bar_order <- c("BMK", "ani-25", "ani-50", "ani-75", "ani-100",
                 "kcal-25","kcal-50", "kcal-75", "kcal-100",
                 "FLX", "PSC","VEG","VGN")
  legend_order <- bar_order
  
  # --- Ajouter le nom du pays depuis le tableau pays_continent ---
  if (exists("pays_continent")) {
    nom_pays_affiche <- pays_continent %>%
      filter(code_pays == country) %>%
      pull(nom_pays)
    
    # si le pays n'est pas trouvé, on garde le code
    if (length(nom_pays_affiche) == 0) nom_pays_affiche <- country
  } else {
    nom_pays_affiche <- country
  }
  
  # --- Construire le titre avec le nom complet du pays ---
  title <- paste0("Comparaison du % de changement de chaque indicateur environnemental par régime alimentaire en ", nom_pays_affiche)
  
  
  # palette simple si colors.scenario n’existe pas
  if (!exists("colors.scenario")) {
    colors.scenario <- c(
      "BMK" = "grey70", "ani-25" = "#ffb3b3", "ani-50" = "#ff6666", "ani-75" = "#ff1a1a", "ani-100" = "#b30000",
      "kcal-25" = "#dab3ff", "kcal-50" = "#b366ff", "kcal-75" = "#8000ff", "kcal-100" = "#4b0082",
      "FLX" = "#6ebf8b", "PSC" = "#32a852", "VEG" = "#26734d", "VGN" = "#145a32"
    )
  }
  
  
  
  # Fonction interne pour générer 1 graphe par indicateur
  by.env <- function(domain) {
    df <- env_new %>%
      filter(code_pays == country,
             .data$item == !!item,
             .data$diet.scenario %in% regimes,
             !is.na(.data[[domain]])) %>%
      group_by(diet.scenario) %>%
      summarise(value = mean(.data[[domain]], na.rm = TRUE), .groups = "drop") %>%
      mutate(diet.scenario = factor(diet.scenario, levels = bar_order))
    
    if (nrow(df) == 0) return(NULL)
    
    y_min <- min(df$value, na.rm = TRUE)
    y_max <- max(df$value, na.rm = TRUE)
    
    g <- ggplot(df, aes(x = factor(diet.scenario, levels = bar_order),
                        y = value,
                        fill = factor(diet.scenario, levels = legend_order))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(value, 1)),
                size = 3, vjust = ifelse(df$value >= 0, -0.3, 1.2),
                color = "black") +
      scale_fill_manual(
        name = "Régime alimentaire", 
        values = colors.scenario
      ) +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      labs(
        title = domain,
        y = "% de changement par rapport à BMK",
        x = "Régime alimentaire"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))
    
    
    return(g)
  }
  
  # --- Créer un graphe pour chaque indicateur environnemental ---
  graphics <- lapply(domains, FUN = by.env)
  graphics <- graphics[!vapply(graphics, is.null, logical(1))]  # supprimer les graphiques vides
  
  # --- Assembler les graphiques avec patchwork ---
  wrap_plots(graphics, ncol = ncol, guides = "collect") +
    plot_annotation(title = title) &
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "white")
    )
}

##########################








##############################################################################
# Exemples d’utilisation
##############################################################################

env_partition()
env_partition(domains = "water", ncol = 1)
env_partition(domains = "GHGe", ncol = 1)
env_partition(domains = "nitr", ncol = 1)

env_partition(domains = c("GHGe", "land", "water"))

env_partition(country = "DEU")



  


