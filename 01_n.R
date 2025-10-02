
# Création du jeu de données final 

library(tidyverse)
library(dplyr)
library(tidyr)

# Importation des trois tableaux 
nutri <- read.table(file = "data_csv/nutri.csv", header = T, sep =";", stringsAsFactors = T, dec = ",")
sante <- read.table(file = "data_csv/sante.csv", header = T, sep =";", stringsAsFactors = T, dec = ",")
env <- read.table(file = "data_csv/env.csv", header = T, sep =";", stringsAsFactors = T, dec = ",")
conti<- read.table(file = "data_csv/conti.csv", header = T, sep=",", stringsAsFactors = T)

# Vérification des data types 
str(nutri)
str(sante)
str(env)
str(conti)


# Ajout des variable nom pays région éco continent au jeu de données 
# suppression des régions : tout + régions eco seules + régions géo eco 


# nutri_new <- left_join(nutri, conti, by = c("region" = "code_pays"))
nutri_new <- nutri[-which(nutri$region %in% c("LMC",'UMC','HIC','LIC', 'all-r')) , ]
nutri_new <- nutri_new[-ends_with( "LMIC", vars = nutri_new$region) , ]

nutri_new <- rename(nutri_new, 
                    code_pays=region) 

# pivot wider de nutri 
nutri_new <- pivot_wider(data = nutri_new, names_from = nutrient, values_from = value)
nutri_new <- nutri_new[-which(nutri_new$stats %in% c("low", "high")) , ]   # garder que les moyennes
nutri_new <- nutri_new[,-3] # supprimer colonne stats (1 seul level)


head(nutri_new)


##############################################################
library(ggplot2)
library(plotly)

p <- ggplot(
  data = subset(nutri_new, code_pays == "FRP" & item == "%rec"),
  aes(x = diet.scenario, 
      y = protein, 
      fill = protein,
      text = paste0("Régime: ", diet.scenario,
                    "<br>Protéine: ", round(protein,1), "%"))) +  # tooltip
  geom_col(width = 0.7) +
  geom_hline(yintercept = 100, 
             linetype = "solid", 
             color = "red", 
             size = 1) +
  scale_fill_gradient(low = "plum1", high = "purple4") +
  labs(
    title = "Apport en protéines selon le régime alimentaire (France)",
    x = "Régime Alimentaire",
    y = "% de la recommandation"
  ) +
  theme_minimal(base_size = 10) +    # texte un peu plus grand
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12), # titre centré et gras
    axis.title = element_text(face = "bold"),   # titres en gras
    panel.grid.major.x = element_blank(),       # enlève les grilles verticales
    panel.grid.minor = element_blank()
  )

# rendre interactif
ggplotly(p, tooltip = "text")




#############################################################
#Fonction généralisée sur les pays et les nutriments : 
#############################################################



nutri_plot <- function(nutri_new, code_pays = NULL, nutrient = NULL) {
  
  # filtrer selon les inputs
  if (!is.null(code_pays)) {
    nutri_new <- nutri_new[nutri_new$code_pays == code_pays, ]
  }
  if (!is.null(nutrient)) {
    nutri_new <- nutri_new[nutri_new$item == "%rec", ]   # garder seulement %rec
  }
  
  # graphique
  p <- ggplot(nutri_new, aes(x = diet.scenario,
                      y = .data[[nutrient]],
                      fill = .data[[nutrient]],
                      text = paste0("Régime: ", diet.scenario,
                                    "<br>", nutrient, ": ", round(.data[[nutrient]],1), "%"))) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 100, linetype = "solid", color = "red", size = 1) +
    scale_fill_gradient(low = "plum1", high = "purple4") +
    labs(
      title = paste("Apport en", nutrient, "selon le régime alimentaire (", code_pays, ")"),
      x = "Régime Alimentaire",
      y = "% de la recommandation"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      axis.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(ggplotly(p, tooltip = "text"))
}

#######
#######Exemples d'utilisation 
#############################

# France + protéines
nutri_plot(nutri_new, code_pays = "FRP", nutrient = "protein")

# Allemagne + calories
nutri_plot(nutri_new, code_pays = "AFG", nutrient = "vitaminC")






