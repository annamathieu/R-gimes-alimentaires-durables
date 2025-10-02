
library(tidyverse)
library(dplyr)
library(tidyr)



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






