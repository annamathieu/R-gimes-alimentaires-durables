# Visualisations des régimes alimentaires et leurs impacts nutritionnels 

# % de couverture des apports nutritionnels par les régimes alimentaires 
# %rec , on va regarder que sur un pays à la fois 

# jeu de données à utiliser : nutri_new 

# chargement des librairies
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(patchwork)
library(purrr)

nutri_new <- as.data.frame(nutri_new)

# ajout d'une colonne "grp_diet" de classe de type de scénario de diet 
nutri_new <- nutri_new %>%
  mutate(grp_diet = case_when(
    diet.scenario %in% c("FLX", "PSC", "VEG", "VGN") ~ "vg",
    diet.scenario %in% c("ani-25", "ani-50", "ani-75", "ani-100") ~ "ani", 
    diet.scenario %in% c("kcal-25", "kcal-50", "kcal-75", "kcal-100") ~ "kcal", 
    TRUE ~ "bmk"))

# tous les régimes alimentaires simultanément POUR UN NUTRIMENT 

all.diet <- function(nutrim = "protein") {
  
  graph <- nutri_new %>% 
    filter((code_pays=="FRP") & (nutri_new$item=="%rec")) %>% 
    ggplot() +
    aes(fct_reorder(diet.scenario, .data[[nutrim]]), .data[[nutrim]], fill = grp_diet, group = grp_diet) +
    geom_bar(stat= "identity", position = position_dodge())+
    geom_text(aes(y = .data[[nutrim]], 
                  label = round(.data[[nutrim]],0),
                  group =grp_diet), 
              color = "black", 
              size = 4,
              vjust = 1) +
    labs(fill = "Groupe de \nscénario \nrégime \nalimentaire", 
         title = "% de couverture des protéines par le régime alimentaire \npar rapport aux recommandations nutritionnelles") + 
    theme(text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1),
          title = element_text(size = 9)) +
    xlab ("Régime alimentaire") +
    geom_hline(yintercept = 100, 
               linetype = "dashed", 
               color = "black", 
               linewidth = 0.8)
  
  return(graph )
}

all.diet()
all.diet("vitaminB12")


# Fonction de combinaison de graph 
combine.graph <- function (nutriments) {
  for (i in 1:length(nutriments)){
    res[[i]] <- all.diet(nutriments[i])
  }
    
  
  
}


graphs <- map(nutriments, ~ by.diet(diet = "FLX", diet2 = "BMK", nutrim = .x))

# Combiner avec patchwork
combined_graph <- wrap_plots(graphs, ncol = 2)  # 2 colonnes par exemple
combined_graph


# POUR un SEUL régime à la fois et tous les nutriments 

by.diet <- function(diet="FLX", diet2 = "BMK", nutrim = "protein") {
  
  y_min <- min(nutri_new[[nutrim]], na.rm = TRUE)
  y_min <- y_min - 5
  
  graph <- nutri_new %>% 
    filter((code_pays=="FRP") & (nutri_new$item=="%rec") & (nutri_new$diet.scenario %in% c(diet, diet2)))  %>% 
    ggplot() +
    aes(diet.scenario, .data[[nutrim]], fill = grp_diet, group = grp_diet) +
    geom_bar(stat= "identity", position = position_dodge())+
    geom_text(aes(y = .data[[nutrim]], 
                  label = round(.data[[nutrim]],0),
                  group =grp_diet), 
              color = "black", 
              size = 4,
              vjust = 1) +
    labs(fill = "Groupe de \nscénario \nrégime \nalimentaire", 
         title = "% de couverture des protéines par le régime alimentaire \npar rapport aux recommandations nutritionnelles") + 
    theme(text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1),
          title = element_text(size = 9)) +
    scale_y_continuous(limits = c(y_min, 100)) +
    xlab ("Régime alimentaire") +
    geom_hline(yintercept = 100, 
               linetype = "dashed", 
               color = "black", 
               linewidth = 0.8) 
  
  return(graph)
  
}

by.diet(diet="VGN", diet2 = "BMK", nutrim = "vitaminB12")  
# pb marge   
  