# Visualisations des régimes alimentaires et leurs impacts nutritionnels 

# % de couverture des apports nutritionnels par les régimes alimentaires 
# %rec , on va regarder que sur un pays à la fois 

# jeu de données à utiliser : nutri_new 

# chargement des librairies (dans Global)
# library(ggplot2)
# library(dplyr)
# library(tidyverse)
# library(tidyr)
# library(patchwork)
# library(purrr)


######################################################################
# DEF COULEURS GRP DIET 
######################################################################

levels(nutri_new$diet.scenario)

colors.scenario <- c("mistyrose",  # ani-100
                     "brown4",        # ani 25
                     "indianred3",    # ani 50
                     "pink2",          # ani 75
                     "gray28",     # BMK 
                     "olivedrab2",  # FLX
                     "darkorange4", # kcal 100
                     "orange",      # kcal 25
                     "darkorange2",  # kcal 50
                     "darkorange3",  # kcal 75

                     "chartreuse3", "forestgreen", "darkgreen") # "PSC"      "VEG"      "VGN" 


names(colors.scenario) = c("ani-100", "ani-25", "ani-50", "ani-75", 
                           "BMK", 
                           "FLX", 
                           "kcal-100", "kcal-25", 
                           "kcal-50" , "kcal-75", 
                           "PSC", "VEG", "VGN"
                           )


######################################################################
# ALL DIET
######################################################################

# tous les régimes alimentaires simultanément POUR UN NUTRIMENT 

all.diet <- function(nutrim = "protein", country = "FRP", ) {
  
  # filter sur pays; nutriments 
  nutri_new %>% filter((code_pays==country) & (nutri_new$item=="%rec"))
  
  #définition des axes y min et max 
  y_min <- min(nutri_new[[nutrim]]) - 10   # axe Y min (si < 0)
  y_max <- max(nutri_new[[nutrim]]) + 10   # axe y min pour si > à 100 %
  
  nutri_new %>% 
    ggplot() +
    aes(diet.scenario, .data[[nutrim]], fill = diet.scenario, group = grp_diet) +
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
          title = element_text(size = 9), 
          legend = F) +
    xlab ("Régime alimentaire") +
    geom_hline(yintercept = 100, 
               linetype = "dashed", 
               color = "black", 
               linewidth = 0.8)
  
  return(graph )
}

all.diet(nutrim = "vitaminB12")

######################################################################














######################################################################
# PARTITION DIET 
########################################################################################

partition.bydiet_f <- function(nutriments = names(nutri_new[, 5:14]),              # choix des nutriments
                                   country = "FRP",                                     # choix du pays 
                                   regimes = levels(nutri_new$diet.scenario),          # choix des diets 
                                   ncol = 3,                                            # nb de colonnes du patchwork (3 par défaut )
                                   title = "Comparaison du % de couverture de chaque nutriment par régime alimentaire") {
  
  # ordre de la légende et des bars 
  bar_order <- c("BMK", "ani-25", "ani-50", "ani-75", "ani-100", "kcal-25","kcal-50", "kcal-75", "kcal-100", "FLX", "PSC","VEG","VGN")
  legend_order <- c("BMK", "ani-25", "ani-50", "ani-75", "ani-100", "kcal-25","kcal-50", "kcal-75", "kcal-100", "FLX", "PSC","VEG","VGN")
  
  # filter pays, %rec, diets 
  nutri_new <- nutri_new %>% 
    filter((code_pays==country) & (nutri_new$item=="%rec") & (nutri_new$diet.scenario %in% regimes))
  
  
  # graph par nutri 
  by.diet <- function(nutrim) {
    
    if (all(is.na(nutri_new[[nutrim]]))) {
      return(NULL)  # renvoie NULL si pas de données
    }
    
    y_min <- min(nutri_new[[nutrim]]) - 10   # axe Y min (si < 0)
    y_max <- max(nutri_new[[nutrim]]) + 10   # axe y min pour si > à 100 %
    
    graph <- nutri_new %>% 
      ggplot() +
      aes(x =factor(diet.scenario, levels = bar_order), 
          .data[[nutrim]], 
          fill = factor(diet.scenario, levels = legend_order), group = grp_diet) +
      geom_bar(stat= "identity", position = position_dodge())+
      geom_text(aes(y = .data[[nutrim]], 
                    label = round(.data[[nutrim]],0),
                    group =grp_diet), 
                color = "black", 
                size = 4,
                vjust = 1) +
      # labs(fill = "Groupe de \nscénario \nrégime \nalimentaire", 
      # title = "% de couverture des protéines par le régime alimentaire \npar rapport aux recommandations nutritionnelles") + 
      theme(text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            title = element_text(size = 7), 
            axis.title.y = element_text(size = 11, face = "bold"),
            legend.position = "none") +
      scale_y_continuous(limits = c(ifelse(y_min<0,y_min,0), ifelse(y_max>100,y_max,100))) +
      scale_fill_manual(values = colors.scenario) +
      # xlab ("Régime alimentaire") +
      geom_hline(yintercept = 100, 
                 linetype = "dashed", 
                 color = "black", 
                 linewidth = 0.8) +
      labs(fill = "Groupe de \nscénario \nrégime \nalimentaire") 
    
    return(graph)
  }
  
  
  #Création de tous les graphiques pour tous les nutri sélectionnés 
  graphics <- lapply(nutriments, FUN = by.diet)
  graphics <- graphics[!vapply(graphics, is.null, logical(1))] # supprimer les graphiques vides
  
  
  # Assembler avec patchwork et légende commune
  wrap_plots(graphics, ncol = ncol, guides = "collect") +
    plot_annotation(title = title) & 
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.title.x = element_blank(),
          legend.text = element_text(size = 7), 
          panel.background = element_rect(fill = "white")) 
  

  }

# test partition diet 
# partition.bydiet_f(ncol = 2)  
# partition.bydiet_f(regimes = c("BMK", "ani-25", "ani-50", "ani-75", "ani-100"), ncol = 3)
# partition.bydiet_f(regimes = "BMK")

  
###############################################################################  




