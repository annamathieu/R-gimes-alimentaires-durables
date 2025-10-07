  
#####----------------------------------------------------------------------#####
##                       Chargement des packages                              ##
#####----------------------------------------------------------------------#####

library(tidyverse)          # %>% 
library(dplyr)              # manipulation de données 
library(tidyr)              # manipulation de tibbles ; opérations de join 
library(sf)                 # cartes interactives et manipulation d'objets sf 
library(rnaturalearth)      # fournit données géographiques
library(leaflet)            # interactives maps
library(rnaturalearthdata)

library(ggplot2)            # pour faire des plots 
library(RColorBrewer)       # palettes de couleur 
library(patchwork)          # pour afficher des patchwork de graph 
library(purrr)

library(FactoMineR)         # analyses factorielles 
library(factoextra)         # personnalisation graphiques d'analyse factorielle 

library(DT)                 # Data tables 

#####----------------------------------------------------------------------#####



###--- Sauvegarde des jeu de données au format RDS ---###

# if (!dir.exists("R-gimes-alimentaires-durables/data")) {
#   dir.create("R-gimes-alimentaires-durables/data")
# }
# saveRDS(nutri_new, "R-gimes-alimentaires-durables/data/nutri_new.rds")
# saveRDS(sante_new, "R-gimes-alimentaires-durables/data/sante_new.rds")
# saveRDS(env_new, "R-gimes-alimentaires-durables/data/env_new.rds")
# saveRDS(conti, "R-gimes-alimentaires-durables/data/conti.rds")



#####----------------------------------------------------------------------#####
##                   Chargement des jeu de données                            ##
#####----------------------------------------------------------------------#####

nutri_new <- readRDS(file = "data/nutri_new.rds")
sante_new <- readRDS(file = "data/sante_new.rds")
env_new <- readRDS(file = "data/env_new.rds")
conti<- readRDS(file = "data/conti.rds")

#####----------------------------------------------------------------------#####



# variables globales

world <- ne_countries(scale = "medium", returnclass = "sf")
world$region_un <- as.factor(world$region_un)



#####----------------------------------------------------------------------#####
##   Création de colonne de correspondance en Anglais pour les continents     ##
#####----------------------------------------------------------------------#####

# Table de correspondance
continent_noms <- c(
  "AFR" = "Africa",
  "AMR" = "Americas",
  "EMR" = "Eastern Mediterranean",
  "EUR" = "Europe",
  "SEA" = "South-East Asia",
  "WPR" = "Western Pacific"
)

# Ajouter la colonne avec les noms complets
conti$nom_continent <- continent_noms[as.character(conti$continent)]

#####----------------------------------------------------------------------#####



#####----------------------------------------------------------------------#####
##   Chargement des scripts avec les fonctions depuis le dossier RScripts     ##
#####----------------------------------------------------------------------#####

source("RScripts/carto.R")    # fonction carto
source("RScripts/03_a.R")     # fonction pour avoir les graphs nutritionnels 
source("RScripts/05_a.R")     # fct DATA TABLE NUTRI 


#####----------------------------------------------------------------------#####



#####----------------------------------------------------------------------#####
##                Definition palette de couleur par scenario                  ##
#####----------------------------------------------------------------------#####


# couleurs 
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
                           "PSC", "VEG", "VGN") 



