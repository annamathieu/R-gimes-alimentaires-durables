
# Librairies  

library(tidyverse)          # %>% 
library(dplyr)              # manipulation de données 
library(tidyr)              # manipulation de tibbles ; opérations de join 
library(sf)                 # cartes interactives et manipulation d'objets sf 
library(rnaturalearth)      # fournit données géographiques
library(leaflet)            # interactives maps
library(rnaturalearthdata)
library(ggplot2)            # pour faire des plots 
library(RColorBrewer)       # palettes de couleur 



# couleurs des diets 
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



# save data
# if (!dir.exists("R-gimes-alimentaires-durables/data")) {
#   dir.create("R-gimes-alimentaires-durables/data")
# }
# saveRDS(nutri_new, "R-gimes-alimentaires-durables/data/nutri_new.rds")
# saveRDS(sante_new, "R-gimes-alimentaires-durables/data/sante_new.rds")
# saveRDS(env_new, "R-gimes-alimentaires-durables/data/env_new.rds")
# saveRDS(conti, "R-gimes-alimentaires-durables/data/conti.rds")


# import data
nutri_new <- readRDS(file = "data/nutri_new.rds")
sante_new <- readRDS(file = "data/sante_new.rds")
env_new <- readRDS(file = "data/env_new.rds")
conti<- readRDS(file = "data/conti.rds")



# variables globales

world <- ne_countries(scale = "medium", returnclass = "sf")
world$region_un <- as.factor(world$region_un)

# Charger les fonctions le dossier RScripts
source("RScripts/carto.R")



