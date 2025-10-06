
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
library(patchwork)          # pour afficher des patchwork de graph 
library(purrr)

library(FactoMineR)         # analyses factorielles 
library(factoextra)         # personnalisation de graphiques d'analyse factorielle 

library(DT)                 # Data tables 

world <- ne_countries(scale = "medium", returnclass = "sf")
world$region_un <- as.factor(world$region_un)

nutri_new <- read.table(file = "data_csv/nutri_new.csv", header = T, sep =",", stringsAsFactors = T, dec = ".")
sante_new <- read.table(file = "data_csv/sante_new.csv", header = T, sep =",", stringsAsFactors = T, dec = ".")
env_new <- read.table(file = "data_csv/env_new.csv", header = T, sep =",", stringsAsFactors = T, dec = ".")
conti<- read.table(file = "data_csv/conti.csv", header = T, sep=",", stringsAsFactors = T)

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

