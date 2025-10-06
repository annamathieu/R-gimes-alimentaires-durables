
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


# fonction pour avoir les graphs nutritionnels 
source("RScripts/03_a.R")
