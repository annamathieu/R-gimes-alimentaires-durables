
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


world <- ne_countries(scale = "medium", returnclass = "sf")
world$region_un <- as.factor(world$region_un)

nutri_new <- read.table(file = "data_csv/nutri_new.csv", header = T, sep =",", stringsAsFactors = T, dec = ".")
sante_new <- read.table(file = "data_csv/sante_new.csv", header = T, sep =",", stringsAsFactors = T, dec = ".")
env_new <- read.table(file = "data_csv/env_new.csv", header = T, sep =",", stringsAsFactors = T, dec = ".")
conti<- read.table(file = "data_csv/conti.csv", header = T, sep=",", stringsAsFactors = T)


