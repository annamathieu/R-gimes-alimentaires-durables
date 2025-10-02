# Visualisation 
# carte des pays inclus dans l'étude! 


# Fonds de cartes 


# librairies 
library(sf)
library(rnaturalearth)      # fournit données géographiques
library(leaflet)            # interactives maps
library(rnaturalearthdata)
library(ggplot2)
library(tidyr)              # pour les join            
library(RColorBrewer)       # palettes de couleur 

#####################################################################
# carte du monde
# world_map <- map_data("world")
# world_map$region <- as.factor(world_map$region)
# levels(world_map$region)

# # left join données et MAP
# countries_map <- left_join(world_map, nutri_new, 
#                            by = c("region" = "nom_pays")) # adjoindre les infos etat par etat 
# 
# countries_map$region <- as.factor(countries_map$region)

#####################################################################




studiedcountries <- function(data = world_new, conti, country = NULL, reg_eco = NULL, contin= NULL) {
  # objet de la classe sf 
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # filtre sur les conti, pays, r eco 
  if (!is.null(country)) {f_conti <- conti %>% filter(nom_pays %in% country) }
  else if (!is.null(reg_eco)) {f_conti <- conti %>% filter(reg_eco %in% r_eco) }
  else if (!is.null(contin)) {f_conti <- conti %>% filter(conti$continent %in% contin) }
  else { f_conti <- conti }
  

  # fusion avec les données de continent et R eco 
  world_new <- left_join(world, f_conti, by = c("iso_a3_eh" = "code_pays")) 
  
  my_colors <- c("forestgreen","firebrick3", "gold2", "yellowgreen" )
  pal <- colorFactor(palette=my_colors, domain=world_new$r_eco)
  

  map <- leaflet(world_new) %>%
    addTiles() %>%                      # Fond OpenStreetMap
    addPolygons(
      fillColor = ~ pal(r_eco),
      color = "#444444",                # Couleur du contour
      weight = 1,                       # Épaisseur
      smoothFactor = 0.2,
      fillOpacity = 0.3,
      popup       = ~paste0("<b>", name, "</b><br>Niveau économique: ", r_eco)      # Popup : nom du pays
    ) %>% 
    addLegend("bottomright",
              values = ~r_eco,
              title = "Région économique",
              colors   = pal(c("HIC","LIC","LMC", "UMC"))[c(1,4,3,2)],  # ordre des facteurs couleur palette
              labels   = c("Pays à hauts revenus",
                           "Pays à revenus moyens-hauts",
                           "Pays à revenus moyens-bas", 
                           "Pays à revenus faibles"))                   # ordre des facteurs légende inversé
              

  return(map) 
}


studiedcountries <- function(conti, country = NULL, reg_eco = NULL, contin= NULL) {
  # objet de la classe sf 
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world$region_un <- as.factor(world$region_un)
  
  # filtre sur les conti, pays, r eco 
  if (!is.null(country)) {f_conti <- conti %>% filter(nom_pays %in% country) }
  else if (!is.null(reg_eco)) {f_conti <- conti %>% filter(r_eco %in% reg_eco) }
  else { f_conti <- conti }
  
  if (!is.null(contin)) {world <- world %>% filter(region_un %in% contin) }
  
  
  # fusion avec les données de continent et R eco 
  world_new <- left_join(world, f_conti, by = c("iso_a3_eh" = "code_pays")) 
  
  my_colors <- c("forestgreen","firebrick3", "gold2", "yellowgreen" )
  pal <- colorFactor(palette=my_colors, domain=world_new$r_eco)
  
  
  map <- leaflet(world_new) %>%
    addTiles() %>%                      # Fond OpenStreetMap
    addPolygons(
      fillColor = ~ pal(world_new$r_eco),
      color = "#444444",                # Couleur du contour
      weight = 1,                       # Épaisseur
      smoothFactor = 0.2,
      fillOpacity = 0.3,
      popup       = ~paste0("<b>", name, "</b><br>Niveau économique: ", world_new$r_eco)      # Popup : nom du pays
    ) %>% 
    addLegend("bottomright",
              values = ~r_eco,
              title = "Région économique",
              colors   = pal(c("HIC","LIC","LMC", "UMC"))[c(1,4,3,2)],  # ordre des facteurs couleur palette
              labels   = c("Pays à hauts revenus",
                           "Pays à revenus moyens-hauts",
                           "Pays à revenus moyens-bas", 
                           "Pays à revenus faibles"))                   # ordre des facteurs légende inversé
  
  
  return(map) 
}

# # tests qui fonctionnent :)
studiedcountries(conti)                                         # appel classique de la fonction : marche 
studiedcountries(conti, contin = "Asia")                        # appel de la fonction avec affichage d'un seul continent
studiedcountries(conti, country = "France", contin = "Europe")  # visualiser un continent d'une région
studiedcountries(conti, contin = "Europe", reg_eco = "HIC")       # visualiser les pays d'une région éco d'un continent 
# 


