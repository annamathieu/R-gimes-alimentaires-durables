studiedcountries <- function(conti, country = NULL, reg_eco = NULL, contin= NULL) {
  # objet de la classe sf 
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world$region_un <- as.factor(world$region_un)
  
  # filtre sur les conti, pays, r eco 
  f_conti <- conti
  
  if (!is.null(contin)) {
    f_conti <- f_conti %>% filter(nom_continent %in% contin)
  }
  if (!is.null(reg_eco)) {
    f_conti <- f_conti %>% filter(r_eco %in% reg_eco)
  }
  if (!is.null(country)) {
    f_conti <- f_conti %>% filter(nom_pays %in% country)
  }
  
  
  # fusion avec les données de continent et R eco 
  world_new <- left_join(world, f_conti, by = c("iso_a3_eh" = "code_pays")) 
  
  my_colors <- c("forestgreen","firebrick3", "gold2", "yellowgreen" )
  pal <- colorFactor(palette=my_colors, domain=world_new$r_eco)
  
  
  map <- leaflet(world_new, options = leafletOptions(minZoom = 2, maxZoom = 7)) %>%
                   addTiles(options = tileOptions(noWrap = TRUE)) %>%
                   setView(lng = 0, lat = 20, zoom = 2) %>%
    addTiles() %>%                      # Fond OpenStreetMap
    addPolygons(
      fillColor = ~ pal(world_new$r_eco),
      color = "#444444",                # Couleur du contour
      weight = 1,                       # Épaisseur
      smoothFactor = 0.2,
      fillOpacity = 0.3,
      popup       = ~paste0("<b>", name, "</b><br>
Economic level: ", world_new$r_eco)      # Popup : nom du pays
    ) %>% 
    addLegend("bottomright",
              values = ~r_eco,
              title = "Région économique",
              colors   = pal(c("HIC","LIC","LMC", "UMC"))[c(1,4,3,2)],  # ordre des facteurs couleur palette
              labels   = c("HIC: High-income countries ",
                           "UMC: Upper middle-income countries",
                           "LMC: Lower middle-income countries",
                           "LIC: Low-income countries"
                           ))                   # ordre des facteurs légende inversé
  
  
  return(map) 
}


#studiedcountries(conti, contin = "Asia")
#studiedcountries(conti, contin = "Africa")
