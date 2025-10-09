##############################################################################
# FUNCTION : PRINT ENVIRONMENTAL DATA TABLE
##############################################################################


print.env <- function(data = env_new) {
  library(DT)
  
  # Arrondir les colonnes numériques
  num_cols <- sapply(data, is.numeric)
  data[, num_cols] <- round(data[, num_cols], 2)
  data[[ncol(data)]] <- data$phos
  
  # Création du tableau
  tab <- DT::datatable(
    data,
    rownames = FALSE,
    colnames = c(
      "socio-econ scenario",
      "Item",
      "Diet scenario",
      "Country code",
      "Environmental domain",
      "Greenhouse-gas emission (GHGe, MtCO2-eq)",
      "Cropland use (land, 1000 km²)",
      "Bluewater use (water, km³)",
      "Nitrogen application (nitr, GgN)",
      "Phosphorus application (phos, GgP)"
    )[1:ncol(data)],
    
    filter = 'top',
    options = list(
      lengthMenu = c(5, 10, 15, 20),
      ordering = TRUE,
      autoWidth = TRUE,
      scrollX = TRUE
    )
  )
  
  return(tab)
}
