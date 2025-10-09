##############################################################################
# FUNCTION : PRINT HEALTH DATA TABLE
##############################################################################

print.sante <- function(data) {
  library(DT)
  
  num_cols <- sapply(data, is.numeric)
  data[, num_cols] <- round(data[, num_cols], 0)
  
  datatable(
    data,
    options = list(
      scrollX = TRUE,        # défilement horizontal si beaucoup de colonnes
      pageLength = 10,       # nb de lignes par page
      autoWidth = TRUE,
      lengthMenu = c(10, 20, 50, 100)
    ),
    rownames = FALSE,
    filter = "top",          # ajout d’un filtre interactif en haut des colonnes
    caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: center; font-weight: bold; font-size: 18px; color: black;',
      "Health dataset"
    )
  )
}


