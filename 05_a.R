# affichage des données 


# chargement des libraires
library(DT)



nutri_table <- nutri_new

# Arrondir les valeurs à l'unité si colonnes type numériK 
# num_cols <- sapply(nutri_table, is.numeric)

nutri_table[, 4:27] <- round(nutri_table[, 4:27], 0)





print.nutri <- function() {
  
  nutri_table <- nutri_new
  
  # Arrondir les valeurs à l'unité si colonnes type numériK 
  # num_cols <- sapply(nutri_table, is.numeric)
  
  nutri_table[, 4:27] <- round(nutri_table[, 4:27], 0)
  
  tab <- DT::datatable(
    nutri_table,
    rownames = FALSE,
    colnames = c("Item","Diet scenario","Country code","Calories (kcal)","Protein (g)","Carbohydrates (g)","Fat (g)","Saturated Fatty Acids (g)", "Mono-unsaturated Fatty Acids (g)","Poly-unsaturated Fatty Acids (g)","Vitamin C (mg)",
    "Vitamin A (µg)","Folate (µg)","Calcium (mg)","Iron (mg)","Zinc (mg)","Potassium (mg)","Fiber (g)","Copper (mg)","Phosphorus (mg)","Thiamin(mg)","Riboflavin (mg)","Niacin (mg)","Vitamin B6 (mg)", "Magnesium (mg)","Pantothenate (mg)","Vitamin B12 (µg)","Diet Group"),
    
    filter = 'top',
    options = list(
      # pageLength = 20,   #le nombre d’observations affichées
      lengthMenu = c(5,10,15,20), # choix du nombre d'observations affichées 
      ordering = TRUE, # permet à l'utilisateur de trier les obs par les c
      # searching = TRUE , # autorise filtering
      autoWidth = TRUE 
    )
  )
  
  return (tab)
  
}

print.nutri()

names(nutri_new)













