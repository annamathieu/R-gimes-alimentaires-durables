### PCA SUR LES DONNEES DE NUTRITION 
# ONGLET DE LA PAGE " NUTRITION " 

# jeu de données nutri_new 


  pca_data_nutri <- nutri_new %>% filter(item == "abs") 
  pca_data_nutri <- pca_data_nutri[,-c(1:2)]

  pca_res_nutri <- PCA(X = pca_data_nutri, 
                       quali.sup = c(1,2,27))
  
  plot.PCA(pca_res_nutri, choix = "ind", habillage = 2, label = "none", legend = F )
  
  
  
pca.nutri <- function () {
  

  
  
  
}