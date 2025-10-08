### PCA SUR LES DONNEES DE NUTRITION 
# ONGLET DE LA PAGE " NUTRITION " 

# jeu de données nutri_new 

 
  
  
pca.nutri <- function (nutri_new, country = "FRP") {
  
  # # df to perform pca onto 
  # pca_data_nutri <- nutri_new %>% filter(item == "abs", code_pays=="FRP") 
  # pca_data_nutri <- pca_data_nutri[,-1]
  # pca_data_nutri <- pca_data_nutri[,-2]
  # pca_data_nutri <- pca_data_nutri[,-26]
  
  # PCA
  pca_res_nutri <- PCA(X = (nutri_new %>% filter(item == "abs", code_pays==country) %>% select(-code_pays,-item,-grp_diet)), 
                       quali.sup = c(1), graph = F)
  
  # graph of individuals PCA 
  graph.ind <- plot.PCA(pca_res_nutri, choix = "ind", 
                habillage = "1",               # habillage basé sur la colonne 'diet scenario' 
                col.hab = colors.scenario,   # on applique nos couleurs (vec dans global )
                label = "none",
                invisible = "quali", 
                graph.type = "ggplot")
  
  graph.ind <- graph.ind + 
    geom_text(aes(x= (pca_res_nutri$ind$coord[,1]), 
                  y = (pca_res_nutri$ind$coord[,2])+0.3), label = c("BMK","FLX","PSC","VEG",'VGN',"ani-25","ani-50","ani-75","ani-100","kcal-25","kcal-50","kcal-75","kcal-100"), 
              col = c("gray28","olivedrab2","chartreuse3", "forestgreen", "darkgreen", 
                      "brown4", "indianred3", "pink3", "pink1",
                      "orange", "darkorange2","darkorange3","darkorange4") ) +
    
    labs(title = "Graph of individuals", 
         col = "Diet") +
    
    theme(title = element_text(size = 20), 
          legend.title = element_text(size=14), 
          legend.text = element_text(size=13))
  
  
  # Crée un vecteur de couleurs : vert pour "fiber", rouge pour les autres
  cols <- c("orange","brown2","orange","orange",
            "orange","orange","orange",
            "green4",'green4',
            "black",            
            "brown2","brown2","brown2","black","green4","black","black","black","black",'black', "black",'black', "black", 
            "brown2")
  
  # graph of variables
  graph.v <- plot.PCA(pca_res_nutri, choix = "var", 
                      select = c("calories","protein","carbohydrates","fat", "vitaminC","vitaminA" ,"calcium","iron","zinc"  ,       
                                 "fiber","vitaminB12"), 
                      col.var = cols, 
                      graph.type = "ggplot") 
  
  graph.v <- graph.v + 
    labs(title = "Graph of variables") +
    theme(title = element_text(size = 20))
  
  return(list(graph.ind, graph.v))
  
}


# pca.nutri(nutri_new)
