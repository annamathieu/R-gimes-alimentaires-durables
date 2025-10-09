#####----------------------------------------------------------------------#####
##    FUNCTION : PCA DATA HEALTH
#####----------------------------------------------------------------------#####

pca.disease <- function(data, country = "FRP") {
  
  # Sélection des données pour le pays choisi
  pca_data_disease <- data[data$code_pays == country, ]
  pca_data_disease <- pca_data_disease[,-2]
  
  # ACP
  pca_res_disease <- PCA(X = pca_data_disease,
                         quali.sup = c(1),   
                         graph = FALSE)
  
  
  colors.scenario <- c("mistyrose",  # ani-100
                       "brown4",        # ani 25
                       "indianred3",    # ani 50
                       "pink2",          # ani 75
                       "olivedrab2",  # FLX
                       "darkorange4", # kcal 100
                       "orange",      # kcal 25
                       "darkorange2",  # kcal 50
                       "darkorange3",  # kcal 75
                       
                       "chartreuse3", # "PSC"            
                       "forestgreen", # "VEG"
                       "darkgreen")   # "VGN"
  
  
  names(colors.scenario) = c("ani-100", "ani-25", "ani-50", "ani-75", 
                             "FLX", 
                             "kcal-100", "kcal-25", 
                             "kcal-50" , "kcal-75", 
                             "PSC", "VEG", "VGN") 
  
  # Graphique des individus
  pca_res_disease <- PCA(X = pca_data_disease,
                         quali.sup = c(1),   # la première colonne est diet.scenario (facteur)
                         graph = FALSE)
  
  graph.ind <- plot.PCA(pca_res_disease, choix = "ind", 
                        habillage = "1",                  # basé sur diet.scenario
                        col.hab = colors.scenario,       # vecteur de couleurs défini dans global
                        label = "none",
                        invisible = "quali", 
                        graph.type = "ggplot")
  
  graph.ind <- graph.ind + 
    geom_text(aes(x = pca_res_disease$ind$coord[,1], y =  pca_res_disease$ind$coord[,2] + 0.25), 
              label = c("FLX","PSC","VEG",'VGN',"ani-25","ani-50","ani-75","ani-100","kcal-25","kcal-50","kcal-75","kcal-100"), 
              col = c("olivedrab2","chartreuse3", "forestgreen", "darkgreen", 
                      "brown4", "indianred3", "pink3", "pink1",
                      "orange", "darkorange2","darkorange3","darkorange4"), 
              size = 5, show.legend = FALSE) +
    labs(title = "Graph of individuals - Disease PCA", col = "Diet") +
    theme(
      legend.title = element_text(size = 14), 
      legend.text = element_text(size = 13), 
      text = element_text(size = 13))
  
  
  # Graphique des variables
  labs(title = "Graph of individuals", 
       col = "Diet") +
    
    theme(title = element_text(size = 20), 
          legend.title = element_text(size=14), 
          legend.text = element_text(size=13), 
          text = element_text(size = 13))
  
  
  # un vecteur de couleurs pour les variables 
  cols <- c(
    "coronary_heart_disease" = "#E41A1C",
    "stroke" = "#984EA3",
    "cancer" = "#4D4D4D",
    "type_2_diabetes" = "#377EB8",
    "other" = "#4DAF4A"
  )
  
  
  
  # graph of variables
  graph.v <- plot.PCA(pca_res_disease, choix = "var", 
                      select = c("coronary_heart_disease", "stroke", "cancer", "type_2_diabetes", "other"), 
                      col.var = cols, 
                      graph.type = "ggplot", 
                      ggoptions = (list(size = 5) ))
  
  graph.v <- graph.v + 
    labs(title = "Graph of variables") +
    theme(title = element_text(size = 20), 
          text = element_text(size = 12))
  
  return(list(graph.ind, graph.v))
}



#pca.disease(data_disease, country = "FRP")



















