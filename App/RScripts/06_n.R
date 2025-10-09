##############################################################################
# FUNCTION : Simple MFA (base version)
##############################################################################

mfa_simple <- function(country = "FRP", selectvar) {

  # creation du jeu de données data.mfa
  data.mfa <- cbind(
    nutri_new %>% 
      filter(item == "abs", code_pays == country, diet.scenario != "BMK") %>% 
      select(-item, -code_pays, -grp_diet),
    
    env_new %>% 
      filter(item == "abs", code_pays == country, socio.econ.scenario == "SSP2", diet.scenario != "BMK") %>% 
      select(-item, -code_pays, -socio.econ.scenario, -grp_diet, -diet.scenario),
    
    sante_new %>% 
      filter(parameter == "deaths_avd", disease == "all-c", code_pays == country) %>% 
      select(-grp_diet, -diet.scenario, -disease, -parameter, -code_pays, -`all-rf`)
  )
  
  # # Vérification 
  # if (nrow(data.mfa) == 0) {
  #   return(ggplot() + annotate("text", x = 0.5, y = 0.5,
  #                              label = paste("No data for", country),
  #                              size = 6, color = "red") + theme_void())
  # }
  
  #  Analyse MFA brute 
  res.mfa.ns <- MFA(data.mfa,
                    group = c(1, 24, 5, 9),
                    type = c("n", "s", "s", "s"),
                    name.group = c("diet", "nutritional", "environmental", "health"),
                    num.group.sup = 1, 
                    graph = F)
  
  # # Graphiques clés 
  # g1 <- fviz_mfa_ind(res.mfa.ns, repel = TRUE, axes = c(1, 2), habillage = 1, col.ind = ) +
  #   labs(title = paste("Individuals factor map -", country)) +
  #   theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  #   
    
    ###### graph des individus #######
  g1 <- plot.MFA(res.mfa.ns, choix = "ind",
                 habillage = "ind",               # habillage basé sur la colonne 'diet scenario'
                 lab.ind =  FALSE,
                 col.hab = c("olivedrab2","chartreuse3", "forestgreen", "darkgreen", 
                             "brown4", "indianred3", "pink3", "pink1",
                             "orange", "darkorange2","darkorange3","darkorange4"),
                 invisible = "quali.sup",
                 
                 graph.type = "ggplot")
  
  g1 <- g1 + geom_text(aes(x= (res.mfa.ns$ind$coord[,1]),
                     y = (res.mfa.ns$ind$coord[,2])+0.09), label = c("FLX","PSC","VEG",'VGN',"ani-25","ani-50","ani-75","ani-100","kcal-25","kcal-50","kcal-75","kcal-100"),
                 col = c("olivedrab2","chartreuse3", "forestgreen", "darkgreen",
                         "brown4", "indianred3", "pink3", "pink",
                         "orange", "darkorange2","darkorange3","darkorange4"),
                 size = 4, 
                 show.legend = T) +
    
    labs(title = "Graph of individuals", col = "Diet") +
    theme(
      plot.title = element_text(size = 16, hjust = 0.55),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 11)
    )

  

    # GRAPH DES VARIABLES 
  
  g2 <- plot.MFA(res.mfa.ns, choix = "var",
                 select = selectvar,
                 lab.ind =  FALSE,
                 graph.type = "ggplot")
  
  
  g2 <- g2 + labs(title = "Graph of variables") +
    theme(
      plot.title = element_text(size = 1, hjust = 0.55),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 11)
    )
    
    
  
  # graph des groupes
  g3 <- plot.MFA(res.mfa.ns, choix = 'group')
  

  # graph des corrélations
  g4 <- plot.MFA(res.mfa.ns, choix = 'axes')
  
  
  return(list(g1,g2,g3,g4))
  
}


res <- mfa_simple(selectvar = names(data.mfa))
