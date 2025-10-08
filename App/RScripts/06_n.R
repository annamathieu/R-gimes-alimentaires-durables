##############################################################################
# FUNCTION : Simple MFA (base version)
##############################################################################

mfa_simple <- function(country = "FRP") {
  library(FactoMineR)
  library(factoextra)
  library(dplyr)
  library(patchwork)
  library(ggplot2)
  
  #Création du jeu de données
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
  
  # Vérification 
  if (nrow(data.mfa) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                               label = paste("No data for", country),
                               size = 6, color = "red") + theme_void())
  }
  
  #  Analyse MFA brute 
  res.mfa.ns <- MFA(data.mfa,
                    group = c(1, 24, 5, 9),
                    type = c("n", "s", "s", "s"),
                    name.group = c("diet", "nutritional", "environmental", "health"),
                    num.group.sup = 1)
  
  # Graphiques clés 
  g1 <- fviz_mfa_ind(res.mfa.ns, repel = TRUE, axes = c(1, 2)) +
    labs(title = paste("Individuals factor map -", country)) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  g2 <- fviz_mfa_var(res.mfa.ns, "group", axes = c(1, 2)) +
    labs(title = "Groups representation") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  g3 <- fviz_mfa_var(res.mfa.ns, "quanti.var", repel = TRUE, axes = c(1, 2)) +
    labs(title = "Correlation circle (quantitative variables)") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  g4 <- fviz_mfa_ind(res.mfa.ns, partial = "all", repel = TRUE, axes = c(1, 2)) +
    labs(title = "Individuals with partial groups") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  #Combinaison harmonisée (comme ACP)
  combined <- (g1 | g2) / (g3 | g4)
  
  
  return(combined)
}
