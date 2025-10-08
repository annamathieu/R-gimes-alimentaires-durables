##############################################################################
# FUNCTION : Simple MFA (base version)
##############################################################################

mfa_simple <- function(country = "FRP") {
  library(FactoMineR)
  library(factoextra)
  library(dplyr)
  
  # Création du jeu de données commun
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
  
  # Vérif
  print(paste("Nombre de lignes dans le jeu de données :", nrow(data.mfa)))
  
  # MFA
  res.mfa <- MFA(data.mfa,
                 group = c(1, 24, 5, 9),
                 type = c("n", "s", "s", "s"),
                 name.group = c("diet", "nutritional", "environmental", "health"),
                 num.group.sup = 1)
  

}

mfa_simple()
