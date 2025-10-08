# FMA 


# création du nouveau jeu de données 

data.mfa <- cbind(
  nutri_new %>% filter(item=="abs", code_pays=="FRP", diet.scenario!= "BMK") %>% select(-item, -code_pays, -grp_diet), 
  env_new %>% filter(item=="abs", code_pays=="FRP", socio.econ.scenario=="SSP2", diet.scenario!= "BMK") %>% select(-item, -code_pays, -socio.econ.scenario, -grp_diet, -diet.scenario), 
  sante_new %>% filter(parameter=="deaths_avd", disease =="all-c", code_pays=="FRP") %>% select ( -grp_diet, -diet.scenario, -disease, -parameter, -code_pays, -`all-rf`)
)


# performing MFA
res.mfa.ns <- MFA(data.mfa, group =c(1,24,5,9), type =c("n","s","s","s"), name.group = c("diet", "nutritional", "environmental", "health"), num.group.sup = 1 )
