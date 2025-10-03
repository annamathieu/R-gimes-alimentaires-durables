

summary(env_new)

# tous les régimes alimentaires simultanément POUR 

all.diet <- function(nutrim = "protein") {
  
  graph <- nutri_new %>% 
    filter((code_pays=="FRP") & (nutri_new$item=="%rec")) %>% 
    ggplot() +
    aes(fct_reorder(diet.scenario, .data[[nutrim]]), .data[[nutrim]], fill = grp_diet, group = grp_diet) +
    geom_bar(stat= "identity", position = position_dodge())+
    geom_text(aes(y = .data[[nutrim]], 
                  label = round(.data[[nutrim]],0),
                  group =grp_diet), 
              color = "black", 
              size = 4,
              vjust = 1) +
    labs(fill = "Groupe de \nscénario \nrégime \nalimentaire", 
         title = "% de couverture des protéines par le régime alimentaire \npar rapport aux recommandations nutritionnelles") + 
    theme(text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1),
          title = element_text(size = 9)) +
    xlab ("Régime alimentaire") +
    geom_hline(yintercept = 100, 
               linetype = "dashed", 
               color = "black", 
               linewidth = 0.8)
  
  return(graph )
}
