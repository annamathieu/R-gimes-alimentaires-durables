
# Création du jeu de données final 

library(tidyverse)
library(dplyr)
library(tidyr)

# Importation des trois tableaux 
nutri <- read.table(file = "data_csv/nutri.csv", header = T, sep =";", stringsAsFactors = T, dec = ",")
sante <- read.table(file = "data_csv/sante.csv", header = T, sep =";", stringsAsFactors = T, dec = ",")
env <- read.table(file = "data_csv/env.csv", header = T, sep =";", stringsAsFactors = T, dec = ",")
conti<- read.table(file = "data_csv/conti.csv", header = T, sep=",", stringsAsFactors = T)

# Vérification des data types 
str(nutri)
str(sante)
str(env)
str(conti)


# Ajout des variable nom pays région éco continent au jeu de données 
# suppression des régions : tout + régions eco seules + régions géo eco 


# nutri_new <- left_join(nutri, conti, by = c("region" = "code_pays"))
nutri_new <- nutri[-which(nutri$region %in% c("LMC",'UMC','HIC','LIC', 'all-r')) , ]
nutri_new <- nutri_new[-ends_with( "LMIC", vars = nutri_new$region) , ]

nutri_new <- rename(nutri_new, 
                    code_pays=region) 

# pivot wider de nutri 
nutri_new <- pivot_wider(data = nutri_new, names_from = nutrient, values_from = value)
nutri_new <- nutri_new[-which(nutri_new$stats %in% c("low", "high")) , ]   # garder que les moyennes
nutri_new <- nutri_new[,-3] # supprimer colonne stats (1 seul level)


head(nutri_new)


##############################################################
library(ggplot2)

ggplot(
  data = subset(nutri_new, code_pays == "FRP" & item == "%rec"),
  aes(x = diet.scenario, 
      y = protein, 
      fill = protein)) +   
  geom_col() +
  geom_hline(yintercept = 100, 
             linetype = "solid", 
             color = "red", 
             size = 1) +
  scale_fill_gradient(low = "plum1", high = "purple4") + # gradient clair → foncé
  labs(
    title = "Apport en protéines selon le régime alimentaire (France)",
    x = "Régime Alimentaire",
    y = "% de la recommandation"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # pas besoin de légende ici








