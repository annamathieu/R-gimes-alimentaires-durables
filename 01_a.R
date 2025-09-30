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

# ⚠️ quelques NA a gérer au cas par cas 
nutri_new <- left_join(nutri, conti, by = c("region" = "code_pays"))
nutri_new <- nutri_new[-which(nutri_new$region %in% c("LMC",'UMC','HIC','LIC', 'all-r')) , ]
nutri_new <- nutri_new[-ends_with( "LMIC", vars = nutri_new$region) , ]

nutri_new <- rename(nutri_new, 
                    code_pays=region) 

# pivot wider de nutri 
nutri_new <- pivot_wider(data = nutri_new, names_from = nutrient, values_from = value)
nutri_new <- nutri_new[-which(nutri_new$stats %in% c("low", "high")) , ]   # garder que les moyennes
nutri_new <- nutri_new[,-3] # supprimer colonne stats (1 seul level)

# env new
env_new <- env[-which(env$region %in% c("LMC",'UMC','HIC','LIC', 'all-r')) , ] # supprimer niveau moyens
env_new <- env_new[-ends_with( "LMIC", vars = env_new$region) , ] # idem
env_new <- rename(env_new,code_pays=region)                       # renomme code pays 
env_new <- pivot_wider(data = env_new, names_from = environmental.domain, values_from = value)


# garder un seul scénario socio éco ?

# sante_new
sante_new <- sante[-which(sante$region %in% c("LMC",'UMC','HIC','LIC', 'all-r')) , ] # supprimer niveau moyens
sante_new <- sante_new[-ends_with( "LMIC", vars = sante_new$region) , ] # idem
sante_new <- rename(sante_new,code_pays=region)                       # renomme code pays 
sante_new <- pivot_wider(data = sante_new, names_from = risk.factor, values_from = value)
sante_new <- sante_new[-which(sante_new$stats %in% c("low", "high")) , ]   # garder que les moyenne
sante_new <- sante_new[,-5]


# analyse des valeurs manquantes 












