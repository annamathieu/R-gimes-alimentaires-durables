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
nutri <-