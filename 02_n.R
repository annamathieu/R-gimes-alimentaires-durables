env <- read.table(file = "data_csv/env.csv", header = T, sep =";", stringsAsFactors = T, dec = ",")

env_new <- env[-which(env$region %in% c("LMC",'UMC','HIC','LIC', 'all-r')) , ] # supprimer niveau moyens
env_new <- env_new[-ends_with( "LMIC", vars = env_new$region) , ] # idem
env_new <- rename(env_new,code_pays=region)                       # renomme code pays 
env_new <- pivot_wider(data = env_new, names_from = environmental.domain, values_from = value)

