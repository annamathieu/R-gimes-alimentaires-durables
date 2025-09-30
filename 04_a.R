### Impact environnemental


library(tidyverse)
library(dplyr)

sante <- read.table(file = "data_csv/sante.csv", header = T, sep =";", stringsAsFactors = T, dec = ",")

str(sante)
