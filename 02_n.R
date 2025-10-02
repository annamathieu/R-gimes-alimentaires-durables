

head(env_new)
env_new <- env_new %>%
  mutate(grp_diet = case_when(
    diet.scenario %in% c("FLX", "PSC", "VEG", "VGN") ~ "vg",
    diet.scenario %in% c("ani-25", "ani-50", "ani-75", "ani-100") ~ "ani", 
    diet.scenario %in% c("kcal-25", "kcal-50", "kcal-75", "kcal-100") ~ "kcal", 
    TRUE ~ "bmk"))

