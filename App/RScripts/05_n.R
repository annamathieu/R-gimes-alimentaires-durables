##############################################################################
# FUNCTION MFA
##############################################################################
mfa_global <- function(country = "FRP") {
  library(FactoMineR)
  library(factoextra)
  library(dplyr)
  library(patchwork)
  library(ggplot2)
  
  # ---- 1) Sous-jeu NUTRI ----
  nutri_df <- nutri_new %>%
    filter(item == "abs", code_pays == country, diet.scenario != "BMK") %>%
    # on garde l'identifiant + toutes les variables numériques nutrition
    select(diet.scenario, where(is.numeric)) 
  
  # ---- 2) Sous-jeu ENV ----
  env_df <- env_new %>%
    filter(item == "abs", code_pays == country, socio.econ.scenario == "SSP2",
           diet.scenario != "BMK") %>%
    # ne garder que les indicateurs env utiles (adapter si besoin)
    select(diet.scenario, GHGe, land, water, nitr, phos)
  
  # ---- 3) Sous-jeu SANTE ----
  sante_df <- sante_new %>%
    filter(parameter == "deaths_avd", disease == "all-c", code_pays == country) %>%
    # supprimer colonnes non utiles et garder numériques
    select(diet.scenario, where(is.numeric), -`all-rf`)
  
  # ---- Contrôles rapides (utile en debug) ----
  # message("nrow nutri: ", nrow(nutri_df),
  #         " | env: ", nrow(env_df),
  #         " | sante: ", nrow(sante_df))
  
  # Si un bloc est vide, on affiche un message propre
  if (nrow(nutri_df) == 0 || nrow(env_df) == 0 || nrow(sante_df) == 0) {
    return(
      ggplot() + annotate("text", x = 0.5, y = 0.5,
                          label = paste("No data available for", country,
                                        "\n(Check nutrition / environment / health filters)"),
                          size = 6, fontface = "bold") + theme_void()
    )
  }
  
  # ---- Jointures par diet.scenario (sécurisé vs cbind) ----
  df_join <- nutri_df %>%
    inner_join(env_df,   by = "diet.scenario") %>%
    inner_join(sante_df, by = "diet.scenario")
  
  if (nrow(df_join) == 0) {
    return(
      ggplot() + annotate("text", x = 0.5, y = 0.5,
                          label = paste("No common diet scenarios for", country),
                          size = 6, fontface = "bold") + theme_void()
    )
  }
  
  # ---- Calcul automatique des tailles de groupes ----
  g_nutri <- ncol(nutri_df) - 1
  g_env   <- ncol(env_df)   - 1
  g_sante <- ncol(sante_df) - 1
  
  # Sécurités : tailles > 0
  if (g_nutri <= 0 || g_env <= 0 || g_sante <= 0) {
    return(
      ggplot() + annotate("text", x = 0.5, y = 0.5,
                          label = "Group sizes invalid (check selected columns).",
                          size = 6, fontface = "bold") + theme_void()
    )
  }
  
  # Facteur pour la première colonne
  df_join$diet.scenario <- factor(df_join$diet.scenario)
  
  # ---- MFA ----
  res.mfa <- MFA(
    df_join,
    group      = c(1, g_nutri, g_env, g_sante),
    type       = c("n", "s", "s", "s"),
    name.group = c("diet", "nutritional", "environmental", "health"),
    num.group.sup = 1
  )
  
  # ---- Graphiques ----
  g1 <- fviz_mfa_ind(res.mfa, habillage = "group", repel = TRUE) +
    labs(title = paste0("MFA - Individuals (", country, ")")) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  g2 <- fviz_mfa_var(res.mfa, "group", repel = TRUE) +
    labs(title = "MFA - Groups") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  g3 <- fviz_mfa_axes(res.mfa, choice = "group", title = "MFA - Group representation") +
    theme_minimal(base_size = 13)
  
  (g1 | g2) / g3
}
