##############################################################################
# PCA ENVIRONMENTAL FUNCTION 
##############################################################################

env_pca_f <- function(data = env_new, color_by = "diet.scenario", country = "FRP") {
  library(FactoMineR)
  library(factoextra)
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  
  #Palette de couleurs
  colors.scenario <- c(
    "BMK" = "grey70",
    "ani-25" = "#b30000",  
    "ani-50" = "#ff1a1a",
    "ani-75" = "#ff6666",
    "ani-100" = "#ffb3b3",  
    "kcal-25" = "#4b0082",
    "kcal-50" = "#8000ff",
    "kcal-75" = "#b366ff",
    "kcal-100" = "#dab3ff",
    "FLX" = "#6ebf8b",
    "PSC" = "#32a852",
    "VEG" = "#26734d",
    "VGN" = "#145a32"
  )
  bar_order <- names(colors.scenario)
  
  # Filtrer les données
  data_fr <- data %>%
    filter(code_pays == country, item == "abs") %>%
    group_by(diet.scenario) %>%
    summarise(across(c(GHGe, land, water, nitr, phos), mean, na.rm = TRUE)) %>%
    ungroup()
  
  # Facteur ordonné
  data_fr$diet.scenario <- factor(data_fr$diet.scenario, levels = bar_order)
  
  # PCA
  acp <- PCA(data_fr[, c("GHGe", "land", "water", "nitr", "phos")], scale.unit = TRUE, graph = FALSE)
  
  # Pourcentages de variance
  dim1_var <- round(acp$eig[1, 2], 1)
  dim2_var <- round(acp$eig[2, 2], 1)
  
  # GRAPH 1 : Variables
  g1 <- fviz_pca_var(acp,
                     col.var = "contrib",
                     gradient.cols = c("skyblue", "orange", "red"),
                     repel = TRUE) +
    labs(title = "Relationships between environmental indicators (France)",
         x = paste0("Dim 1 (", dim1_var, "%)"),
         y = paste0("Dim 2 (", dim2_var, "%)")) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  # GRAPH 2 : Individus
  ind_df <- data.frame(acp$ind$coord, diet.scenario = data_fr$diet.scenario)
  
  g2 <- ggplot(ind_df, aes(x = Dim.1, y = Dim.2, color = diet.scenario)) +
    # Ajout des axes centraux
    geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.7) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 0.7) +
    geom_point(size = 4, alpha = 0.9) +
    geom_text(aes(label = diet.scenario), vjust = -0.8, size = 3.5) +
    scale_color_manual(values = colors.scenario, breaks = bar_order) +
    labs(
      title = "Diet scenarios projection (France)",
      color = "Diet scenario",
      x = paste0("Dim 1 (", dim1_var, "%)"),
      y = paste0("Dim 2 (", dim2_var, "%)")
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90")
    )
  
  # Combinaison
  combined <- g1 + g2 + plot_layout(widths = c(1, 1.2))
  return(combined)
}
