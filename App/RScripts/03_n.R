##############################################################################
# PCA ENVIRONMENTAL FUNCTION 
##############################################################################

env_pca_f <- function(data = env_new, color_by = "diet.scenario") {
  library(FactoMineR)
  library(factoextra)
  library(ggplot2)
  library(patchwork)
  
  # ---- Custom colors ----
  colors.scenario <- c(
    "BMK" = "grey70",
    "ani-25" = "#ffb3b3", "ani-50" = "#ff6666", "ani-75" = "#ff1a1a", "ani-100" = "#b30000",
    "kcal-25" = "#dab3ff", "kcal-50" = "#b366ff", "kcal-75" = "#8000ff", "kcal-100" = "#4b0082",
    "FLX" = "#6ebf8b", "PSC" = "#32a852", "VEG" = "#26734d", "VGN" = "#145a32"
  )
  
  # ---- Order of legend ----
  bar_order <- c("BMK", "ani-25", "ani-50", "ani-75", "ani-100",
                 "kcal-25","kcal-50", "kcal-75", "kcal-100",
                 "FLX", "PSC","VEG","VGN")
  
  # ---- Select numeric indicators ----
  vars <- c("GHGe", "land", "water", "nitr", "phos")
  
  # On garde seulement les lignes sans NA sur ces variables et diet.scenario
  data <- data[, c(color_by, vars)]
  data <- na.omit(data)
  
  # ---- Run PCA ----
  acp <- PCA(data[, vars], scale.unit = TRUE, graph = FALSE)
  
  # ---- GRAPH 1: Variables ----
  g1 <- fviz_pca_var(acp,
                     col.var = "contrib",
                     gradient.cols = c("skyblue", "orange", "red"),
                     repel = TRUE) +
    labs(title = "Relationships between environmental indicators") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90")
    )
  
  # ---- GRAPH 2: Individuals ----
  data[[color_by]] <- factor(data[[color_by]], levels = bar_order)
  
  g2 <- fviz_pca_ind(acp,
                     geom.ind = "point",
                     col.ind = data[[color_by]],
                     pointshape = 16,
                     pointsize = 3,
                     addEllipses = FALSE,
                     legend.title = "Diet scenario") +
    scale_color_manual(values = colors.scenario, breaks = bar_order) +
    labs(title = "Diets position in the environmental PCA space") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90")
    )
  
  # ---- Combine plots ----
  combined <- g1 + g2 + plot_layout(widths = c(1, 1.2))
  return(combined)
}
