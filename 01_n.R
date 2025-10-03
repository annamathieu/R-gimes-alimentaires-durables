
library(tidyverse)
library(dplyr)
library(tidyr)



head(nutri_new)


##############################################################
library(ggplot2)
library(plotly)

p <- ggplot(
  data = subset(nutri_new, code_pays == "FRP" & item == "%rec"),
  aes(x = diet.scenario, 
      y = protein, 
      fill = protein,
      text = paste0("Régime: ", diet.scenario,
                    "<br>Protéine: ", round(protein,1), "%"))) +  # tooltip
  geom_col(width = 0.7) +
  geom_hline(yintercept = 100, 
             linetype = "solid", 
             color = "red", 
             size = 1) +
  scale_fill_gradient(low = "plum1", high = "purple4") +
  labs(
    title = "Apport en protéines selon le régime alimentaire (France)",
    x = "Régime Alimentaire",
    y = "% de la recommandation"
  ) +
  theme_minimal(base_size = 10) +    # texte un peu plus grand
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12), # titre centré et gras
    axis.title = element_text(face = "bold"),   # titres en gras
    panel.grid.major.x = element_blank(),       # enlève les grilles verticales
    panel.grid.minor = element_blank()
  )

# rendre interactif
ggplotly(p, tooltip = "text")




#############################################################
#Fonction généralisée sur les pays et les nutriments : 
#############################################################



nutri_plot <- function(nutri_new, code_pays = NULL, nutrient = NULL) {
  
  # filtrer selon les inputs
  if (!is.null(code_pays)) {
    nutri_new <- nutri_new[nutri_new$code_pays == code_pays, ]
  }
  if (!is.null(nutrient)) {
    nutri_new <- nutri_new[nutri_new$item == "%rec", ]   # garder seulement %rec
  }
  
  # graphique
  p <- ggplot(nutri_new, aes(x = diet.scenario,
                      y = .data[[nutrient]],
                      fill = .data[[nutrient]],
                      text = paste0("Régime: ", diet.scenario,
                                    "<br>", nutrient, ": ", round(.data[[nutrient]],1), "%"))) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 100, linetype = "solid", color = "red", size = 1) +
    scale_fill_gradient(low = "plum1", high = "purple4") +
    labs(
      title = paste("Apport en", nutrient, "selon le régime alimentaire (", code_pays, ")"),
      x = "Régime Alimentaire",
      y = "% de la recommandation"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      axis.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(ggplotly(p, tooltip = "text"))
}

#######
#######Exemples d'utilisation 
#############################

# France + protéines
nutri_plot(nutri_new, code_pays = "FRP", nutrient = "protein")

# Allemagne + calories
nutri_plot(nutri_new, code_pays = "AFG", nutrient = "vitaminC")





###############################################################################
library(tidyverse)
library(plotly)

nutri_plot3 <- function(nutri_new, code_pays = NULL, nutrient = NULL) {
  
  df <- nutri_new
  
  # Filtres
  if (!is.null(code_pays)) {
    df <- df %>% filter(code_pays == code_pays)
  }
  if (!is.null(nutrient)) {
    df <- df %>% filter(item == "%rec")
  }
  
  # Sécuriser grp_diet (on accepte bmk/vg/ani/kcal ou déjà jolis)
  df <- df %>%
    mutate(
      grp_diet = case_when(
        grp_diet %in% c("bmk","BMK") ~ "BMK",
        grp_diet %in% c("vg","VG","Végétal","Vegetal","vegetal") ~ "Végétal",
        grp_diet %in% c("kcal","Kcal","KCAL") ~ "Kcal",
        grp_diet %in% c("ani","ANI","Animal") ~ "Animal",
        TRUE ~ grp_diet
      ),
      grp_diet = factor(grp_diet, levels = c("BMK","Végétal","Kcal","Animal"))
    )
  
  # Ordonner les scénarios à l’intérieur de chaque groupe par valeur croissante
  df <- df %>%
    group_by(grp_diet) %>%
    arrange(.by_group = TRUE, .data[[nutrient]]) %>%
    # mise à l’échelle intra-groupe pour construire les gradients
    mutate(
      v = .data[[nutrient]],
      v_scaled = ifelse(max(v, na.rm = TRUE) == min(v, na.rm = TRUE),
                        0.5,
                        (v - min(v, na.rm = TRUE)) / (max(v, na.rm = TRUE) - min(v, na.rm = TRUE)))
    ) %>%
    ungroup() %>%
    mutate(diet.scenario = factor(diet.scenario, levels = unique(diet.scenario)))
  
  # Couleurs (gradients par groupe)
  green_grad <- scales::seq_gradient_pal("#C8E6C9", "#1B5E20", space = "Lab")
  red_grad   <- scales::seq_gradient_pal("#FAD4D4", "#B71C1C", space = "Lab")
  violet_grad<- scales::seq_gradient_pal("#E1BEE7", "#4A148C", space = "Lab")
  
  df <- df %>%
    mutate(
      fill_color = case_when(
        grp_diet == "BMK"     ~ "#b0b0b0",
        grp_diet == "Végétal" ~ green_grad(v_scaled),
        grp_diet == "Kcal"    ~ violet_grad(v_scaled),
        grp_diet == "Animal"  ~ red_grad(v_scaled),
        TRUE ~ "#b0b0b0"
      ),
      # couleurs de contour pour la légende des familles
      stroke_color = case_when(
        grp_diet == "BMK"     ~ "#7f7f7f",
        grp_diet == "Végétal" ~ "#1B5E20",
        grp_diet == "Kcal"    ~ "#6A1B9A",
        grp_diet == "Animal"  ~ "#B71C1C",
        TRUE ~ "#7f7f7f"
      )
    )
  
  # Graphique
  p <- ggplot(df, aes(x = diet.scenario, y = .data[[nutrient]])) +
    geom_col(aes(fill = fill_color, colour = grp_diet,
                 text = paste0("Régime: ", diet.scenario,
                               "<br>", nutrient, ": ", round(.data[[nutrient]],1), "%",
                               "<br>Famille: ", grp_diet)),
             width = 0.7, linewidth = 0.5, show.legend = TRUE) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "black", linewidth = 0.8) +
    scale_fill_identity() +  # on utilise nos hex directement
    scale_colour_manual(
      name = "Famille de régime",
      values = c("BMK" = "#7f7f7f", "Végétal" = "#1B5E20", "Kcal" = "#6A1B9A", "Animal" = "#B71C1C"),
      guide = guide_legend(
        override.aes = list(
          fill = c("#b0b0b0", "#66BB6A", "#9575CD", "#E57373"), # tuiles de légende
          colour = NA
        )
      )
    ) +
    facet_wrap(~ grp_diet, scales = "free_x", nrow = 1) +
    labs(
      title = paste("Apport en", nutrient, "selon le régime alimentaire (", code_pays, ")"),
      x = "Régime Alimentaire",
      y = "% de la recommandation"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
  
  ggplotly(p, tooltip = "text")
}


# France + protéines
nutri_plot3(nutri_new, code_pays = "FRP", nutrient = "protein")

# Afghanistan + Vitamine C
nutri_plot3(nutri_new, code_pays = "AFG", nutrient = "vitaminC")
