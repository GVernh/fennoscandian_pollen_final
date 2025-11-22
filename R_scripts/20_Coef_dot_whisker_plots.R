libs <- c("tidyverse")

installed_libs <- libs %in% rownames(
  installed.packages())

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

invisible(lapply(
  libs,
  library,
  character.only = T
))
rm(list = setdiff(ls(), "relative_abun"))

all_data <- read.csv("./Results/Plots/LCC_abun/Significant_results_all_abun.csv") %>%
  dplyr::mutate(data = "All")
before_data <- read.csv("./Results/Plots/LCC_abun/Significant_results_before_abun.csv")%>%
  dplyr::mutate(data = "Before")
after_data <- read.csv("./Results/Plots/LCC_abun/Significant_results_after_abun.csv")%>%
  dplyr::mutate(data = "After")


combined_data <- rbind(after_data, before_data, all_data) %>%
  select(-X)

write.csv(combined_data, "./Results/Plots/LCC_abun/Significant_results_combined.csv", row.names = F)



# Load data
df <- read_csv("./Results/Plots/LCC_abun/Significant_results_combined.csv")

df$data <- factor(
  df$data,
  levels = c("All", "Before", "After"),
  labels = c("All data", "Before farming", "After farming")
)

# 
# Summarize: mean + SD of Coef. by data & LCC
df_sum <- df %>%
  group_by(data, LCC) %>%
  summarise(
    mean_coef = mean(Coef., na.rm = TRUE),
    sd_coef   = sd(Coef., na.rm = TRUE),
    .groups = "drop"
  )

plot <- ggplot(df_sum, aes(x = LCC, y = mean_coef)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = mean_coef - sd_coef,
                    ymax = mean_coef + sd_coef),
                width = 0.15, color = "black") +
  facet_wrap(~ data, scales = "free_x", nrow = 1) +
  scale_y_continuous(
    labels = function(x) paste0(x, ""),
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "grey60", fill = NA),
    panel.grid.minor = element_blank()
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    x = "LCC",
    y = "Commonality coefficient (± SD, %)",
  ) +
  


# Save plot
ggsave(
  filename = "./Results/Plots/LCC_abun/dot_whisker_comm_coef.png",
  plot     = plot,
  width    = 10,       # inches (adjust as needed)
  height   = 7,        # inches (adjust as needed)
  dpi      = 300       # 300 = standard high-res, 600 = ultra-print quality
)


# Grouped by LCC & causality
df <- read_csv("./Results/Plots/LCC_abun/Significant_results_combined.csv")

df$data <- factor(
  df$data,
  levels = c("All", "Before", "After"),
  labels = c("All data", "Before farming", "After farming")
)

# Reorder data categories
df_sum <- df %>%
  group_by(data, LCC, Causality) %>%
  summarise(
    mean_coef = mean(Coef., na.rm = TRUE),
    sd_coef   = sd(Coef., na.rm = TRUE),
    .groups = "drop"
  )

cb_palette <- c(
  "SPD"        = "#E69F00",  # Blue
  "Climate"      = "#0072B2",  # Orange
  "SPD + Climate" = "#009E73"   # Green
)

df_sum$Causality <- factor(
  df_sum$Causality,
  levels = c("clim","SPD", "SPD|clim"),
  labels = c("Climate","SPD", "SPD + Climate")
)

# Build professional plot
plot <- ggplot(df_sum, aes(x = LCC, y = mean_coef, color = Causality)) +
  
  # Vertical separators between LCC categories
  geom_vline(
    xintercept = seq(1.5, length(unique(df_sum$LCC)) - 0.5, by = 1),
    color = "grey80",
    linewidth = 0.5
  ) +
  
  # Whiskers
  geom_errorbar(
    aes(
      ymin = mean_coef - sd_coef,
      ymax = mean_coef + sd_coef
    ),
    position = position_dodge(width = 0.6),
    width = 0.3,
    linewidth = 0.7,
    alpha = 0.7
  ) +
  
  # Dots
  geom_point(
    position = position_dodge(width = 0.6),
    size = 3,
    stroke = 0.7
  ) +
  
  # Facets for data periods
  facet_wrap(~ data, nrow = 1) +
  
  # Y-axis with ticks every 2%
  scale_y_continuous(
    labels = function(x) paste0(x, ""),
    breaks = seq(
      floor(min(df_sum$mean_coef - df_sum$sd_coef, na.rm = TRUE)),
      ceiling(max(df_sum$mean_coef + df_sum$sd_coef, na.rm = TRUE)),
      by = 2
    ),
    limits = c(
      floor(min(df_sum$mean_coef - df_sum$sd_coef, na.rm = TRUE)),
      ceiling(max(df_sum$mean_coef + df_sum$sd_coef, na.rm = TRUE))
    ),
    expand = c(0, 0)
  ) +
  
  # Colour-blind friendly palette (replaces Dark2)
  scale_color_manual(values = cb_palette) +
  
  # Labels
  labs(
    x = "Land cover class (LCC)",
    y = "Coefficient (± SD, %)",
    color = "Model"
  ) +
  
  # Professional theme
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 40, hjust = 1),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13, color = "grey30"),
    legend.position = "right"
  )


ggsave(
  filename = "./Results/Plots/LCC_abun/dot_whisker_comm_coef.png",
  plot     = plot,
  width    = 10,       # inches (adjust as needed)
  height   = 7,        # inches (adjust as needed)
  dpi      = 300       # 300 = standard high-res, 600 = ultra-print quality
)
