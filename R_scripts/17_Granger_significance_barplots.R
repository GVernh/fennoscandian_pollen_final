### plots all ###
libs <- c("ggplot2", "dplyr", "tidyr", "stringr")

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
rm(list = ls())

# Load Granger causality results ----
sig_abun <- read.csv("./Results/Granger_causality/Table_granger_significance_abun.csv")

# Data cleaning ----
sig_abun = sig_abun %>% pivot_longer(cols=c(Coniferous.woodland,Deciduous.woodland,Wet.woodland,
                                            Wet.meadow,Pasture,Arable.land,Heath,All),
                    names_to='LCC',
                    values_to='Count') %>%
  mutate(across('Variable', str_replace, 'Both', 'SPD + Climate'))

sig_abun$LCC = gsub(".", " ", sig_abun$LCC, fixed = T)

sig_abun$Subset[sig_abun$Subset == "Entire dataset"] <- "All data"
sig_abun$LCC[sig_abun$LCC == "Heath"] <- "Heathland"
sig_abun$LCC[sig_abun$LCC == "Arable land"] <- "Arable"

sig_abun$Subset <- factor(sig_abun$Subset,
                          levels = c("All data", "Before farming", "After farming"))

# Barplots ----

plot <- ggplot(sig_abun, aes(x = LCC, y = Count, fill = Variable)) +
  geom_bar(stat = "identity",
           width = 0.6,
           position = position_dodge(width = 0.7)) +
  facet_wrap(~ Subset, scales = "free_x") +
  ylab("Number of significant models") +
  xlab("Land cover class (LCC)") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(
    name = "Model",
    values = c(
      "Climate" = "#0072B2",
      "SPD" = "#E69F00",
      "SPD + Climate" = "#009E73"
    ),
    breaks = c("Climate", "SPD", "SPD + Climate")
  )+
  scale_y_continuous(breaks = seq(0, max(sig_abun$Count), by = 2))+
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 18),
    plot.margin = margin(10, 10, 10, 10)
  )
ggsave(
  filename = "./Results/Plots/LCC_abun/Granger_significance_bar_abun.png",
  plot     = plot,
  width    = 10,       # inches (adjust as needed)
  height   = 7,        # inches (adjust as needed)
  dpi      = 300       # 300 = standard high-res, 600 = ultra-print quality
)

