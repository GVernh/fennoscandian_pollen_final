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

sig_count <- read.csv("./Results/Granger_causality/Table_granger_significance_count.csv")

sig_count = sig_count %>% pivot_longer(cols=c(Coniferous.woodland,Deciduous.woodland,Wet.woodland,
                                              Wet.meadow,Pasture,Arable.land,Heath,All),
                                       names_to='LCC',
                                       values_to='Count') %>%
  mutate(across('Variable', str_replace, 'Both', 'SPD + Climate'))

sig_count$LCC = gsub(".", " ", sig_count$LCC, fixed = T)

png("./Results/Plots/LCC_count/Granger_significance_bar_count.png", width = 1400, height = 900)

ggplot(sig_count, aes(x = LCC, y = Count, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Subset) +
  ylab("N. significant models") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  guides(fill=guide_legend(title="Model")) +
  theme(legend.title=element_text(face = "bold"),
        axis.text = element_text(size= 16), 
        axis.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12, face = "bold")) +
  scale_fill_discrete(breaks=c('Climate', 'SPD', 'SPD + Climate')) +
  scale_fill_manual("legend", values = c("Climate" = "#0072B2", "SPD" = "#E69F00", "SPD + Climate" = "#009E73"))

dev.off()