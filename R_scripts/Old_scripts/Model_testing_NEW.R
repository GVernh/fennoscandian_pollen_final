#- LIBRARIES ----
libs <- c("ggplot2","ecp", "mcp", "dplyr", "ggpubr", "purrr")

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

#disable scientific notation
options(scipen=999)

#-FUNCTIONS ----

source("./Functions/CountSitesPerInterval.R")
source("./Functions/sqrt_sums.R")
source("./Functions/makeIncrements_singleLCC.R")

### Create directories ###
dir.create(file.path("./Processed_data/", "mcp_models"), showWarnings = FALSE)
dir.create(file.path("./", "Results"), showWarnings = FALSE)
dir.create(file.path("./Results/", "Plots"), showWarnings = FALSE)
dir.create(file.path("./Results/Plots/", "LCC_abun"), showWarnings = FALSE)
dir.create(file.path("./Results/Plots/LCC_abun/", "mcp_plots"), showWarnings = FALSE)

# Set Bayes factor threshold for mcp acceptance
BF_thresh <- 10

# Prepare model (5 change points)
#model = list(LCC~1+age, ~1+age, ~1+age, ~1+age, ~1+age, ~1+age)
model = list(LCC~1+age, ~1+age, ~1+age, ~1+age, ~1+age)
# Run Models

# NORTH ----
all_data<- read.csv("./Processed_data/Full_datasets/alldataNs_abun.csv") %>%
  select(yearsBP, SPD, clim, conNs, decNs, wetwNs, wetmNs, pasNs, araNs, heaNs)

cp_df <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(cp_df) <- c("name", "mean", "lower", "upper", "Rhat", "n.eff", "BF", "ID")

for (i in 4:length(all_data)) {
  tag <- names(all_data[i])
  
  data = all_data %>%
    select(yearsBP, tag)
  colnames(data) <- c("age","LCC")
  
  fit_mcp = mcp(model, data = data, par_x = "age", adapt = 100000, sample = "both", iter = 50000)
  
  plot(fit_mcp)+
    ggtitle(paste0(tag))
  ggsave(paste0("./Results/Plots/LCC_abun/mcp_plots/mcp_",tag,".png"))
  
  tab = summary(fit_mcp)
  hyp_test = hypothesis(fit_mcp, c(paste0("cp_1=",tab[7,2]),
                                   paste0("cp_2=",tab[8,2]),
                                   paste0("cp_3=",tab[9,2]),
                                   paste0("cp_4=",tab[10,2]),
                                   paste0("cp_5=",tab[11,2])))
  
  cp_tab <- subset(tab, name %in% c("cp_1", "cp_2", "cp_3", "cp_4", "cp_5"))
  cp_tab$BF <- hyp_test$BF
  cp_tab$ID <- tag
  cp_df <- rbind(cp_df, cp_tab)
}
write.csv(cp_df, "./Processed_data/mcp_models/North_mcp.csv")

# SOUTHEAST ----

all_data<- read.csv("./Processed_data/Full_datasets/alldataSEs_abun.csv") %>%
  select(yearsBP, SPD, clim, conSEs, decSEs, wetwSEs, wetmSEs, pasSEs, araSEs, heaSEs)

cp_df <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(cp_df) <- c("name", "mean", "lower", "upper", "Rhat", "n.eff", "BF", "ID")

for (i in 4:length(all_data)) {
  tag <- names(all_data[i])
  
  data = all_data %>%
    select(yearsBP, tag)
  colnames(data) <- c("age","LCC")
  
  fit_mcp = mcp(model, data = data, par_x = "age", adapt = 100000, sample = "both", iter = 50000)
  
  plot(fit_mcp)+
    ggtitle(paste0(tag))
  ggsave(paste0("./Results/Plots/LCC_abun/mcp_plots/mcp_",tag,".png"))
  
  tab = summary(fit_mcp)
  hyp_test = hypothesis(fit_mcp, c(paste0("cp_1=",tab[7,2]),
                                   paste0("cp_2=",tab[8,2]),
                                   paste0("cp_3=",tab[9,2]),
                                   paste0("cp_4=",tab[10,2]),
                                   paste0("cp_5=",tab[11,2])))
  
  cp_tab <- subset(tab, name %in% c("cp_1", "cp_2", "cp_3", "cp_4", "cp_5"))
  cp_tab$BF <- hyp_test$BF
  cp_tab$ID <- tag
  cp_df <- rbind(cp_df, cp_tab)
}
write.csv(cp_df, "./Processed_data/mcp_models/SOUTHEAST_mcp.csv")

# SOUTHMID ----
all_data<- read.csv("./Processed_data/Full_datasets/alldataSMs_abun.csv") %>%
  select(yearsBP, SPD, clim, conSMs, decSMs, wetwSMs, wetmSMs, pasSMs, araSMs, heaSMs)

cp_df <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(cp_df) <- c("name", "mean", "lower", "upper", "Rhat", "n.eff", "BF", "ID")

for (i in 4:length(all_data)) {
  tag <- names(all_data[i])
  
  data = all_data %>%
    select(yearsBP, tag)
  colnames(data) <- c("age","LCC")
  
  fit_mcp = mcp(model, data = data, par_x = "age", adapt = 100000, sample = "both", iter = 50000)
  
  plot(fit_mcp)+
    ggtitle(paste0(tag))
  ggsave(paste0("./Results/Plots/LCC_abun/mcp_plots/mcp_",tag,".png"))
  
  tab = summary(fit_mcp)
  hyp_test = hypothesis(fit_mcp, c(paste0("cp_1=",tab[7,2]),
                                   paste0("cp_2=",tab[8,2]),
                                   paste0("cp_3=",tab[9,2]),
                                   paste0("cp_4=",tab[10,2]),
                                   paste0("cp_5=",tab[11,2])))
  
  cp_tab <- subset(tab, name %in% c("cp_1", "cp_2", "cp_3", "cp_4", "cp_5"))
  cp_tab$BF <- hyp_test$BF
  cp_tab$ID <- tag
  cp_df <- rbind(cp_df, cp_tab)
}
write.csv(cp_df, "./Processed_data/mcp_models/SOUTHMID_mcp.csv")


# CENTRAL ---- ##########################
all_data<- read.csv("./Processed_data/Full_datasets/alldataMMs_abun.csv") %>%
  select(yearsBP, SPD, clim, conMMs, decMMs, wetwMMs, wetmMMs, pasMMs, araMMs, heaMMs)

cp_df <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(cp_df) <- c("name", "mean", "lower", "upper", "Rhat", "n.eff", "BF", "ID")

for (i in 4:length(all_data)) {
  tag <- names(all_data[i])
  
  data = all_data %>%
    select(yearsBP, tag)
  colnames(data) <- c("age","LCC")
  
  fit_mcp = mcp(model, data = data, par_x = "age", adapt = 100000, sample = "both", iter = 50000)
  
  plot(fit_mcp)+
    ggtitle(paste0(tag))
  ggsave(paste0("./Results/Plots/LCC_abun/mcp_plots/mcp_",tag,".png"))
  
  tab = summary(fit_mcp)
  hyp_test = hypothesis(fit_mcp, c(paste0("cp_1=",tab[7,2]),
                                   paste0("cp_2=",tab[8,2]),
                                   paste0("cp_3=",tab[9,2]),
                                   paste0("cp_4=",tab[10,2]),
                                   paste0("cp_5=",tab[11,2])))
  
  cp_tab <- subset(tab, name %in% c("cp_1", "cp_2", "cp_3", "cp_4", "cp_5"))
  cp_tab$BF <- hyp_test$BF
  cp_tab$ID <- tag
  cp_df <- rbind(cp_df, cp_tab)
}
write.csv(cp_df, "./Processed_data/mcp_models/MIDMID_mcp.csv")


# MIDWEST ----
all_data<- read.csv("./Processed_data/Full_datasets/alldataMWs_abun.csv") %>%
  select(yearsBP, SPD, clim, conMWs, decMWs, wetwMWs, wetmMWs, pasMWs, araMWs, heaMWs)

cp_df <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(cp_df) <- c("name", "mean", "lower", "upper", "Rhat", "n.eff", "BF", "ID")

for (i in 4:length(all_data)) {
  tag <- names(all_data[i])
  
  data = all_data %>%
    select(yearsBP, tag)
  colnames(data) <- c("age","LCC")
  
  fit_mcp = mcp(model, data = data, par_x = "age", adapt = 100000, sample = "both", iter = 50000)
  
  plot(fit_mcp)+
    ggtitle(paste0(tag))
  ggsave(paste0("./Results/Plots/LCC_abun/mcp_plots/mcp_",tag,".png"))
  
  tab = summary(fit_mcp)
  hyp_test = hypothesis(fit_mcp, c(paste0("cp_1=",tab[7,2]),
                                   paste0("cp_2=",tab[8,2]),
                                   paste0("cp_3=",tab[9,2]),
                                   paste0("cp_4=",tab[10,2]),
                                   paste0("cp_5=",tab[11,2])))
  
  cp_tab <- subset(tab, name %in% c("cp_1", "cp_2", "cp_3", "cp_4", "cp_5"))
  cp_tab$BF <- hyp_test$BF
  cp_tab$ID <- tag
  cp_df <- rbind(cp_df, cp_tab)
}
write.csv(cp_df, "./Processed_data/mcp_models/MIDWEST_mcp.csv")

# SOUTHWEST ----
all_data<- read.csv("./Processed_data/Full_datasets/alldataSWs_abun.csv") %>%
  select(yearsBP, SPD, clim, conSWs, decSWs, wetwSWs, wetmSWs, pasSWs, araSWs, heaSWs)

cp_df <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(cp_df) <- c("name", "mean", "lower", "upper", "Rhat", "n.eff", "BF", "ID")

for (i in 4:length(all_data)) {
  tag <- names(all_data[i])
  
  data = all_data %>%
    select(yearsBP, tag)
  colnames(data) <- c("age","LCC")
  
  fit_mcp = mcp(model, data = data, par_x = "age", adapt = 100000, sample = "both", iter = 50000)
  
  plot(fit_mcp)+
    ggtitle(paste0(tag))
  ggsave(paste0("./Results/Plots/LCC_abun/mcp_plots/mcp_",tag,".png"))
  
  tab = summary(fit_mcp)
  hyp_test = hypothesis(fit_mcp, c(paste0("cp_1=",tab[7,2]),
                                   paste0("cp_2=",tab[8,2]),
                                   paste0("cp_3=",tab[9,2]),
                                   paste0("cp_4=",tab[10,2]),
                                   paste0("cp_5=",tab[11,2])))
  
  cp_tab <- subset(tab, name %in% c("cp_1", "cp_2", "cp_3", "cp_4", "cp_5"))
  cp_tab$BF <- hyp_test$BF
  cp_tab$ID <- tag
  cp_df <- rbind(cp_df, cp_tab)
}
write.csv(cp_df, "./Processed_data/mcp_models/SOUTHWEST_mcp.csv")
