### Summed Probability Distribution ###
# Modified 14/10/24 by G.Vernham
# TIME TO RUN: 20 mins on 1 processor

### Load and install packages ###
libs <- c("dplyr","ggplot2", "stats", "rcarbon", "zoo", "graphics", "stringr")

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

### Data ###
human.footprint <- read.csv("./Processed_data/archeological_data/human.footprint.csv")
human.withoutRussia <-read.csv("./Processed_data/archeological_data/human.withoutRussia.csv")

### Directories ###
dir.create(file.path("./Processed_data/", "Footprint_calibration_results"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "SPD_data"), showWarnings = FALSE)
dir.create(file.path("./", "Plots"), showWarnings = FALSE)
dir.create(file.path("./Plots/", "Null_model_plots"), showWarnings = FALSE)

### DATA WRANGLING ###
#!!! CONSIDER MOVING TO "archeological_data.R" !!!
human.footprint$SiteName <- sub("_", "", human.footprint$SiteName)
hfSubset <- human.footprint[grep("Sundfj", human.footprint$SiteName), ]
hfSubset$SiteName <- "Sundfj"
human.footprint <- human.footprint[-grep("Sundfj", human.footprint$SiteName), ]
human.footprint <- rbind(human.footprint, hfSubset)

### LOAD FUNCTIONS ###
source("./Functions/make_interval_arc.R")
source("./Functions/plot_arc.R")
source("./Functions/interval_spd.R")
source("./Functions/arc_spd.R")
source("./Functions/nullmodel_spd.R")
#barCodes(arc.bins.med,yrng = c(0,0.01))

### NORTH_15 ###
#-------------------------------------------------------------------------------
arc.north <- subset(human.footprint, Lat>67.5)
if (!(paste0("spdN.csv") %in% list.files("./Processed_data/SPD_data/"))){
  spdN <- arc_spd(arc.north, 1400)
  write.csv(spdN, "./Processed_data/SPD_data/spdN.csv", row.names = F)
}

if (!(paste0("nullmodelN.Rdata") %in% list.files("./Processed_data/Footprint_calibration_results/"))){
  nullmodelN <- nullmodel_spd(arc.north, 1400)
  saveRDS(nullmodelN, file="./Processed_data/Footprint_calibration_results/nullmodelN.Rdata")
  nullmodelN <- readRDS("./Processed_data/Footprint_calibration_results/nullmodelN.Rdata")
} else {
  nullmodelN <- readRDS("./Processed_data/Footprint_calibration_results/nullmodelN.Rdata")
}

if (!(paste0("nullmodelN.png") %in% 
      list.files("./Plots/Null_model_plots/"))){
png(file="./Plots/Null_model_plots/nullmodelN.png",
    width=1000, height=650)
plot.nullN <- plot(nullmodelN)
dev.off()
}

#sites <- unique(arc.north$Lat)
#sites <- unique(arc.north$Long)

### SOUTHEAST_911 ###
#-------------------------------------------------------------------------------
arc.southeast <- subset(human.footprint, Lat>60 & Lat<=65 & Long>20)
if (!(paste0("spdSE.csv") %in% list.files("./Processed_data/SPD_data/"))){
  spdSE <- arc_spd(arc.southeast, 500)
  write.csv(spdSE, "./Processed_data/SPD_data/spdSE.csv", row.names = F)
}

if (!(paste0("nullmodelSE.Rdata") %in% list.files("./Processed_data/Footprint_calibration_results/"))){
  nullmodelSE <- nullmodel_spd(arc.southeast, 500)
  saveRDS(nullmodelSE, file="./Processed_data/Footprint_calibration_results/nullmodelSE.Rdata")
  nullmodelSE <- readRDS("./Processed_data/Footprint_calibration_results/nullmodelSE.Rdata")
} else {
  nullmodelSE <- readRDS("./Processed_data/Footprint_calibration_results/nullmodelSE.Rdata")
}

if (!(paste0("nullmodelSE.png") %in% 
      list.files("./Plots/Null_model_plots/"))){
  png(file="./Plots/Null_model_plots/nullmodelSE.png",
      width=1000, height=650)
  plot.nullSE <- plot(nullmodelSE)
  dev.off()
}


# GRANT:: Originally set up wrong. data (incl Russia) not used for the models. Possibly remove if not used.
arc.southeast2 <- subset(human.withoutRussia, Lat>60 & Lat<=65 & Long>20)
if (!(paste0("spdSE2.csv") %in% list.files("./Processed_data/SPD_data/"))){
  spdSE2 <- arc_spd(arc.southeast2, 500)
  write.csv(spdSE2, "./Processed_data/SPD_data/spdSE2.csv", row.names = F)
}

if (!(paste0("nullmodelSE2.Rdata") %in% list.files("./Processed_data/Footprint_calibration_results/"))){
  nullmodelSE2 <- nullmodel_spd(arc.southeast2, 500)
  saveRDS(nullmodelSE2, file="./Processed_data/Footprint_calibration_results/nullmodelSE2.Rdata")
  nullmodelSE2 <- readRDS("./Processed_data/Footprint_calibration_results/nullmodelSE2.Rdata")
} else {
  nullmodelSE2 <- readRDS("./Processed_data/Footprint_calibration_results/nullmodelSE2.Rdata")
}

if (!(paste0("nullmodelSE2.png") %in% 
      list.files("./Plots/Null_model_plots/"))){
  png(file="./Plots/Null_model_plots/nullmodelSE2.png",
      width=1000, height=650)
  plot.nullSE2 <- plot(nullmodelSE2)
  dev.off()
}


### MIDWEST_2 ###
#-------------------------------------------------------------------------------
arc.midwest <- subset(human.footprint, Lat>60 & Long<=10)
if (!(paste0("spdMW.csv") %in% list.files("./Processed_data/SPD_data/"))){
  spdMW <- arc_spd(arc.midwest, 700)
  write.csv(spdMW, "./Processed_data/SPD_data/spdMW.csv", row.names = F)
}

if (!(paste0("nullmodelMW.Rdata") %in% list.files("./Processed_data/Footprint_calibration_results/"))){
  nullmodelMW <- nullmodel_spd(arc.midwest, 700)
  saveRDS(nullmodelMW, file="./Processed_data/Footprint_calibration_results/nullmodelMW.Rdata")
  nullmodelMW <- readRDS("./Processed_data/Footprint_calibration_results/nullmodelMW.Rdata")
} else {
  nullmodelMW <- readRDS("./Processed_data/Footprint_calibration_results/nullmodelMW.Rdata")
}

if (!(paste0("nullmodelMW.png") %in% 
      list.files("./Plots/Null_model_plots/Null_model_plots/"))){
  png(file="./Plots/Null_model_plots/nullmodelMW.png",
      width=1000, height=650)
  plot.nullMW <- plot(nullmodelMW)
  dev.off()
}

### MIDMID_47 ###
#-------------------------------------------------------------------------------
arc.midmid <- subset(human.footprint, Lat>60 & Lat<=65 & Long>10 & Long<=20)
if (!(paste0("spdMM.csv") %in% list.files("./Processed_data/SPD_data/"))){
  spdMM <- arc_spd(arc.midmid, 700)
  write.csv(spdMM, "./Processed_data/SPD_data/spdMM.csv", row.names = F)
}

if (!(paste0("nullmodelMM.Rdata") %in% list.files("./Processed_data/Footprint_calibration_results/"))){
  nullmodelMM <- nullmodel_spd(arc.midmid, 700)
  saveRDS(nullmodelMM, file="./Processed_data/Footprint_calibration_results/nullmodelMM.Rdata")
  nullmodelMM <- readRDS("./Processed_data/Footprint_calibration_results/nullmodelMM.Rdata")
} else {
  nullmodelMM <- readRDS("./Processed_data/Footprint_calibration_results/nullmodelMM.Rdata")
}

if (!(paste0("nullmodelMM.png") %in% 
      list.files("./Plots/Null_model_plots/"))){
  png(file="./Plots/Null_model_plots/nullmodelMM.png",
      width=1000, height=650)
  plot.nullMM <- plot(nullmodelMM)
  dev.off()
}

### SOUTHWEST_1 ###
#-------------------------------------------------------------------------------
arc.southwest <- subset(human.footprint, Lat <= 60 & Long<=10)
if (!(paste0("spdSW.csv") %in% list.files("./Processed_data/SPD_data/"))){
  spdSW <- arc_spd(arc.southwest, 700)
  write.csv(spdSW, "./Processed_data/SPD_data/spdSW.csv", row.names = F)
}

if (!(paste0("nullmodelSW.Rdata") %in% list.files("./Processed_data/Footprint_calibration_results/"))){
  nullmodelSW <- nullmodel_spd(arc.southwest, 700)
  saveRDS(nullmodelSW, file="./Processed_data/Footprint_calibration_results/nullmodelSW.Rdata")
  nullmodelSW <- readRDS("./Processed_data/Footprint_calibration_results/nullmodelSW.Rdata")
} else {
  nullmodelSW <- readRDS("./Processed_data/Footprint_calibration_results/nullmodelSW.Rdata")
}

if (!(paste0("nullmodelSW.png") %in% 
      list.files("./Plots/Null_model_plots/"))){
  png(file="./Plots/Null_model_plots/nullmodelSW.png",
      width=1000, height=650)
  plot.nullSW <- plot(nullmodelSW, ylim=c(0,1))
  dev.off()
}

### SOUTHMID_36 ###
#-------------------------------------------------------------------------------
arc.southmid <- subset(human.footprint, Lat<=60 & Long>10 & Long<=20)
if (!(paste0("spdSM.csv") %in% list.files("./Processed_data/SPD_data/"))){
  spdSM <- arc_spd(arc.southmid, 1400)
  write.csv(spdSM, "./Processed_data/SPD_data/spdSM.csv", row.names = F)
}

if (!(paste0("nullmodelSM.Rdata") %in% list.files("./Processed_data/Footprint_calibration_results/"))){
  nullmodelSM <- nullmodel_spd(arc.southmid, 1400)
  saveRDS(nullmodelSM, file="./Processed_data/Footprint_calibration_results/nullmodelSM.Rdata")
  nullmodelSM <- readRDS("./Processed_data/Footprint_calibration_results/nullmodelSM.Rdata")
} else {
  nullmodelSM <- readRDS("./Processed_data/Footprint_calibration_results/nullmodelSM.Rdata")
}

if (!(paste0("nullmodelSM.png") %in% 
      list.files("./Plots/Null_model_plots/"))){
  png(file="./Plots/Null_model_plots/nullmodelSM.png",
      width=1000, height=650)
  plot.nullSM <- plot(nullmodelSM)
  dev.off()
}
