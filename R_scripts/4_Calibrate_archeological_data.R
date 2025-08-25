# Create calibration curves from human footprint data
# Last modified: 12/10/2024 by G.Vernham

### Install/load libraries ###
libs <- c("dplyr", "Bchron")

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

### Load functions ###
source("./Functions/mean_arch_age.R")

### Calibrate archeological data ###
human.footprint <- read.csv("./Processed_data/archeological_data/human.footprint.csv")
calcurve = rep(paste0("intcal20"), nrow(human.footprint))

calibrated.arch.data <- as.list.data.frame(
  Bchron::BchronCalibrate(
    ages = human.footprint$Age,
    ageSds = human.footprint$Error,
    calCurves = calcurve,
    ids = human.footprint$LabID,
    allowOutside = TRUE))

saveRDS(calibrated.arch.data, file="./Processed_data/archeological_data/calibrated.arch.data.RData")

### calculate weighted mean ###
weighted_mean_df = NULL
for(names in names(calibrated.arch.data))
{
  namedf = mean.arch.age(calibrated.arch.data, names)
  if(names=="AA 1841"){
    weighted_mean_df = namedf
  }
  else{
    weighted_mean_df = dplyr::bind_rows(weighted_mean_df, namedf)
  }
}

weighted_mean_df$long = human.footprint$Long
weighted_mean_df$lat = human.footprint$Lat
weighted_mean_df$Material = human.footprint$Material

write.csv(weighted_mean_df, file = "./Processed_data/archeological_data/archaeological_cal.csv")

# summed probability distribution #
# GRANT:: not sure what the below code is?
#arc15_ages <- arc15$weighted_mean
#arc15.spd <- spd(arc15_ages)