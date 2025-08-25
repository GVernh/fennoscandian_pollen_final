
#################
##### Please specify if using relative abundance ####
relative_abun = T

#### Please specify if new multiple change points should be created.
# WARNIGN: If set to TRUE, this will dramatically increase computational requirments.
mcp = F
#################

libs <- c("dplyr","ggplot2", "remotes", "stringr","tidyr","rnaturalearth", "rcarbon",
          "sf", "rnaturalearthdata", "Bchron", "stats", "zoo", "graphics", "remotes",
          "ggspatial", "magrittr", "ecp", "mcp", "ggpubr", "purrr","lubridate", "fpp2", 
          "smooth", "TTR", "vars","modelr", "glm2", "timetk", "bruceR", 
          "lmtest", "scales")

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

lipdR_check <- "lipdR" %in% rownames(
  installed.packages())

if (lipdR_check == F) {
  remotes::install_github("nickmckay/lipdR")
}
invisible(library(lipdR))
rm(list = setdiff(ls(), "relative_abun"))

dir.create(file.path("./", "Processed_data"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "Pollen_data"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "archeological_data"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "Footprint_calibration_results"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "SPD_data"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "Climate"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "lpd_datasets"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "gridded_data"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "LCC_data"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/LCC_data/", "LCC_count"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/LCC_data/", "LCC_abun"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/LCC_data/LCC_count/", "mcp_models"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/LCC_data/LCC_abun/", "mcp_models"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "Full_datasets"), showWarnings = FALSE)

dir.create(file.path("./", "Plots"), showWarnings = FALSE)
dir.create(file.path("./Plots/", "Null_model_plots"), showWarnings = FALSE)
dir.create(file.path("./Plots/", "Datapoint_plots" ), showWarnings = FALSE)

dir.create(file.path("./", "Results"), showWarnings = FALSE)
dir.create(file.path("./Results/", "Plots"), showWarnings = FALSE)
dir.create(file.path("./Results/Plots/", "LCC_count"), showWarnings = FALSE)
dir.create(file.path("./Results/Plots/", "LCC_abun"), showWarnings = FALSE)
dir.create(file.path("./Results/Plots/LCC_count/", "mcp_plots"), showWarnings = FALSE)
dir.create(file.path("./Results/Plots/LCC_abun/", "mcp_plots"), showWarnings = FALSE)
dir.create(file.path("./Results/", "Cross_validation"), showWarnings = FALSE)
dir.create(file.path("./Results/", "Granger_causality"), showWarnings = FALSE)
dir.create(file.path("./Results/", "Commonality_analysis"), showWarnings = FALSE)
dir.create(file.path("./Results/", "multiple_change_point"), showWarnings = FALSE)

source("./R_scripts/1_Script1_bigdf_with_familynames.R")

if (relative_abun==T)
  {
  source("./R_scripts/2_Compute_relative_abundance.R")
  }
source("./R_scripts/3_archaeological_data.R")
source("./R_scripts/4_Calibrate_archeological_data.R")
source("./R_scripts/5_SPD.R")
source("./R_scripts/6_Plot_human_calibration_results.R")
source("./R_scripts/7_Climatic_data.R")
source("./R_scripts/8_all_lat_long_grids.R")

if (relative_abun==T)
  {
source("./R_scripts/9_LCC.R")
source("./R_scripts/10_smoothing.R")
  if (mcp == T) {
source("./R_scripts/11_MCP.R")
  }
source("./R_scripts/12_cross_validation.R")
source("./R_scripts/13_grangercausality.R")
source("./R_scripts/14_final_plots.R")
}

if (relative_abun==F)
{
  source("./R_scripts/LCC_count_scripts/9_LCC_Count.R")
  source("./R_scripts/LCC_count_scripts/10_smoothing_Count.R")
    if (mcp == T) {
  source("./R_scripts/LCC_count_scripts/11_MCP_Count.R")
  }
  source("./R_scripts/LCC_count_scripts/12_cross_validation_Count.R")
  source("./R_scripts/LCC_count_scripts/13_grangercausality_Count.R")
  source("./R_scripts/LCC_count_scripts/14_final_plots_Count.R")
}

print("Results and plots for Granger causality models can be found in ./Results/")
print("Maps and calibration plots can be found in ./Plots/ ")
