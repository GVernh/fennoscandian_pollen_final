# Install/load libraries
libs <- c("dplyr")

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

# Source functions
source("./Functions/Compute_relative_abundance.R")

# Load raw data
load("./Raw_Data/bigdf_familynames.Rda")
time_ID = bigdf_familynames[c("dataset_ID", "meantimes")] # subset time/site data

####
#y = bigdf_familynames[!duplicated(colnames(bigdf_familynames))]
#y = dplyr::select(y, -colnames(time_ID))
#d = as.data.frame(rowSums(y, na.rm=T)) # To check relative abundance calculations
####

# Apply data quality control and compute relative abundance
pollen_relative_abun <- bigdf_familynames %>%
  {bigdf_familynames[!duplicated(colnames(bigdf_familynames))]} %>% # "Cyperaceae" is duplicated in the data.
  dplyr::select(-colnames(time_ID)) %>%
  as.matrix(.) %>%
  compute_relative_abundance(.) %>%
  cbind(time_ID) %>%
  dplyr::select(colnames(time_ID), everything(.))

#There are a number of identical values in the relative abundance 
#data. Please check that the relative abundance data is calulated
#correctly.

dir.create(file.path("./", "Processed_data"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "LCC_data"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/LCC_data/", "LCC_raw"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/LCC_data/", "LCC_abun"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/LCC_data/LCC_abun/", "Pollen_data"), showWarnings = FALSE)
save(pollen_relative_abun,file="./Processed_data/LCC_data/LCC_abun/Pollen_data/Pollen_relative_abun.Rda")
