### Climatic data ###
# NOTE: lipdR package will be installed from github

### Load/Install packages ----
libs <- c("dplyr","ggplot2", "remotes")

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

if (!(paste0("Temp12k_v1_0_0.RData") %in% list.files("./Raw_Data/"))){
  zipF<- "./Raw_Data/Temp12k_v1_0_0.RData.zip"
  outDir<-"./Raw_Data/"
  unzip(zipF,exdir=outDir)
}

### Load Data ### ----
load("./Raw_Data/Temp12k_v1_0_0.RData")
climate = TS
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

dir.create(file.path("./Processed_data/", "Climate"), showWarnings = FALSE)

### Functions ###
source("./Functions/ClimateDF.R")
source("./Functions/Create_climate_list.R")

### List selected climate data sets ###
source("./Processed_data/Climate/Selected_climate_datasets.R")

### Create climate data frame ###
climate.df = NULL
for(i in 1:length(climate))
{
  namedf = climateDF(climate, i)
  if(i==1){
    climate.df = namedf
  }
  else{
    climate.df = dplyr::bind_rows(climate.df, namedf)
  }
}

### Data cleaning ###
climate.points = climate.df %>%
  dplyr::filter(long >= 4 & long < 42) %>%
  dplyr::filter(lat >= 55 & lat < 71) %>%
  dplyr::filter(grepl(c('Sweden|Finland|Norway|Russia'), location)) # NOTE: Russia is not in the dataset
#climate.points <- subset(climate.points, Age <= 15000) # GRANT: No variable "Age"?

write.csv(climate.points, file = "./Processed_data/Climate/climate.points.csv")
temp_longlat <- data.frame(climate.points$dataSet, climate.points$long, climate.points$lat)


### Map ###
(sites <- data.frame(longitude = climate.points$long, latitude = climate.points$lat))

ggplot2::ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(4, 42), ylim = c(55, 71), expand = FALSE) +
  ggtitle("climatic.data")

climateSets <- list(unique(climate.points$dataSet))


### Create .lpd datasets from climate data ###
dir.create(file.path("./Processed_data/", "lpd_datasets"), showWarnings = FALSE)

for (i in seq_along(Selected_climate_data)){
  if (!(paste0(Selected_climate_data[[i]], ".lpd") %in% list.files("./Processed_data/lpd_datasets/"))) {
    print(Selected_climate_data[[i]])
    writeLipd(D[paste0(Selected_climate_data[[i]])], "Processed_data/lpd_datasets/")
  }
}

lpd_filenames = list.files("./Processed_data/lpd_datasets/",
               pattern = ".lpd",
               all.files = F,
               recursive = F)
all_temp_sites = readLipd(paste0("./Processed_data/lpd_datasets/", lpd_filenames))

saveRDS(all_temp_sites, file = "./Processed_data/Climate/all_temp_sites.RDS")

temp.list <- NULL
for (i in 1:length(all_temp_sites)) {
  namelist <- climate_list(climate.points,all_temp_sites, i)
  if(i=="Lake.Shemesh.2001"){
    temp.list <- namelist
  }
  else{
    temp.list <- append(temp.list, namelist)
  }
}

### Plot individual sites ###
plot(temp.list[[15]]$age, temp.list[[15]]$temp, type = "l")
plot(temp.list[[1]]$age, temp.list[[1]]$temp, type = "l", ylim = c(1,12))
par(new=TRUE)
plot(temp.list[[2]]$age, temp.list[[2]]$temp, type = "l", ylim = c(1,12))


#### calibration methods used ###
calibration.list = NULL
for (i in 1:length(all_temp_sites)) {
  namelist <- list(temp.list[[i]]$calibration)
  if(i==1){
    calibration.list <- namelist
  }
  else{
    calibration.list <- append(calibration.list, namelist)
  }
}

### ArchiveType ###
archiveType = NULL
for (i in 1:length(all_temp_sites)) {
  namelist <- list(all_temp_sites[[i]]$archiveType)
  if(i==1){
    archiveType <- namelist
  }
  else{
    archiveType <- append(archiveType, namelist)
  }
}

# saveRDS(temp.list, file = "./Processed_data/temp.list.RDS") GRANT: Why place here?

### adapt the temperatures ###
#North-summer
temp.list[[32]]$temp <- all_temp_sites$Dalmutladdo.Bjune.2004$paleoData[[1]]$measurementTable[[2]]$temperature$values
temp.list[[32]]$age <- all_temp_sites$Dalmutladdo.Bjune.2004$paleoData[[1]]$measurementTable[[2]]$age$values
temp.list[[33]]$temp <- all_temp_sites[["Donvold.Nilssen.1983"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperature-1"]][["values"]]
temp.list[[17]]$temp <- all_temp_sites[["Tsuolbmajavri.Korhola.2002"]][["paleoData"]][[2]][["measurementTable"]][[1]][["temperature"]][["values"]]
temp.list[[17]]$age <- all_temp_sites[["Tsuolbmajavri.Korhola.2002"]][["paleoData"]][[2]][["measurementTable"]][[1]][["age"]][["values"]]
temp.list[[19]]$temp <- all_temp_sites[["VuolepNjakajaure.Heinrichs.2006"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperature-2"]][["values"]]

#SouthWest-summer
temp.list[[52]]$temp <- all_temp_sites[["VestreOykjamyrtorn.EPD"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperature-1"]][["values"]]
temp.list[[39]]$temp <- all_temp_sites[["Holebudalen.Seppa.2009"]][["paleoData"]][[1]][["measurementTable"]][[2]][["temperature"]][["values"]]
temp.list[[39]]$age <- all_temp_sites[["Holebudalen.Seppa.2009"]][["paleoData"]][[1]][["measurementTable"]][[2]][["age"]][["values"]]

#SouthMid_annual
temp.list[[2]]$temp <- all_temp_sites[["AgeroedsMosse.Nilsson.1964"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]
temp.list[[4]]$temp <- all_temp_sites[["Flarken.Berglund.1966"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]
temp.list[[9]]$temp <- all_temp_sites[["Kansjon.EPD"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]

#SoutheEast_annual
temp.list[[21]]$temp <- all_temp_sites[["Kaartlamminsuo.Rankama.1988"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]
temp.list[[28]]$temp <- all_temp_sites[["Ylimysneva.Huttunen.1990"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]
temp.list[[22]]$temp <- all_temp_sites[["Laihalampi.Giesecke.2008"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]

saveRDS(temp.list, file = "./Processed_data/Climate/temp.list.RDS")