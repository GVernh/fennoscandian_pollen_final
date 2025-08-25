### Install/load libraries ### ----
libs <- c("dplyr","ggplot2", "rnaturalearth", "ggspatial", "sf", "rnaturalearthdata")

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

### LOAD POLLEN, CLIMATE & ARCHEOLOGICAL DATA ### ----
archaeological_cal <- read.csv("./Processed_data/archeological_data/archaeological_cal.csv")
All_Age_Depth_Curves = readRDS(file = "./Raw_Data/All_Age_Depth_Curves_2021-11-23") 
pollen_all = readRDS(file = "./Raw_Data/KMH_S1_pollen_all_2021-11-22")
climatic_list <- readRDS("./Processed_data/Climate/temp.list.RDS")

### CREATE DIRECTORIES ###
dir.create(file.path("./Processed_data/", "gridded_data"), showWarnings = FALSE)
dir.create(file.path("Plots"), showWarnings = FALSE)
dir.create(file.path("./Plots/", "Datapoint_plots" ), showWarnings = FALSE)

### FUNCTIONS ###
source("./Functions/LonglatDF.R")
source("./Functions/Plot_grids.R")

###  EXTRACT LAT/LONG DATA ###
climate_lat_long = NULL
for (i in 1:length(climatic_list)) {
  namelist <- data.frame(site=climatic_list[[i]]$site, long=climatic_list[[i]]$long, lat=climatic_list[[i]]$lat)
  if(i==1){
    climate_lat_long <- namelist
  }
  else{
    climate_lat_long <- dplyr::bind_rows(climate_lat_long, namelist)
  }
}

pollen_long_lat = NULL
for(name in names(All_Age_Depth_Curves))
{
  namedf = longlatDF(pollen_all, name)
  if(name=="12"){
    pollen_long_lat = namedf
  }
  else{
    pollen_long_lat = dplyr::bind_rows(pollen_long_lat, namedf)
  }
}

### MAP ###
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

(sites.arch <- data.frame(archaeological_cal))
(sites.climate <- data.frame(climate_lat_long))
(sites.pollen <- pollen_long_lat)

All_sites <- ggplot2::ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_point(data = sites.arch, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "orange") +
  geom_point(data = sites.pollen, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "green4") +
  #geom_point(data = sites.climate, aes(x = long, y = lat), size = 4, 
   #          shape = 21, fill = "blue") +
  coord_sf(xlim = c(4, 42), ylim = c(55, 72), expand = FALSE) +
  ggtitle("Fennoscandian datapoints") +
  labs(x = "Longitude",y = "Latitude",fill = "Legend") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.2),
        panel.background = element_rect(fill = "aliceblue"))
#theme(legend.title = element_blank()) + # omit plot title saying 'color'
#scale_fill_manual(values = c('darkred', 'darkgreen'),
#                  labels = c('archaeological', 'pollen'))

ggplot2::ggsave(All_sites, 
       filename = "./Plots/Datapoint_plots/Fennoscandian_datapoints.png",
       device = "png",
       height = 6, width = 6, units = "in")


# make the grids (2°lat, 5°long)
#-------------------------------------------------------------------------------
#3 lat=beginning-60, long=10-15
clim3 <- subset(climate_lat_long, lat<=60 & long>10 & long<=15)
pol3 <- subset(pollen_long_lat, lat<=60 & long>10 & long<=15)
arc3 <- subset(archaeological_cal, lat<=60 & long>10 & long<=15)
#plot_grids(arc3, pol3, clim3)

#4 lat=60-65, long=10-15
clim4 <- subset(climate_lat_long, lat>60 & lat<=65 & long>10 & long<=15)
pol4 <- subset(pollen_long_lat, lat>60 & lat<=65 & long>10 & long<=15)
arc4 <- subset(archaeological_cal, lat>60 & lat<=65 & long>10 & long<=15)
#plot_grids(arc4, pol4, clim4)

#5 lat=65-end, long=10-15 #NO POLLEN SITE
clim5 <- subset(climate_lat_long, lat>65 & long>10 & long<=15)
pol5 <- subset(pollen_long_lat, lat>65 & long>10 & long<=15)
arc5 <- subset(archaeological_cal, lat>65 & long>10 & long<=15)
#plot_grids(arc5, pol5, clim5)

#6 lat=beginning-60, long=15-20 #NO POLLEN SITE
clim6 <- subset(climate_lat_long, lat<=60 & long>15 & long<=20)
pol6 <- subset(pollen_long_lat, lat<=60 & long>15 & long<=20)
arc6 <- subset(archaeological_cal, lat<=60 & long>15 & long<=20)
#plot_grids(arc6, pol6, clim6)

#7 lat=60-65, long=15-20 #NO POLLEN SITE
clim7 <- subset(climate_lat_long, lat>60 & lat<=65 & long>15 & long<=20)
pol7 <- subset(pollen_long_lat, lat>60 & lat<=65 & long>15 & long<=20)
arc7 <- subset(archaeological_cal, lat>60 & lat<=65 & long>15 & long<=20)
#plot_grids(arc7, pol7, clim7)

#8 lat=65-end, long=15-20 #NO POLLEN SITE
clim8 <- subset(climate_lat_long, lat>65 & long>15 & long<=20)
pol8 <- subset(pollen_long_lat, lat>65 & long>15 & long<=20)
arc8 <- subset(archaeological_cal, lat>65 & long>15 & long<=20)
#plot_grids(arc8, pol8, clim8)

#9 lat=beginning-65, long=20-25 #MINUS ONE SITE FROM OTHER COAST
clim9 <- subset(climate_lat_long, lat<=65 & long>20 & long<=25)
pol9 <- subset(pollen_long_lat, lat<=65 & long>20 & long<=25)
arc9 <- subset(archaeological_cal, lat<=65 & long>20 & long<=25)
#plot_grids(arc9, pol9, clim9)

#10 lat=65-end, long=20-25
clim10 <- subset(climate_lat_long, lat>65 & long>20 & long<=25)
pol10 <- subset(pollen_long_lat, lat>65 & long>20 & long<=25)
arc10 <- subset(archaeological_cal, lat>65 & long>20 & long<=25)
#plot_grids(arc10, pol10, clim10)

#11 lat=beginning-65, long=25-30
clim11 <- subset(climate_lat_long, lat<=65 & long>25 & long<=30)
pol11 <- subset(pollen_long_lat, lat<=65 & long>25 & long<=30)
arc11 <- subset(archaeological_cal, lat<=65 & long>25 & long<=30)
#plot_grids(arc11, pol11, clim11)

#12 lat=65-end, long=25-30
clim12 <- subset(climate_lat_long, lat>65 & long>25 & long<=30)
pol12 <- subset(pollen_long_lat, lat>65 & long>25 & long<=30)
arc12 <- subset(archaeological_cal, lat>65 & long>25 & long<=30)
#plot_grids(arc12, pol12, clim12)

#13 lat=beginning-65, long=30-end #NO CLIMATE DATA
clim13 <- subset(climate_lat_long, lat<=65 & long>30)
pol13 <- subset(pollen_long_lat, lat<=65 & long>30)
arc13 <- subset(archaeological_cal, lat<=65 & long>30)
#plot_grids(arc13, pol13, clim13)

#14 lat=65-end, long=30-end #NO CLIMATE DATA
clim14 <- subset(climate_lat_long, lat>65 & long>30)
pol14 <- subset(pollen_long_lat, lat>65 & long>30)
arc14 <- subset(archaeological_cal, lat>65 & long>30)
#plot_grids(arc14, pol14, clim14)

#16 
clim16 <- subset(climate_lat_long, lat>65 & lat<=67.5)
pol16 <- subset(pollen_long_lat, lat>65 & lat<=67.5)
arc16 <- subset(archaeological_cal, lat>65 & lat<=67.5)
#plot_grids(arc16, pol16, clim16)

#FINAL GRIDS
#-------------------------------------------------------------------------------
#15 northernmost
clim15 <- subset(climate_lat_long, lat>67.5)
pol15 <- subset(pollen_long_lat, lat>67.5)
arc15 <- subset(archaeological_cal, lat>67.5)

#Save
write.csv(clim15, file = "./Processed_data/gridded_data/clim15.csv", row.names = FALSE)
write.csv(pol15, file = "./Processed_data/gridded_data/pol15.csv", row.names = FALSE)
write.csv(arc15, file = "./Processed_data/gridded_data/arc15.csv", row.names = FALSE)

#1 lat=beginning-60, long=beginning-10 #SW
clim1 <- subset(climate_lat_long, lat <= 60 & long<=10)
pol1 <- subset(pollen_long_lat, lat <= 60 & long <= 10)
arc1 <- subset(archaeological_cal, lat <= 60 & long <= 10)

#Save
write.csv(clim1, file = "./Processed_data/gridded_data/clim1.csv", row.names = FALSE)
write.csv(pol1, file = "./Processed_data/gridded_data/pol1.csv", row.names = FALSE)
write.csv(arc1, file = "./Processed_data/gridded_data/arc1.csv", row.names = FALSE)

#2 lat=60-end, long=beginning-10 #MW
clim2 <- subset(climate_lat_long, lat>60 & long<=10)
pol2 <- subset(pollen_long_lat, lat>60 & long<=10)
arc2 <- subset(archaeological_cal, lat>60 & long<=10)

#Save
write.csv(clim2, file = "./Processed_data/gridded_data/clim2.csv", row.names = FALSE)
write.csv(pol2, file = "./Processed_data/gridded_data/pol2.csv", row.names = FALSE)
write.csv(arc2, file = "./Processed_data/gridded_data/arc2.csv", row.names = FALSE)

#3+6 #SM
clim36 <- subset(climate_lat_long, lat<=60 & long>10 & long<=20)
pol36 <- subset(pollen_long_lat, lat<=60 & long>10 & long<=20)
arc36 <- subset(archaeological_cal, lat<=60 & long>10 & long<=20)

#Save
write.csv(clim36, file = "./Processed_data/gridded_data/clim36.csv", row.names = FALSE)
write.csv(pol36, file = "./Processed_data/gridded_data/pol36.csv", row.names = FALSE)
write.csv(arc36, file = "./Processed_data/gridded_data/arc36.csv", row.names = FALSE)

#4+7 #MM
clim47 <- subset(climate_lat_long, lat>60 & lat<=65 & long>10 & long<=20)
pol47 <- subset(pollen_long_lat, lat>60 & lat<=65 & long>10 & long<=20)
arc47 <- subset(archaeological_cal, lat>60 & lat<=65 & long>10 & long<=20)

#Save
write.csv(clim47, file = "./Processed_data/gridded_data/clim47.csv", row.names = FALSE)
write.csv(pol47, file = "./Processed_data/gridded_data/pol47.csv", row.names = FALSE)
write.csv(arc47, file = "./Processed_data/gridded_data/arc47.csv", row.names = FALSE)

#9+11+13 #SE
clim911 <- subset(climate_lat_long, lat>60 & lat<=65 & long>20)
pol911 <- subset(pollen_long_lat, lat>60 & lat<=65 & long>20)
arc911 <- subset(archaeological_cal, lat>60 & lat<=65 & long>20)
arc911 <- arc911[-c(arc911$LabID=="U 579"),] #on the other side of the coast
arc911 <- arc911[-c(arc911$LabID=="U-579"),] #on the other side of the coast
arc911 <- arc911[-c(arc911$LabID=="U-612"),] #on the other side of the coast

#Save
write.csv(clim911, file = "./Processed_data/gridded_data/clim911.csv", row.names = FALSE)
write.csv(pol911, file = "./Processed_data/gridded_data/pol911.csv", row.names = FALSE)
write.csv(arc911, file = "./Processed_data/gridded_data/arc911.csv", row.names = FALSE)

clim_no_use <- subset(climate_lat_long, lat>65 & lat<67.5)

plot_grid_all <- ggplot2::ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_point(data = arc1, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "yellow") +
  geom_point(data = pol1, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "orange") +
  geom_point(data = clim1, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "red") +
  geom_point(data = arc15, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "deepskyblue") +
  geom_point(data = pol15, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "blue") +
  geom_point(data = clim15, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "blueviolet") +
  geom_point(data = arc2, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "green") +
  geom_point(data = pol2, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "green4") +
  geom_point(data = clim2, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "darkgreen") +
  geom_point(data = arc36, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "white") +
  geom_point(data = pol36, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "grey") +
  geom_point(data = clim36, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "black") +
  geom_point(data = arc47, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "tan") +
  geom_point(data = pol47, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "tan1") +
  geom_point(data = clim47, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "tan4") +
  geom_point(data = arc911, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "darkred") +
  geom_point(data = pol911, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "darkgreen") +
  geom_point(data = clim911, aes(x = long, y = lat), size = 4, 
             shape = 21, fill = "darkblue") +
  coord_sf(xlim = c(4, 42), ylim = c(55, 72), expand = FALSE) +
  ggtitle("Fennoscandian datapoints") +
  labs(x = "Longitude",y = "Latitude",fill = "Legend") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.2),
        panel.background = element_rect(fill = "aliceblue"))

### LIST PLOTS ###
plots <- NULL
tags <- c("15", "1", "2", "36", "47", "911", "1_no_clim", "all")

plots[[1]] <- plot_grids(arc15, pol15, clim15)
plots[[2]] <- plot_grids(arc1, pol1, clim1)
plots[[3]] <- plot_grids(arc2, pol2, clim2)
plots[[4]] <- plot_grids(arc36, pol36, clim36)
plots[[5]] <- plot_grids(arc47, pol47, clim47)
plots[[6]] <- plot_grids(arc911, pol911, clim911)
plots[[7]] <- plot_grids(arc1, pol1, clim_no_use)
plots[[8]] <- plot_grid_all

### LOOP & SAVE PLOTS ###
for (i in seq_along(plots)){
  if (!(paste0("arc_pol_clim_",tags[i],".png") %in% list.files("./Plots/Datapoint_plots/"))) {
    ggplot2::ggsave(plots[[i]], 
           filename = paste0("./Plots/Datapoint_plots/arc_pol_clim_",tags[i], ".png"),
           device = "png",
           height = 6, width = 5, units = "in")
  }
}