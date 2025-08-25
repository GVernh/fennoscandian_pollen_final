all_temp_sites = readRDS("./Processed_data/all_temp_sites.RDS")

### Functions ###
source("./Functions/Make_clim_df.R")
source("./Functions/MakestandardDF.R")

age = all_temp_sites$AgeroedsMosse.Nilsson.1964$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$AgeroedsMosse.Nilsson.1964$paleoData[[1]]$measurementTable[[1]]$temperature$values
df <- data.frame(age, temp)
df$site = "AgeroedsMosse.Nilsson.1964" 

# changed to get rid of the error..
df$long= all_temp_sites$AgeroedsMosse.Nilsson.1964$geo$longitude
df$lat=all_temp_sites$AgeroedsMosse.Nilsson.1964$geo$latitude
makeStandardDF = df


test = make_clim_df(all_temp_sites, "850Lake.Shemesh.2001")

test.list <- NULL
for (site in clim15$site) {
  age = all_temp_sites$site$paleoData[[1]]$measurementTable[[1]]$age$values
  temp = all_temp_sites$site$paleoData[[1]]$measurementTable[[1]]$temperature$values
  df <- data.frame(age, temp)
  namelist = df
  if(site=="850Lake.Shemesh.2001"){
    test.list <- namelist
  }
  else{
    test.list <- dplyr::bind_rows(test.list, namelist)
  }
}
