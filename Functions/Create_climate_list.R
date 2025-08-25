climate_list <- function(climate.points, all_temp_sites, list_name) {
  temp_list <- list(list(site = all_temp_sites[[list_name]]$dataSetName,
                         age = all_temp_sites[[list_name]]$paleoData[[1]]$measurementTable[[1]]$age$values,
                         temp = all_temp_sites[[list_name]]$paleoData[[1]]$measurementTable[[1]]$temperature$values,
                         long= all_temp_sites[[list_name]]$geo$longitude, lat=all_temp_sites[[list_name]]$geo$latitude,
                         calibration= all_temp_sites[[list_name]]$paleoData[[1]]$measurementTable[[1]]$temperature$calibration$method))
}