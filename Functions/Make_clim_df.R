make_clim_df = function(all_temp_sites, listname){
  age = all_temp_sites$listname$paleoData[[1]]$measurementTable[[1]]$age$values
  temp = all_temp_sites$listname$paleoData[[1]]$measurementTable[[1]]$temperature$values
  df <- data.frame(age, temp)
  make_clim_df = df
}