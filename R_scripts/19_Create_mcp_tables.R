# Create change point table 
libs <- c("tidyverse", "rlist")

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

# Data
data = list.files("./Results/multiple_change_point/",
           pattern = ".csv",
           recursive = F)

df_list <- list()

# Loop over data to create cleaned table of selected multiple change points

for (i in 1:length(data)) {
  print(i)
  mcp_dat <- read.csv(paste0("./Results/multiple_change_point/", data[[i]]))
  name = data[[i]]
  name = gsub('[alldata_abun.csv]', '', name)
  
  cleaned_mcp <- mcp_dat %>%
    dplyr::select(mean,lower, upper, BF, ID)%>%
    dplyr::mutate(Region = name) %>%
    dplyr::mutate(ID = gsub(name, "", ID)) %>%
    dplyr::mutate(ID = recode(ID, "clim" = "Climate", "cons" = "Coniferous woodland", "decs" = "Dedicduous woodland", "wetws" = "Wet woodland",
                              "wetms" = "Wet meadow", "pass" = "Pasture", "heas" = "Heath", "aras" = "Arable")) %>% 
    dplyr::filter(BF >10) %>%
    dplyr::mutate_if(is.numeric, round, digits = 1) %>%
    dplyr::rename("Mean" = mean, 
                  "Lower" = lower,
                  "Upper" = upper,
                  "Dataset" = ID) %>%
    dplyr::select(Region, Dataset, Mean, Lower, Upper, BF)
  
  df_list[[i]] <- cleaned_mcp 
}

# Create finalised dataframe
comp_df <- rlist::list.rbind(df_list)

comp_df <- comp_df %>%
  dplyr::mutate(Region = recode(Region, "MM" = "Central", "MW" = "Midwest", "N" = "North", "SM" = "Southmid",
                                "SW" = "Southwest"))

write.csv(comp_df, "./Results/Plots/LCC_abun/Selected_MCP_table.csv", row.names = F)

