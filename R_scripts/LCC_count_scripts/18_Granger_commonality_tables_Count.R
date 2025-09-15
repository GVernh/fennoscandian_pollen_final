
# Load library ----
libs <- c("tidyverse")

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

####################################
####### CLEAN COMMONALITY DATA #####
####################################

data <- read.csv("./Results/Commonality_analysis/Count_data/No_sig_stars/Com_analysis_NORTH_count.csv")

North_com_long <- data %>%
  pivot_longer(cols=c(Coniferous.woodland,Deciduous.woodland,Wet.woodland,
                      Wet.meadow,Pasture,Arable.land,Heath),
               names_to='LCC',
               values_to='Coef.') %>%
  rename(Variable = "X") %>%
  mutate(Region = "North")

data <- read.csv("./Results/Commonality_analysis/Count_data/No_sig_stars/Com_analysis_MIDMID_count.csv")

Central_com_long <- data %>%
  pivot_longer(cols=c(Coniferous.woodland,Deciduous.woodland,Wet.woodland,
                      Wet.meadow,Pasture,Arable.land,Heath),
               names_to='LCC',
               values_to='Coef.') %>%
  rename(Variable = "X") %>%
  mutate(Region = "Central")

data <- read.csv("./Results/Commonality_analysis/Count_data/No_sig_stars/Com_analysis_MIDWEST_count.csv")

Midwest_com_long <- data %>%
  pivot_longer(cols=c(Coniferous.woodland,Deciduous.woodland,Wet.woodland,
                      Wet.meadow,Pasture,Arable.land,Heath),
               names_to='LCC',
               values_to='Coef.') %>%
  rename(Variable = "X") %>%
  mutate(Region = "Midwest")

data <- read.csv("./Results/Commonality_analysis/Count_data/No_sig_stars/Com_analysis_SOUTHEAST_count.csv")

Southeast_com_long <- data %>%
  pivot_longer(cols=c(Coniferous.woodland,Deciduous.woodland,Wet.woodland,
                      Wet.meadow,Pasture,Arable.land,Heath),
               names_to='LCC',
               values_to='Coef.') %>%
  rename(Variable = "X") %>%
  mutate(Region = "Southeast")

data <- read.csv("./Results/Commonality_analysis/Count_data/No_sig_stars/Com_analysis_SOUTHMID_count.csv")

Southmid_com_long <- data %>%
  pivot_longer(cols=c(Coniferous.woodland,Deciduous.woodland,Wet.woodland,
                      Wet.meadow,Pasture,Arable.land,Heath),
               names_to='LCC',
               values_to='Coef.') %>%
  rename(Variable = "X") %>%
  mutate(Region = "Southmid")


data <- read.csv("./Results/Commonality_analysis/Count_data/No_sig_stars/Com_analysis_SOUTHWEST_count.csv")

Southwest_com_long <- data %>%
  pivot_longer(cols=c(Coniferous.woodland,Deciduous.woodland,Wet.woodland,
                      Wet.meadow,Pasture,Arable.land,Heath),
               names_to='LCC',
               values_to='Coef.') %>%
  rename(Variable = "X") %>%
  mutate(Region = "Southwest")

com_data_comp = rbind(Southmid_com_long, Southeast_com_long, Southwest_com_long, 
                      Midwest_com_long, Central_com_long, North_com_long) %>%
  dplyr::mutate(LCC = recode(LCC, 
                             "Wet.woodland" = "Wet woodland",
                             "Coniferous.woodland" = "Coniferous woodland",
                             "Deciduous.woodland" = "Deciduous woodland",
                             "Arable.land" = "Arable",
                             "Heath" = "Heathland",
                             "Wet.meadow" = "Wet meadow"))  %>%
  dplyr::rename(Causality = Variable)

com_data_comp_all <- com_data_comp %>%
  filter(Causality == "SPD" | Causality == "Climate" | Causality == "All")%>%
  mutate(Causality = recode(Causality,
                            "Climate" = "clim",
                            "All" = "SPD|clim"))

com_data_comp_before <- com_data_comp %>%
  filter(Causality == "SPD_before" | Causality == "Climate_before" | Causality == "All_before")%>%
  mutate(Causality = recode(Causality,
                            "SPD_before" = "SPD",
                            "Climate_before" = "clim",
                            "All_before" = "SPD|clim"))

com_data_comp_after <- com_data_comp %>%
  filter(Causality == "SPD_after" | Causality == "Climate_after" | Causality == "All_after") %>%
  mutate(Causality = recode(Causality,
                            "SPD_after" = "SPD",
                            "Climate_after" = "clim",
                            "All_after" = "SPD|clim"))

# Count - All data ----

# Data removal lists

remove_area <- c("SMs", "Ns", "SEs", "SWs", "MMs", "MWs")
remove_LCC <- c("con", "dec", "wetw", "pas", "ara", "hea", "wetm")
results = read.csv("./Results/Granger_causality/Granger_results_allData_count.csv")

# Data clean

Sig_table_all_count <- results %>%
  dplyr::select(!(c(Excluded, df1,df2, df))) %>%
  mutate(LCC = str_remove_all(Equation, paste(remove_area, collapse = "|"))) %>%
  mutate(LCC = recode(LCC, 
                      "con" = "Coniferous woodland",
                      "dec" = "Deciduous woodland",
                      "wetw" = "Wet woodland",
                      "pas" = "Pasture",
                      "ara" = "Arable",
                      "hea" = "Heathland",
                      "wetm" = "Wet meadow")) %>%
  mutate(Region = str_remove_all(Equation, paste(remove_LCC, collapse = "|"))) %>%
  mutate(Region = recode(Region, 
                         "SMs" = "Southmid",
                         "Ns" = "North",
                         "SEs" = "Southeast",
                         "SWs" = "Southwest",
                         "MMs" = "Central",
                         "MWs" = "Midwest")) %>%
  dplyr::mutate(sig.Chisq = str_trim(sig.Chisq, side = "both")) %>%
  dplyr::filter(sig.Chisq == '*'| sig.Chisq == "**"| sig.Chisq == "***") %>%
  dplyr::mutate(p.F = round(p.F, 2)) %>%
  dplyr::mutate("F" = round(.[["F"]], 3)) %>%
  dplyr::select("F", p.F, Causality, LCC, Region) %>%
  dplyr::rename(P = p.F) %>%
  dplyr::mutate(Causality = gsub("^.{0,10}", "", Causality)) %>%
  dplyr::mutate(Causality = str_trim(Causality, side = "both")) %>%
  dplyr::select(Region, LCC, Causality, F, P)

# Merge
Sig_table_all_count <- merge(Sig_table_all_count, com_data_comp_all, by = c("Region", "LCC", "Causality"))

write.csv(Sig_table_all_count, "./Results/Plots/LCC_count/Significant_results_all_count.csv")
rm(results)
# Count - Before farming ----

# Data removal lists

remove_area <- c("SMs", "Ns", "SEs", "SWs", "MMs", "MWs")
remove_LCC <- c("con", "dec", "wetw", "pas", "ara", "hea", "wetm")
results = read.csv("./Results/Granger_causality/Granger_results_beforeFarming_count.csv")

# Data clean

Sig_table_before_count <- results %>%
  dplyr::select(!(c(Excluded, df1,df2, df))) %>%
  mutate(LCC = str_remove_all(Equation, paste(remove_area, collapse = "|"))) %>%
  mutate(LCC = recode(LCC, 
                      "con" = "Coniferous woodland",
                      "dec" = "Deciduous woodland",
                      "wetw" = "Wet woodland",
                      "pas" = "Pasture",
                      "ara" = "Arable",
                      "hea" = "Heathland",
                      "wetm" = "Wet meadow")) %>%
  mutate(Region = str_remove_all(Equation, paste(remove_LCC, collapse = "|"))) %>%
  mutate(Region = recode(Region, 
                         "SMs" = "Southmid",
                         "Ns" = "North",
                         "SEs" = "Southeast",
                         "SWs" = "Southwest",
                         "MMs" = "Central",
                         "MWs" = "Midwest")) %>%
  dplyr::mutate(sig.Chisq = str_trim(sig.Chisq, side = "both")) %>%
  dplyr::filter(sig.Chisq == '*'| sig.Chisq == "**"| sig.Chisq == "***") %>%
  dplyr::mutate(p.F = round(p.F, 2)) %>%
  dplyr::mutate("F" = round(.[["F"]], 3)) %>%
  dplyr::select("F", p.F, Causality, LCC, Region) %>%
  dplyr::rename(P = p.F) %>%
  dplyr::mutate(Causality = gsub("^.{0,10}", "", Causality)) %>%
  dplyr::mutate(Causality = str_trim(Causality, side = "both")) %>%
  dplyr::select(Region, LCC, Causality, F, P)

# Merge
Sig_table_before_count <- merge(Sig_table_before_count, com_data_comp_before, by = c("Region", "LCC", "Causality"))

write.csv(Sig_table_before_count, "./Results/Plots/LCC_count/Significant_results_before_count.csv")
rm(results)
# Count - After farming ----

# Data removal lists

remove_area <- c("SMs", "Ns", "SEs", "SWs", "MMs", "MWs")
remove_LCC <- c("con", "dec", "wetw", "pas", "ara", "hea", "wetm")
results = read.csv("./Results/Granger_causality/Granger_results_afterFarming_count.csv")

# Data clean

Sig_table_after_count <- results %>%
  dplyr::select(!(c(Excluded, df1,df2, df))) %>%
  mutate(LCC = str_remove_all(Equation, paste(remove_area, collapse = "|"))) %>%
  mutate(LCC = recode(LCC, 
                      "con" = "Coniferous woodland",
                      "dec" = "Deciduous woodland",
                      "wetw" = "Wet woodland",
                      "pas" = "Pasture",
                      "ara" = "Arable",
                      "hea" = "Heathland",
                      "wetm" = "Wet meadow")) %>%
  mutate(Region = str_remove_all(Equation, paste(remove_LCC, collapse = "|"))) %>%
  mutate(Region = recode(Region, 
                         "SMs" = "Southmid",
                         "Ns" = "North",
                         "SEs" = "Southeast",
                         "SWs" = "Southwest",
                         "MMs" = "Central",
                         "MWs" = "Midwest")) %>%
  dplyr::mutate(sig.Chisq = str_trim(sig.Chisq, side = "both")) %>%
  dplyr::filter(sig.Chisq == '*'| sig.Chisq == "**"| sig.Chisq == "***") %>%
  dplyr::mutate(p.F = round(p.F, 2)) %>%
  dplyr::mutate("F" = round(.[["F"]], 3)) %>%
  dplyr::select("F", p.F, Causality, LCC, Region) %>%
  dplyr::rename(P = p.F) %>%
  dplyr::mutate(Causality = gsub("^.{0,10}", "", Causality)) %>%
  dplyr::mutate(Causality = str_trim(Causality, side = "both")) %>%
  dplyr::select(Region, LCC, Causality, F, P)

# Merge
Sig_table_after_count <- merge(Sig_table_after_count, com_data_comp_after, by = c("Region", "LCC", "Causality"))

write.csv(Sig_table_after_count, "./Results/Plots/LCC_count/Significant_results_after_count.csv")


