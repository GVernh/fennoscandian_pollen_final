#- LIBRARIES ----
libs <- c("ggplot2","ecp", "mcp", "dplyr", "ggpubr", "purrr")

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

#disable scientific notation
options(scipen=999)

#-FUNCTIONS ----
source("./Functions/CallSite_functions.R")
source("./Functions/Make_interval_pol.R")
source("./Functions/CountSitesPerInterval.R")
source("./Functions/sqrt_sums.R")
source("./Functions/makeIncrements_singleLCC.R")

#-DATA ----
coniferous_woodland <- read.csv("./Processed_data/LCC_data/LCC_count/coniferous_woodland.csv")
deciduous_woodland <- read.csv("./Processed_data/LCC_data/LCC_count/deciduous_woodland.csv")
wet_woodland <- read.csv("./Processed_data/LCC_data/LCC_count/wet_woodland.csv")
wet_meadow <- read.csv("./Processed_data/LCC_data/LCC_count/wet_meadow.csv")
pasture <- read.csv("./Processed_data/LCC_data/LCC_count/pasture.csv")
arable <- read.csv("./Processed_data/LCC_data/LCC_count/arable.csv")
heath <- read.csv("./Processed_data/LCC_data/LCC_count/heath.csv")

### Create directories ###
dir.create(file.path("./Processed_data/LCC_data/LCC_count/", "mcp_models"), showWarnings = FALSE)
dir.create(file.path("./", "Results"), showWarnings = FALSE)
dir.create(file.path("./Results/", "Plots"), showWarnings = FALSE)
dir.create(file.path("./Results/Plots/", "LCC_count"), showWarnings = FALSE)
dir.create(file.path("./Results/Plots/LCC_count/", "mcp_plots"), showWarnings = FALSE)

#-NORTH -------------------------------------------------------------------------
# total nr sites: 18
# 20%: 4
### coniferous_woodland SIG
coniferousN <- CallSites_N(coniferous_woodland)
coniferousN_int <- make_interval_pol(coniferousN, 100)
conN <- rowSums(coniferousN_int[3:ncol(coniferousN_int)], na.rm = TRUE)#all coniferous taxa

###############################
# The following data cleaning for each dataset should not be happening here (needs to be in LCC.R before smoothing happens)!:
conN_dat =  coniferousN_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(conN = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "conN")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(conN_dat, "./Processed_data/LCC_data/LCC_count/conN.csv", row.names = F)
#################################

if (!(paste0("mcp_ConN.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  
#########################################
  # I dont know what any of the following code is doing and it seems to be causing problems for the mcp's. I get
  # Identical results to the OG manuscript when I dont include this code.
  mcp_area <- data.frame(age=coniferousN_int$lower_ends, LCC=conN) #LCC = PinusPiceaN or conN
  #plot(mcp_area)
  #ggplot(mcp_area, aes(x=age, y=LCC))+
    #geom_line()+
    #scale_x_reverse()
  # count sites
  count <- countSitesPerInterval(coniferousN, 100)
  # added from other branch to fix the issue - JT
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=4)
  #plot(mcp_area$age, mcp_area$LCC)
##################################
  
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 100000, sample = "both", iter = 50000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_ConN.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_ConN.Rda")
}

summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("con")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_ConN.png")
plot.conN <- plot(fit_mcp)+
  ggtitle("con")
hypothesis(fit_mcp, "cp_1>5995") #20
hypothesis(fit_mcp, "cp_1<8350") #283
hypothesis(fit_mcp, "cp_1=7516") #1
#countTaxa

###deciduous_woodland
deciduousN <- CallSites_N(deciduous_woodland)
deciduousN_int <- make_interval_pol(deciduousN, 100)
decN <- rowSums(deciduousN_int[3:ncol(deciduousN_int)], na.rm = TRUE)

decN_dat =  deciduousN_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(decN = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "decN")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(decN_dat, "./Processed_data/LCC_data/LCC_count/decN.csv", row.names = F)

if (!(paste0("mcp_decN.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=deciduousN_int$lower_ends, LCC=decN)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(deciduousN, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=4)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 100000, sample = "both", iter = 20000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_decN.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_decN.Rda")
}

summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("dec")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_decN.png")
plot.decN <- plot(fit_mcp)+
  ggtitle("dec")
#NO CP'S

###wet_woodland
wetwoodlandN <- CallSites_N(wet_woodland)
wetwoodlandN_int <- make_interval_pol(wetwoodlandN, 100)
wetwN <- rowSums(wetwoodlandN_int[3:ncol(wetwoodlandN_int)], na.rm = TRUE)

wetwN_dat =  wetwoodlandN_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetwN = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetwN")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(wetwN_dat, "./Processed_data/LCC_data/LCC_count/wetwN.csv", row.names = F)

if (!(paste0("mcp_wetwN.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=wetwoodlandN_int$lower_ends, LCC=wetwN)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(wetwoodlandN, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=4)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetwN.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetwN.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("wetw")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_wetwN.png")
plot.wetwN <- plot(fit_mcp)+
  ggtitle("wetw")
hypothesis(fit_mcp, "cp_1=5477") #53 #5244
hypothesis(fit_mcp, "cp_2=8168") #53 #8133

###wet_meadow
wetmeadowN <- CallSites_N(wet_meadow)
wetmeadowN_int <- make_interval_pol(wetmeadowN, 100)
wetmN <- rowSums(wetmeadowN_int[3:ncol(wetmeadowN_int)], na.rm = TRUE)

wetmN_dat =  wetmeadowN_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetmN = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetmN")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(wetmN_dat, "./Processed_data/LCC_data/LCC_count/wetmN.csv", row.names = F)

if (!(paste0("mcp_wetmN.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=wetmeadowN_int$lower_ends, LCC=wetmN)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(wetmeadowN, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=4)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetmN.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetmN.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("wetm")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_wetmN.png")
plot.wetmN <- plot(fit_mcp)+
  ggtitle("wetm")
#NO CP. inclining

###pasture
pastureN <- CallSites_N(pasture)
pastureN_int <- make_interval_pol(pastureN, 100)
pasN <- rowSums(pastureN_int[3:ncol(pastureN_int)], na.rm = TRUE)

pasN_dat =  pastureN_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(pasN = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "pasN")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(pasN_dat, "./Processed_data/LCC_data/LCC_count/pasN.csv", row.names = F)

if (!(paste0("mcp_pasN.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=pastureN_int$lower_ends, LCC=pasN)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(pastureN, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=4)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_pasN.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_pasN.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("pas")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_pasN.png")
plot.pasN <- plot(fit_mcp)+
  ggtitle("pas")
hypothesis(fit_mcp, "cp_1=7057") #6

###arable SIG
arableN <- CallSites_N(arable)
arableN_int <- make_interval_pol(arableN, 100)
araN <- rowSums(arableN_int[3:ncol(arableN_int)], na.rm = TRUE)

araN_dat =  arableN_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(araN = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "araN")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(araN_dat, "./Processed_data/LCC_data/LCC_count/araN.csv", row.names = F)

if (!(paste0("mcp_araN.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=arableN_int$lower_ends, LCC=araN)
  mcp_area <- mcp_area[28:nrow(mcp_area),] #until 7000BP
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(arableN, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=4)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 100000, sample = "both", iter = 50000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_araN.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_araN.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("ara")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_araN.png")
plot.araN <- plot(fit_mcp)+
  ggtitle("ara")
hypothesis(fit_mcp, "cp_1=2207") #2

###heath
heathN <- CallSites_N(heath)
heathN_int <- make_interval_pol(heathN, 100)
EricaN <- rowSums(heathN_int[c(12:15)])
heaN <- rowSums(heathN_int[3:ncol(heathN_int)], na.rm = TRUE)

heaN_dat =  heathN_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(heaN = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "heaN")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(heaN_dat, "./Processed_data/LCC_data/LCC_count/heaN.csv", row.names = F)

if (!(paste0("mcp_heaN.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=heathN_int$lower_ends, LCC=heaN)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(heathN, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=4)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_heaN.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_heaN.Rda")
}
summary(fit_mcp)
plot(fit_mcp) +
  ggtitle("hea")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_heaN.png")
plot.heaN <- plot(fit_mcp)+
  ggtitle("hea")
#NO CP

### PLOTS ###
ggpubr::ggarrange(plot.conN, plot.decN, plot.wetwN, plot.wetmN, plot.pasN, plot.araN, plot.heaN + rremove("x.text"), 
          ncol = 3, nrow = 3)
# plot.spd_mcp, plot.climN # These wore included in the above plot but never defined
ggsave("./Results/Plots/mcp_plots/mcp_allN.png", width = 11, height = 9)
Full_list_N = list(conN_dat, decN_dat, wetwN_dat, wetmN_dat, pasN_dat, araN_dat, heaN_dat)

#ecp
#ecp_area <- data.frame(age=coniferousN_int$lower_ends, conN, decN, wetwN, wetmN, pasN, araN, heaN)
#ecp_sqrt <- sqrt_sums(ecp_area)
# GRANT:: Below codes currently return an error
# GRANT:: The makeIncrements_singleLCC function seems to deduct the same dataframe from itself. The error...
# is caused by the -1 wich causes the dataframe to be of a different size.

#ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
#ecpN <- ecp_sums(ecp_area, "ecp_area")
#plot_LCCecp(ecp_area, ecpN, "North_ecp")

#-SOUTHEAST ---------------------------------------------------------------------
# total nr sites: 12
# 20%: 3
###coniferous_woodland SIG
coniferousSE <- CallSites_SE(coniferous_woodland)
coniferousSE_int <- make_interval_pol(coniferousSE, 100)
conSE <- rowSums(coniferousSE_int[3:ncol(coniferousSE_int)], na.rm = TRUE)

conSE_dat =  coniferousSE_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(conSE = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "conSE")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>400 & calBP<9800)
write.csv(conSE_dat, "./Processed_data/LCC_data/LCC_count/conSE.csv", row.names = F)

if (!(paste0("mcp_conSE.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=coniferousSE_int$lower_ends, LCC=conSE)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(coniferousSE, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 100000, sample = "both", iter = 50000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_conSE.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_conSE.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("con")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_ConSE.png")
plot.conSE <- plot(fit_mcp)+
  ggtitle("con")
hypothesis(fit_mcp, "cp_1=4859") #7

###deciduous_woodland
deciduousSE <- CallSites_SE(deciduous_woodland)
deciduousSE_int <- make_interval_pol(deciduousSE, 100)
decSE <- rowSums(deciduousSE_int[3:ncol(deciduousSE_int)], na.rm = TRUE)

decSE_dat =  deciduousSE_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(decSE = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "decSE")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>400 & calBP<9800)
write.csv(decSE_dat, "./Processed_data/LCC_data/LCC_count/decSE.csv", row.names = F)

if (!(paste0("mcp_decSE.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=deciduousSE_int$lower_ends, LCC=decSE)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(deciduousSE, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_decSE.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_decSE.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("dec")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_decSE.png")
plot.decSE <- plot(fit_mcp)+
  ggtitle("dec")
hypothesis(fit_mcp, "cp_1=4553") #62

###wet_woodland
wetwoodlandSE <- CallSites_SE(wet_woodland)
wetwoodlandSE_int <- make_interval_pol(wetwoodlandSE, 100)
wetwSE <- rowSums(wetwoodlandSE_int[3:ncol(wetwoodlandSE_int)], na.rm = TRUE)

wetwSE_dat =  wetwoodlandSE_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetwSE = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetwSE")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>400 & calBP<9800)
write.csv(wetwSE_dat, "./Processed_data/LCC_data/LCC_count/wetwSE.csv", row.names = F)

mcp_area <- data.frame(age=wetwoodlandSE_int$lower_ends, LCC=wetwSE)
if (!(paste0("mcp_wetwSE.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(wetwoodlandSE, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetwSE.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetwSE.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("wetw")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_wetw.png")
plot.wetwSE <- plot(fit_mcp)+
  ggtitle("wetw")
hypothesis(fit_mcp, "cp_1=650") #114

if (!(paste0("mcp_wetwSE2.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  model = list(LCC~1+age, ~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 100000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetwSE2.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetwSE2.Rda")
}
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=650") #61
hypothesis(fit_mcp, "cp_2=4373") #4

###wet_meadow
wetmeadowSE <- CallSites_SE(wet_meadow)
wetmeadowSE_int <- make_interval_pol(wetmeadowSE, 100)
wetmSE <- rowSums(wetmeadowSE_int[3:ncol(wetmeadowSE_int)], na.rm = TRUE)

wetmSE_dat =  wetmeadowSE_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetmSE = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetmSE")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>400 & calBP<9800)
write.csv(wetmSE_dat, "./Processed_data/LCC_data/LCC_count/wetmSE.csv", row.names = F)

if (!(paste0("mcp_wetmSE.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=wetmeadowSE_int$lower_ends, LCC=wetmSE)
  #?mcp_area <- mcp_area[c(6:95),] #because there are some very high counts to the end
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(wetmeadowSE, 100)
  count <- count[c(6:95),]
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetmSE.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetmSE.Rda")
}

summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("wetm")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_wetmSE.png")
plot.wetmSE <- plot(fit_mcp)+
  ggtitle("wetm")
hypothesis(fit_mcp, "cp_1=1447") #46

###pasture
pastureSE <- CallSites_SE(pasture)
pastureSE_int <- make_interval_pol(pastureSE, 100)
pasSE <- rowSums(pastureSE_int[3:ncol(pastureSE_int)], na.rm = TRUE)

pasSE_dat =  pastureSE_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(pasSE = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "pasSE")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>400 & calBP<9800)
write.csv(pasSE_dat, "./Processed_data/LCC_data/LCC_count/pasSE.csv", row.names = F)

if (!(paste0("mcp_pasSE.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=pastureSE_int$lower_ends, LCC=pasSE)
  # count sites
  count <- countSitesPerInterval(pastureSE, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age, ~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_pasSE.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_pasSE.Rda")
}

summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("pas")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_pasSE.png")
plot.pasSE <- plot(fit_mcp)+
  ggtitle("pas")
hypothesis(fit_mcp, "cp_1=645") #45
hypothesis(fit_mcp, "cp_2=1441") #134
hypothesis(fit_mcp, "cp_3=8647") #54

###arable SIG
arableSE <- CallSites_SE(arable)
arableSE_int <- make_interval_pol(arableSE, 100)
araSE <- rowSums(arableSE_int[3:ncol(arableSE_int)], na.rm = TRUE)

araSE_dat =  arableSE_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(araSE = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "araSE")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>400 & calBP<9800)
write.csv(araSE_dat, "./Processed_data/LCC_data/LCC_count/araSE.csv", row.names = F)

if (!(paste0("mcp_araSE.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=arableSE_int$lower_ends, LCC=araSE)
  mcp_area <- mcp_area[28:nrow(mcp_area),]
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(arableSE, 100)
  count <- count[28:nrow(count),]
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 100000, sample = "both", iter = 50000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_araSE.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_araSE.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("ara")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_araSE.png")
plot.araSE <- plot(fit_mcp)+
  ggtitle("ara")
hypothesis(fit_mcp, "cp_1=650") #69

###heath
heathSE <- CallSites_SE(heath)
heathSE_int <- make_interval_pol(heathSE, 100)
EricaSE <- rowSums(heathSE_int[c(12:15)]) # GRANT:: What is this?
heaSE <- rowSums(heathSE_int[3:ncol(heathSE_int)], na.rm = TRUE)

heaSE_dat =  heathSE_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(heaSE = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "heaSE")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>400 & calBP<9800)
write.csv(heaSE_dat, "./Processed_data/LCC_data/LCC_count/heaSE.csv", row.names = F)

if (!(paste0("mcp_heaSE.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=heathSE_int$lower_ends, LCC=heaSE)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(heathSE, 100) # GRANT: this was set to arableSE originally?
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_heaSE.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_heaSE.Rda")
}

summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("hea")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_heaSE.png")
plot.heaSE <- plot(fit_mcp)+
  ggtitle("hea")
# NO CP

#ecp
# GRANT:: CODE BELOW RETURNS ERROR
#ecp_area <- data.frame(age=coniferousSE_int$lower_ends, conSE, decSE, wetwSE, wetmSE, pasSE, araSE, heaSE)
#ecp_sqrt <- sqrt_sums(ecp_area)
#ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
#ecpSE <- ecp_sums(ecp_area, "ecp_area")
#plot_LCCecp(ecp_area, ecpSE, "SouthEast_ecp")

ggpubr::ggarrange(plot.conSE, plot.decSE, plot.wetwSE, plot.wetmSE, plot.pasSE, plot.araSE, plot.heaSE + rremove("x.text"), 
          #labels = c("con", "dec", "wetw", "wetm", "pas", "ara", "hea", "SPD", "clim"),
          ncol = 3, nrow = 3)
Full_list_SE = list(conSE_dat, decSE_dat, wetwSE_dat, wetmSE_dat, pasSE_dat, araSE_dat, heaSE_dat)

#-MIDWEST ----------------------------------------------------------------------
# total nr sites: 7
# 20%: 2
###coniferous_woodland
coniferousMW <- CallSites_MW(coniferous_woodland)
coniferousMW_int <- make_interval_pol(coniferousMW, 100)
conMW <- rowSums(coniferousMW_int[3:ncol(coniferousMW_int)], na.rm = TRUE)

conMW_dat =  coniferousMW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(conMW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "conMW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(conMW_dat, "./Processed_data/LCC_data/LCC_count/conMW.csv", row.names = F)

if (!(paste0("mcp_conMW.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=coniferousMW_int$lower_ends, LCC=conMW)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(coniferousMW, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=2)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_conMW.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_conMW.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("con")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_conMW.png")
plot.conMW <- plot(fit_mcp)+
  ggtitle("con")
# NO CP

###deciduous_woodland
deciduousMW <- CallSites_MW(deciduous_woodland)
deciduousMW_int <- make_interval_pol(deciduousMW, 100)
decMW <- rowSums(deciduousMW_int[3:ncol(deciduousMW_int)], na.rm = TRUE)

decMW_dat =  deciduousMW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(decMW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "decMW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(decMW_dat, "./Processed_data/LCC_data/LCC_count/decMW.csv", row.names = F)

if (!(paste0("mcp_decMW.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=deciduousMW_int$lower_ends, LCC=decMW)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(deciduousMW, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_decMW.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_decMW.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("dec")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_decMW.png")
plot.decMW <- plot(fit_mcp)+
  ggtitle("dec")
hypothesis(fit_mcp, "cp_1=6786") #1

###wet_woodland
wetwoodlandMW <- CallSites_MW(wet_woodland)
wetwoodlandMW_int <- make_interval_pol(wetwoodlandMW, 100)
wetwMW <- rowSums(wetwoodlandMW_int[3:ncol(wetwoodlandMW_int)], na.rm = TRUE)

wetwMW_dat =  wetwoodlandMW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetwMW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetwMW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(wetwMW_dat, "./Processed_data/LCC_data/LCC_count/wetwMW.csv", row.names = F)

if (!(paste0("mcp_wetwMW.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=wetwoodlandMW_int$lower_ends, LCC=wetwMW)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(wetwoodlandMW, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetwMW.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetwMW.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("wetw")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_wetwMW.png")
plot.wetwMW <- plot(fit_mcp)+
  ggtitle("wetw")
hypothesis(fit_mcp, "cp_1=6286") #13
hypothesis(fit_mcp, "cp_2=8486") #14

###wet_meadow
wetmeadowMW <- CallSites_MW(wet_meadow)
wetmeadowMW_int <- make_interval_pol(wetmeadowMW, 100)
wetmMW <- rowSums(wetmeadowMW_int[3:ncol(wetmeadowMW_int)], na.rm = TRUE)

wetmMW_dat =  wetmeadowMW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetmMW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetmMW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(wetmMW_dat, "./Processed_data/LCC_data/LCC_count/wetmMW.csv", row.names = F)

if (!(paste0("mcp_wetmMW.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=wetmeadowMW_int$lower_ends, LCC=wetmMW)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(wetmeadowMW, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetmMW.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetmMW.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("wetm")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_wetmMW.png")
plot.wetmMW <- plot(fit_mcp)+
  ggtitle("wetm")
hypothesis(fit_mcp, "cp_1=4167") #43

###pasture SIG
pastureMW <- CallSites_MW(pasture)
pastureMW_int <- make_interval_pol(pastureMW, 100)
pasMW <- rowSums(pastureMW_int[3:ncol(pastureMW_int)], na.rm = TRUE)

pasMW_dat =  pastureMW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(pasMW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "pasMW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(pasMW_dat, "./Processed_data/LCC_data/LCC_count/pasMW.csv", row.names = F)

if (!(paste0("mcp_pasMW.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=pastureMW_int$lower_ends, LCC=pasMW)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(pastureMW, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 100000, sample = "both", iter = 50000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_pasMW.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_pasMW.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("pas")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_pasMW.png")
plot.pasMW <- plot(fit_mcp)+
  ggtitle("pas")
hypothesis(fit_mcp, "cp_1=1401") #45

###arable
arableMW <- CallSites_MW(arable)
arableMW_int <- make_interval_pol(arableMW, 100)
araMW <- rowSums(arableMW_int[3:ncol(arableMW_int)], na.rm = TRUE)

araMW_dat =  arableMW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(araMW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "araMW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(araMW_dat, "./Processed_data/LCC_data/LCC_count/araMW.csv", row.names = F)

if (!(paste0("mcp_araMW.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=arableMW_int$lower_ends, LCC=araMW)
  mcp_area <- mcp_area[28:nrow(mcp_area),]
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(arableMW, 100)
  count <- count[28:nrow(count),]
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_araMW.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_araMW.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("ara")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_araMW.png")
plot.araMW <- plot(fit_mcp)+
  ggtitle("ara")
hypothesis(fit_mcp, "cp_1=2640") #5

###heath
heathMW <- CallSites_MW(heath)
heathMW_int <- make_interval_pol(heathMW, 100)
EricaMW <- rowSums(heathMW_int[c(12:15)]) # GRANT:: Again here?
heaMW <- rowSums(heathMW_int[3:ncol(heathMW_int)], na.rm = TRUE)

heaMW_dat =  heathMW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(heaMW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "heaMW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(heaMW_dat, "./Processed_data/LCC_data/LCC_count/heaMW.csv", row.names = F)

if (!(paste0("mcp_heaMW.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=heathMW_int$lower_ends, LCC=heaMW)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(heathMW, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_heaMW.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_heaMW.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("hea")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_heaMW.png")
plot.heaMW <- plot(fit_mcp)+
  ggtitle("hea")
# NO CP. increasing

#ecp
#ecp_area <- data.frame(age=coniferousMW_int$lower_ends, conMW, decMW, wetwMW, wetmMW, pasMW, araMW, heaMW)
#ecp_sqrt <- sqrt_sums(ecp_area)
#ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
#ecpMW <- ecp_sums(ecp_area, "ecp_area")
#plot_LCCecp(ecp_area, ecpSE, "MidWest_ecp")

ggpubr::ggarrange(plot.conMW, plot.decMW, plot.wetwMW, plot.wetmMW, plot.pasMW, plot.araMW, plot.heaMW + rremove("x.text"), 
          #labels = c("con", "dec", "wetw", "wetm", "pas", "ara", "hea", "SPD", "clim"),
          ncol = 3, nrow = 3)

Full_list_MW = list(conMW_dat, decMW_dat, wetwMW_dat, wetmMW_dat, pasMW_dat, araMW_dat, heaMW_dat)

#-MIDMID -------------------------------------------------------------------------
# total nr sites: 8
# 20%: 2
###coniferous_woodland
coniferousMM <- CallSites_MM(coniferous_woodland)
coniferousMM_int <- make_interval_pol(coniferousMM, 100)
conMM <- rowSums(coniferousMM_int[3:ncol(coniferousMM_int)], na.rm = TRUE)

conMM_dat =  coniferousMM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(conMM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "conMM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(conMM_dat, "./Processed_data/LCC_data/LCC_count/conMM.csv", row.names = F)

if (!(paste0("mcp_conMM.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=coniferousMM_int$lower_ends, LCC=conMM)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(coniferousMM, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_conMM.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_conMM.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("con")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_conMM.png")
plot.conMM <- plot(fit_mcp)+
  ggtitle("con")
hypothesis(fit_mcp, "cp_1=4190") #59

###deciduous_woodland
deciduousMM <- CallSites_MM(deciduous_woodland)
deciduousMM_int <- make_interval_pol(deciduousMM, 100)
decMM <- rowSums(deciduousMM_int[3:ncol(deciduousMM_int)], na.rm = TRUE)

decMM_dat =  deciduousMM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(decMM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "decMM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(decMM_dat, "./Processed_data/LCC_data/LCC_count/decMM.csv", row.names = F)

if (!(paste0("mcp_decMM.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=deciduousMM_int$lower_ends, LCC=decMM)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(deciduousMM, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_decMM.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_decMM.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("dec")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_decMM.png")
plot.decMM <- plot(fit_mcp)+
  ggtitle("dec")
hypothesis(fit_mcp, "cp_1=2587") #18

###wet_woodland SIG
wetwoodlandMM <- CallSites_MM(wet_woodland)
wetwoodlandMM_int <- make_interval_pol(wetwoodlandMM, 100)
wetwMM <- rowSums(wetwoodlandMM_int[3:ncol(wetwoodlandMM_int)], na.rm = TRUE)

wetwMM_dat =  wetwoodlandMM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetwMM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetwMM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(wetwMM_dat, "./Processed_data/LCC_data/LCC_count/wetwMM.csv", row.names = F)

if (!(paste0("mcp_wetwMM.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=wetwoodlandMM_int$lower_ends, LCC=wetwMM)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(wetwoodlandMM, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 100000, sample = "both", iter = 50000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetwMM.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetwMM.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("wetw")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_wetwMM.png")
plot.wetwMM <- plot(fit_mcp)+
  ggtitle("wetw")
hypothesis(fit_mcp, "cp_1=9161") #17

###wet_meadow SIG
wetmeadowMM <- CallSites_MM(wet_meadow)
wetmeadowMM_int <- make_interval_pol(wetmeadowMM, 100)
wetmMM <- rowSums(wetmeadowMM_int[3:ncol(wetmeadowMM_int)], na.rm = TRUE)

wetmMM_dat =  wetmeadowMM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetmMM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetmMM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(wetmMM_dat, "./Processed_data/LCC_data/LCC_count/wetmMM.csv", row.names = F)

if (!(paste0("mcp_wetmMM.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=wetmeadowMM_int$lower_ends, LCC=wetmMM)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(wetmeadowMM, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 100000, sample = "both", iter = 50000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetmMM.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetmMM.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("wetm")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_wetmMM.png")
plot.wetmMM <- plot(fit_mcp)+
  ggtitle("wetm")
hypothesis(fit_mcp, "cp_1=1448") #23
hypothesis(fit_mcp, "cp_2=9352") #99
#hypothesis(fit_mcp, "cp_3=9549") #40 # GRANT: DOESNT EXIST?

###pasture
pastureMM <- CallSites_MM(pasture)
pastureMM_int <- make_interval_pol(pastureMM, 100)
pasMM <- rowSums(pastureMM_int[3:ncol(pastureMM_int)], na.rm = TRUE)

pasMM_dat =  pastureMM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(pasMM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "pasMM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(pasMM_dat, "./Processed_data/LCC_data/LCC_count/pasMM.csv", row.names = F)

if (!(paste0("mcp_wetmMM.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=pastureMM_int$lower_ends, LCC=pasMM)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(pastureMM, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetmMM.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetmMM.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("pas")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_pasMM.png")
plot.pasMM <- plot(fit_mcp)+
  ggtitle("pas")
hypothesis(fit_mcp, "cp_1=3179") #7

###arable
arableMM <- CallSites_MM(arable)
arableMM_int <- make_interval_pol(arableMM, 100)
araMM <- rowSums(arableMM_int[3:ncol(arableMM_int)], na.rm = TRUE)

araMM_dat =  arableMM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(araMM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "araMM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(araMM_dat, "./Processed_data/LCC_data/LCC_count/araMM.csv", row.names = F)

if (!(paste0("mcp_araMM.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=arableMM_int$lower_ends, LCC=araMM)
  mcp_area <- mcp_area[28:nrow(mcp_area),]
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(arableMM, 100)
  count <- count[28:nrow(count),]
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_araMM.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_araMM.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("ara")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_araMM.png")
plot.araMM <- plot(fit_mcp)+
  ggtitle("ara")
hypothesis(fit_mcp, "cp_1=1937") #7

###heath
heathMM <- CallSites_MM(heath)
heathMM_int <- make_interval_pol(heathMM, 100)
EricaMM <- rowSums(heathMM_int[c(12:15)])
heaMM <- rowSums(heathMM_int[3:ncol(heathMM_int)], na.rm = TRUE)

heaMM_dat =  heathMM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(heaMM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "heaMM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(heaMM_dat, "./Processed_data/LCC_data/LCC_count/heaMM.csv", row.names = F)

if (!(paste0("mcp_heaMM.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=heathMM_int$lower_ends, LCC=heaMM)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(heathMM, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age, ~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_heaMM.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_heaMM.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("hea")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_heaMM.png")
plot.heaMM <- plot(fit_mcp)+
  ggtitle("hea")
hypothesis(fit_mcp, "cp_1=451") #66
hypothesis(fit_mcp, "cp_2=1624") #88
hypothesis(fit_mcp, "cp_3=7927") #43

ggpubr::ggarrange(plot.conMM, plot.decMM, plot.wetwMM, plot.wetmMM, plot.pasMM, plot.araMM, plot.heaMM + rremove("x.text"), 
          #labels = c("con", "dec", "wetw", "wetm", "pas", "ara", "hea", "SPD", "clim"),
          ncol = 3, nrow = 3)

Full_list_MM = list(conMM_dat, decMM_dat, wetwMM_dat, wetmMM_dat, pasMM_dat, araMM_dat, heaMM_dat)

#ecp
#ecp_area <- data.frame(age=coniferousMM_int$lower_ends, conMM, decMM, wetwMM, wetmMM, pasMM, araMM, heaMM)
#ecp_sqrt <- sqrt_sums(ecp_area)
#ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
#ecpMM <- ecp_sums(ecp_area, "ecp_area")
#plot_LCCecp(ecp_area, ecpMM, "MidMid_ecp")

#-SOUTHWEST ----------------------------------------------------------------------
# total nr sites: 11
# 20%: 3
###coniferous_woodland
coniferousSW <- CallSites_SW(coniferous_woodland)
coniferousSW_int <- make_interval_pol(coniferousSW, 100)
conSW <- rowSums(coniferousSW_int[3:ncol(coniferousSW_int)], na.rm = TRUE)

conSW_dat =  coniferousSW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(conSW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "conSW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(conSW_dat, "./Processed_data/LCC_data/LCC_count/conSW.csv", row.names = F)

if (!(paste0("mcp_conSW.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=coniferousSW_int$lower_ends, LCC=conSW)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(coniferousSW, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_conSW.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_conSW.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("con")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_conSW.png")
plot.conSW <- plot(fit_mcp)+
  ggtitle("con")
#hypothesis(fit_mcp, "cp_1=6056") # no cp

###deciduous_woodland
deciduousSW <- CallSites_SW(deciduous_woodland)
deciduousSW_int <- make_interval_pol(deciduousSW, 100)
decSW <- rowSums(deciduousSW_int[3:ncol(deciduousSW_int)], na.rm = TRUE)

decSW_dat =  deciduousSW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(decSW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "decSW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(decSW_dat, "./Processed_data/LCC_data/LCC_count/decSW.csv", row.names = F)

if (!(paste0("mcp_decSW.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=deciduousSW_int$lower_ends, LCC=decSW)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(deciduousSW, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_decSW.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_decSW.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("dec")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_decSW.png")
plot.decSW <- plot(fit_mcp)+
  ggtitle("dec")
#hypothesis(fit_mcp, "cp_1=5433") #1

###wet_woodland
wetwoodlandSW <- CallSites_SW(wet_woodland)
wetwoodlandSW_int <- make_interval_pol(wetwoodlandSW, 100)
wetwSW <- rowSums(wetwoodlandSW_int[3:ncol(wetwoodlandSW_int)], na.rm = TRUE)

wetwSW_dat =  wetwoodlandSW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetwSW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetwSW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(wetwSW_dat, "./Processed_data/LCC_data/LCC_count/wetwSW.csv", row.names = F)

if (!(paste0("mcp_wetwSW.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=wetwoodlandSW_int$lower_ends, LCC=wetwSW)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(wetwoodlandSW, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetwSW.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetwSW.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("wetw")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_wetwSW.png")
plot.wetwSW <- plot(fit_mcp)+
  ggtitle("wetw")
hypothesis(fit_mcp, "cp_1=5548") #6
hypothesis(fit_mcp, "cp_2=9135") #37

###wet_meadow
wetmeadowSW <- CallSites_SW(wet_meadow)
wetmeadowSW_int <- make_interval_pol(wetmeadowSW, 100)
wetmSW <- rowSums(wetmeadowSW_int[3:ncol(wetmeadowSW_int)], na.rm = TRUE)

wetmSW_dat =  wetmeadowSW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetmSW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetmSW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(wetmSW_dat, "./Processed_data/LCC_data/LCC_count/wetmSW.csv", row.names = F)

if (!(paste0("mcp_wetmSW.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=wetmeadowSW_int$lower_ends, LCC=wetmSW)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(wetmeadowSW, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetmSW.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetmSW.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("wetm")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_wetmSW.png")
plot.wetmSW <- plot(fit_mcp)+
  ggtitle("wetm")
hypothesis(fit_mcp, "cp_1=4346") #4

###pasture
pastureSW <- CallSites_SW(pasture)
pastureSW_int <- make_interval_pol(pastureSW, 100)
pasSW <- rowSums(pastureSW_int[3:ncol(pastureSW_int)], na.rm = TRUE)

pasSW_dat =  pastureSW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(pasSW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "pasSW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(pasSW_dat, "./Processed_data/LCC_data/LCC_count/pasSW.csv", row.names = F)

if (!(paste0("mcp_pasSW.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=pastureSW_int$lower_ends, LCC=pasSW)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(pastureSW, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_pasSW.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_pasSW.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("pas")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_pasSW.png")
plot.pasSW <- plot(fit_mcp)+
  ggtitle("pas")
hypothesis(fit_mcp, "cp_1=4188") #6

###arable
arableSW <- CallSites_SW(arable)
arableSW_int <- make_interval_pol(arableSW, 100)
araSW <- rowSums(arableSW_int[3:ncol(arableSW_int)], na.rm = TRUE)

araSW_dat =  arableSW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(araSW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "araSW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(araSW_dat, "./Processed_data/LCC_data/LCC_count/araSW.csv", row.names = F)

if (!(paste0("mcp_araSW.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=arableSW_int$lower_ends, LCC=araSW)
  mcp_area <- mcp_area[28:nrow(mcp_area),]
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(arableSW, 100)
  count <- count[28:nrow(count),]
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_araSW.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_araSW.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("ara")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_araSW.png")
plot.araSW <- plot(fit_mcp)+
  ggtitle("ara")
hypothesis(fit_mcp, "cp_1=1150") #75

###heath
heathSW <- CallSites_SW(heath)
heathSW_int <- make_interval_pol(heathSW, 100)
EricaSW <- rowSums(heathSW_int[c(12:15)])
heaSW <- rowSums(heathSW_int[3:ncol(heathSW_int)], na.rm = TRUE)

heaSW_dat =  heathSW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(heaSW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "heaSW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(heaSW_dat, "./Processed_data/LCC_data/LCC_count/heaSW.csv", row.names = F)

if (!(paste0("mcp_heaSW.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=heathSW_int$lower_ends, LCC=heaSW)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(heathSW, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_heaSW.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_heaSW.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("hea")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_heaSW.png")
plot.heaSW <- plot(fit_mcp)+
  ggtitle("hea")
hypothesis(fit_mcp, "cp_1=874") #88

ggpubr::ggarrange(plot.conSW, plot.decSW, plot.wetwSW, plot.wetmSW, plot.pasSW, plot.araSW, plot.heaSW + rremove("x.text"), 
          #labels = c("con", "dec", "wetw", "wetm", "pas", "ara", "hea", "SPD", "clim"),
          ncol = 3, nrow = 3)

Full_list_SW = list(conSW_dat, decSW_dat, wetwSW_dat, wetmSW_dat, pasSW_dat, araSW_dat, heaSW_dat)

#ecp
#ecp_area <- data.frame(age=coniferousSW_int$lower_ends, conSW, decSW, wetwSW, wetmSW, pasSW, araSW, heaSW)
#ecp_sqrt <- sqrt_sums(ecp_area)
#ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
#ecpSW <- ecp_sums(ecp_area, "ecp_area")
#plot_LCCecp(ecp_area, ecpSW, "SouthWest_ecp")

#-SOUTHMID --------------------------------------------------------------------
# total nr sites: 4
# 20%: 1 (at least 2)
###coniferous_woodland
coniferousSM <- CallSites_SM(coniferous_woodland)
coniferousSM_int <- make_interval_pol(coniferousSM, 100)
conSM <- rowSums(coniferousSM_int[3:ncol(coniferousSM_int)], na.rm = TRUE)

conSM_dat =  coniferousSM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(conSM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "conSM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(conSM_dat, "./Processed_data/LCC_data/LCC_count/conSM.csv", row.names = F)

if (!(paste0("mcp_conSM.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=coniferousSM_int$lower_ends, LCC=conSM)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(coniferousSM, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_conSM.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_conSM.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("con")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_conSM.png")
plot.conSM <- plot(fit_mcp)+
  ggtitle("con")
hypothesis(fit_mcp, "cp_1=1124") #24
hypothesis(fit_mcp, "cp_2=8174") #14 manually adapted

###deciduous_woodland
deciduousSM <- CallSites_SM(deciduous_woodland)
deciduousSM_int <- make_interval_pol(deciduousSM, 100)
BetulaSM <- rowSums(deciduousSM_int[7:13])
decSM <- rowSums(deciduousSM_int[3:ncol(deciduousSM_int)], na.rm = TRUE)

decSM_dat =  deciduousSM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(decSM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "decSM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(decSM_dat, "./Processed_data/LCC_data/LCC_count/decSM.csv", row.names = F)

if (!(paste0("mcp_decSM.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=deciduousSM_int$lower_ends, LCC=decSM)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(deciduousSM, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_decSM.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_decSM.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("dec")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_decSM.png")
plot.decSM <- plot(fit_mcp)+
  ggtitle("dec")
hypothesis(fit_mcp, "cp_1=900") #29
hypothesis(fit_mcp, "cp_2=1299") #179

###wet_woodland
wetwoodlandSM <- CallSites_SM(wet_woodland)
wetwoodlandSM_int <- make_interval_pol(wetwoodlandSM, 100)
AlnusSalixSM <- rowSums(wetwoodlandSM_int[c(3,4,5,6,23)])
wetwSM <- rowSums(wetwoodlandSM_int[3:ncol(wetwoodlandSM_int)], na.rm = TRUE)

wetwSM_dat =  wetwoodlandSM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetwSM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetwSM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(wetwSM_dat, "./Processed_data/LCC_data/LCC_count/wetwSM.csv", row.names = F)

if (!(paste0("mcp_wetwSM.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=wetwoodlandSM_int$lower_ends, LCC=wetwSM)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(wetwoodlandSM, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetwSM.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetwSM.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("wetw")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_wetwSM.png")
plot.wetwSM <- plot(fit_mcp)+
  ggtitle("wetw")
hypothesis(fit_mcp, "cp_1=4752") #18
hypothesis(fit_mcp, "cp_2=5228") #72

###wet_meadow
wetmeadowSM <- CallSites_SM(wet_meadow)
wetmeadowSM_int <- make_interval_pol(wetmeadowSM, 100)
wetmSM <- rowSums(wetmeadowSM_int[3:ncol(wetmeadowSM_int)], na.rm = TRUE)

wetmSM_dat =  wetmeadowSM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetmSM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetmSM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(wetmSM_dat, "./Processed_data/LCC_data/LCC_count/wetmSM.csv", row.names = F)

if (!(paste0("mcp_wetmSM.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=wetmeadowSM_int$lower_ends, LCC=wetmSM)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(wetmeadowSM, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age, ~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetmSM.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_wetmSM.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("wetm")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_wetmSM.png")
plot.wetmSM <- plot(fit_mcp)+
  ggtitle("wetm")
hypothesis(fit_mcp, "cp_1=2414") #9
hypothesis(fit_mcp, "cp_2=7300") #46
hypothesis(fit_mcp, "cp_3=7454") #65

###pasture
pastureSM <- CallSites_SM(pasture)
pastureSM_int <- make_interval_pol(pastureSM, 100)
#PoaceaeSM
pasSM <- rowSums(pastureSM_int[3:ncol(pastureSM_int)], na.rm = TRUE)

pasSM_dat =  pastureSM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(pasSM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "pasSM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(pasSM_dat, "./Processed_data/LCC_data/LCC_count/pasSM.csv", row.names = F)

if (!(paste0("mcp_pasSM.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=pastureSM_int$lower_ends, LCC=pasSM)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(pastureSM, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 50000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_pasSM.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_pasSM.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("pas")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_pasSM.png")
plot.pasSM <- plot(fit_mcp)+
  ggtitle("pas")
hypothesis(fit_mcp, "cp_1=1299") #25
hypothesis(fit_mcp, "cp_2=3650") #2

###arable
arableSM <- CallSites_SM(arable)
arableSM_int <- make_interval_pol(arableSM, 100)
araSM <- rowSums(arableSM_int[3:ncol(arableSM_int)], na.rm = TRUE)

araSM_dat =  arableSM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(araSM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "araSM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(araSM_dat, "./Processed_data/LCC_data/LCC_count/araSM.csv", row.names = F)

if (!(paste0("mcp_araSM.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=arableSM_int$lower_ends, LCC=araSM)
  mcp_area <- mcp_area[28:nrow(mcp_area),]
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(arableSM, 100)
  count <- count[28:nrow(count),]
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_araSM.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_araSM.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("ara")
ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_araSM.png")
plot.araSM <- plot(fit_mcp)+
  ggtitle("ara")
hypothesis(fit_mcp, "cp_1=1300") #39

###heath
heathSM <- CallSites_SM(heath)
heathSM_int <- make_interval_pol(heathSM, 100)
EricaSM <- rowSums(heathSM_int[c(12:15)])
heaSM <- rowSums(heathSM_int[3:ncol(heathSM_int)], na.rm = TRUE)

heaSM_dat =  heathSM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(heaSM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "heaSM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(heaSM_dat, "./Processed_data/LCC_data/LCC_count/heaSM.csv", row.names = F)

if (!(paste0("mcp_heaSM.Rda") %in% list.files("./Processed_data/LCC_data/LCC_count/mcp_models/"))) {
  mcp_area <- data.frame(age=heathSM_int$lower_ends, LCC=heaSM)
  plot(mcp_area)
  # count sites
  count <- countSitesPerInterval(heathSM, 100)
  mcp_area <- merge(mcp_area, count, by="age", all.x=TRUE)
  mcp_area <- subset(mcp_area, NRsites >=3)
  #plot(mcp_area$age, mcp_area$LCC)
  model = list(LCC~1+age, ~1+age)
  fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
  saveRDS(fit_mcp, "./Processed_data/LCC_data/LCC_count/mcp_models/mcp_heaSM.Rda")
} else {
  fit_mcp = readRDS("./Processed_data/LCC_data/LCC_count/mcp_models/mcp_heaSM.Rda")
}
summary(fit_mcp)
plot(fit_mcp)+
  ggtitle("hea")
ggplot2::ggsave("./Results/Plots/LCC_count/mcp_plots/mcp_heaSM.png")
plot.heaSM <- plot(fit_mcp)+
  ggtitle("hea")
hypothesis(fit_mcp, "cp_1=1100") #47

#ecp
#ecp_area <- data.frame(age=coniferousSM_int$lower_ends, conSM, decSM, wetwSM, wetmSM, pasSM, araSM, heaSM)
#ecp_sqrt <- sqrt_sums(ecp_area)
#ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
#ecpSM <- ecp_sums(ecp_area, "ecp_area")
#plot_LCCecp(ecp_area, ecpSW, "SouthMid_ecp")

ggpubr::ggarrange(plot.conSM, plot.decSM, plot.wetwSM, plot.wetmSM, plot.pasSM, plot.araSM, plot.heaSM + rremove("x.text"), 
          #labels = c("con", "dec", "wetw", "wetm", "pas", "ara", "hea", "SPD", "clim"),
          ncol = 3, nrow = 3)

Full_list_SM = list(conSM_dat, decSM_dat, wetwSM_dat, wetmSM_dat, pasSM_dat, araSM_dat, heaSM_dat)


#-CREATE FULL DATASETS --------

merged_data_N = Full_list_N %>% reduce(full_join, by='calBP')
write.csv(merged_data_N, "./Processed_data/LCC_data/LCC_count/merged_data_N.csv", row.names = F)
merged_data_SE = Full_list_SE %>% reduce(full_join, by='calBP')
write.csv(merged_data_SE, "./Processed_data/LCC_data/LCC_count/merged_data_SE.csv", row.names = F)
merged_data_MM = Full_list_MM %>% reduce(full_join, by='calBP')
write.csv(merged_data_MM,"./Processed_data/LCC_data/LCC_count/merged_data_MM.csv", row.names = F)
merged_data_MW = Full_list_MW %>% reduce(full_join, by='calBP')
write.csv(merged_data_MW, "./Processed_data/LCC_data/LCC_count/merged_data_MW.csv", row.names = F)
merged_data_SW = Full_list_SW %>% reduce(full_join, by='calBP')
write.csv(merged_data_SW, "./Processed_data/LCC_data/LCC_count/merged_data_SW.csv", row.names = F)
merged_data_SM = Full_list_SM %>% reduce(full_join, by='calBP')
write.csv(merged_data_SM, "./Processed_data/LCC_data/LCC_count/merged_data_SM.csv", row.names = F)