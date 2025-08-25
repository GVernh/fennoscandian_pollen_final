#- LIBRARIES ----
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


coniferous_woodland <- read.csv("./Processed_data/LCC_data/LCC_abun/coniferous_woodland_abun.csv")
deciduous_woodland <- read.csv("./Processed_data/LCC_data/LCC_abun/deciduous_woodland_abun.csv")
wet_woodland <- read.csv("./Processed_data/LCC_data/LCC_abun/wet_woodland_abun.csv")
wet_meadow <- read.csv("./Processed_data/LCC_data/LCC_abun/wet_meadow_abun.csv")
pasture <- read.csv("./Processed_data/LCC_data/LCC_abun/pasture_abun.csv") 
arable <- read.csv("./Processed_data/LCC_data/LCC_abun/arable_abun.csv") 
heath <- read.csv("./Processed_data/LCC_data/LCC_abun/heath_abun.csv")

source("./Functions/CallSite_functions.R")
source("./Functions/Make_interval_pol.R") # This summarises time and dataset ID's to time points

# NORTH ----
# ConN
coniferousN <- CallSites_N(coniferous_woodland)
coniferousN_int <- make_interval_pol(coniferousN, 100)

conN_dat =  coniferousN_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(conN = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "conN")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(conN_dat, "./Processed_data/LCC_data/LCC_abun/conN.csv", row.names = F)

# DecN
deciduousN <- CallSites_N(deciduous_woodland)
deciduousN_int <- make_interval_pol(deciduousN, 100)

decN_dat =  deciduousN_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(decN = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "decN")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(decN_dat, "./Processed_data/LCC_data/LCC_abun/decN.csv", row.names = F)

# wetwN

wetwoodlandN <- CallSites_N(wet_woodland)
wetwoodlandN_int <- make_interval_pol(wetwoodlandN, 100)

wetwN_dat =  wetwoodlandN_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetwN = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetwN")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(wetwN_dat, "./Processed_data/LCC_data/LCC_abun/wetwN.csv", row.names = F)

#wetmN
wetmeadowN <- CallSites_N(wet_meadow)
wetmeadowN_int <- make_interval_pol(wetmeadowN, 100)

wetmN_dat =  wetmeadowN_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetmN = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetmN")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(wetmN_dat, "./Processed_data/LCC_data/LCC_abun/wetmN.csv", row.names = F)

# PasN
pastureN <- CallSites_N(pasture)
pastureN_int <- make_interval_pol(pastureN, 100)

pasN_dat =  pastureN_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(pasN = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "pasN")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(pasN_dat, "./Processed_data/LCC_data/LCC_abun/pasN.csv", row.names = F)


# AraN
arableN <- CallSites_N(arable)
arableN_int <- make_interval_pol(arableN, 100)

araN_dat =  arableN_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(araN = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "araN")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(araN_dat, "./Processed_data/LCC_data/LCC_abun/araN.csv", row.names = F)

# heaN
heathN <- CallSites_N(heath)
heathN_int <- make_interval_pol(heathN, 100)

heaN_dat =  heathN_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(heaN = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "heaN")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(heaN_dat, "./Processed_data/LCC_data/LCC_abun/heaN.csv", row.names = F)

Full_list_N = list(conN_dat, decN_dat, wetwN_dat, wetmN_dat, pasN_dat, araN_dat, heaN_dat)

# SOUTHEAST ----

# COnSE
coniferousSE <- CallSites_SE(coniferous_woodland)
coniferousSE_int <- make_interval_pol(coniferousSE, 100)

conSE_dat =  coniferousSE_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(conSE = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "conSE")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>400 & calBP<9800)
write.csv(conSE_dat, "./Processed_data/LCC_data/LCC_abun/conSE.csv", row.names = F)

# DecSE
deciduousSE <- CallSites_SE(deciduous_woodland)
deciduousSE_int <- make_interval_pol(deciduousSE, 100)

decSE_dat =  deciduousSE_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(decSE = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "decSE")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>400 & calBP<9800)
write.csv(decSE_dat, "./Processed_data/LCC_data/LCC_abun/decSE.csv", row.names = F)

# WetwSE
wetwoodlandSE <- CallSites_SE(wet_woodland)
wetwoodlandSE_int <- make_interval_pol(wetwoodlandSE, 100)

wetwSE_dat =  wetwoodlandSE_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetwSE = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetwSE")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>400 & calBP<9800)
write.csv(wetwSE_dat, "./Processed_data/LCC_data/LCC_abun/wetwSE.csv", row.names = F)

# wetmSE
wetmeadowSE <- CallSites_SE(wet_meadow)
wetmeadowSE_int <- make_interval_pol(wetmeadowSE, 100)

wetmSE_dat =  wetmeadowSE_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetmSE = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetmSE")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>400 & calBP<9800)
write.csv(wetmSE_dat, "./Processed_data/LCC_data/LCC_abun/wetmSE.csv", row.names = F)

# PasSE
pastureSE <- CallSites_SE(pasture)
pastureSE_int <- make_interval_pol(pastureSE, 100)

pasSE_dat =  pastureSE_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(pasSE = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "pasSE")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>400 & calBP<9800)
write.csv(pasSE_dat, "./Processed_data/LCC_data/LCC_abun/pasSE.csv", row.names = F)


# AraSE

arableSE <- CallSites_SE(arable)
arableSE_int <- make_interval_pol(arableSE, 100)

araSE_dat =  arableSE_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(araSE = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "araSE")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>400 & calBP<9800)
write.csv(araSE_dat, "./Processed_data/LCC_data/LCC_abun/araSE.csv", row.names = F)

# HeaSE

heathSE <- CallSites_SE(heath)
heathSE_int <- make_interval_pol(heathSE, 100)

heaSE_dat =  heathSE_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(heaSE = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "heaSE")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>400 & calBP<9800)
write.csv(heaSE_dat, "./Processed_data/LCC_data/LCC_abun/heaSE.csv", row.names = F)

Full_list_SE = list(conSE_dat, decSE_dat, wetwSE_dat, wetmSE_dat, pasSE_dat, araSE_dat, heaSE_dat)

# MIDWEST ----

# ConMW
coniferousMW <- CallSites_MW(coniferous_woodland)
coniferousMW_int <- make_interval_pol(coniferousMW, 100)

conMW_dat =  coniferousMW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(conMW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "conMW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(conMW_dat, "./Processed_data/LCC_data/LCC_abun/conMW.csv", row.names = F)

# DecMW
deciduousMW <- CallSites_MW(deciduous_woodland)
deciduousMW_int <- make_interval_pol(deciduousMW, 100)

decMW_dat =  deciduousMW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(decMW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "decMW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(decMW_dat, "./Processed_data/LCC_data/LCC_abun/decMW.csv", row.names = F)

# WetwMW
wetwoodlandMW <- CallSites_MW(wet_woodland)
wetwoodlandMW_int <- make_interval_pol(wetwoodlandMW, 100)

wetwMW_dat =  wetwoodlandMW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetwMW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetwMW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(wetwMW_dat, "./Processed_data/LCC_data/LCC_abun/wetwMW.csv", row.names = F)

#WetmMW
wetmeadowMW <- CallSites_MW(wet_meadow)
wetmeadowMW_int <- make_interval_pol(wetmeadowMW, 100)

wetmMW_dat =  wetmeadowMW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetmMW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetmMW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(wetmMW_dat, "./Processed_data/LCC_data/LCC_abun/wetmMW.csv", row.names = F)

# PasMW
pastureMW <- CallSites_MW(pasture)
pastureMW_int <- make_interval_pol(pastureMW, 100)

pasMW_dat =  pastureMW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(pasMW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "pasMW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(pasMW_dat, "./Processed_data/LCC_data/LCC_abun/pasMW.csv", row.names = F)

#AraMW
arableMW <- CallSites_MW(arable)
arableMW_int <- make_interval_pol(arableMW, 100)

araMW_dat =  arableMW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(araMW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "araMW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(araMW_dat, "./Processed_data/LCC_data/LCC_abun/araMW.csv", row.names = F)

#HeaMW
heathMW <- CallSites_MW(heath)
heathMW_int <- make_interval_pol(heathMW, 100)

heaMW_dat =  heathMW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(heaMW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "heaMW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(heaMW_dat, "./Processed_data/LCC_data/LCC_abun/heaMW.csv", row.names = F)

Full_list_MW = list(conMW_dat, decMW_dat, wetwMW_dat, wetmMW_dat, pasMW_dat, araMW_dat, heaMW_dat)

# CENTRAL ----

# ConMM
coniferousMM <- CallSites_MM(coniferous_woodland)
coniferousMM_int <- make_interval_pol(coniferousMM, 100)

conMM_dat =  coniferousMM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(conMM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "conMM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(conMM_dat, "./Processed_data/LCC_data/LCC_abun/conMM.csv", row.names = F)

# DecMM
deciduousMM <- CallSites_MM(deciduous_woodland)
deciduousMM_int <- make_interval_pol(deciduousMM, 100)

decMM_dat =  deciduousMM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(decMM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "decMM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(decMM_dat, "./Processed_data/LCC_data/LCC_abun/decMM.csv", row.names = F)

# WetwMM
wetwoodlandMM <- CallSites_MM(wet_woodland)
wetwoodlandMM_int <- make_interval_pol(wetwoodlandMM, 100)

wetwMM_dat =  wetwoodlandMM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetwMM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetwMM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(wetwMM_dat, "./Processed_data/LCC_data/LCC_abun/wetwMM.csv", row.names = F)

# WetmMM
wetmeadowMM <- CallSites_MM(wet_meadow)
wetmeadowMM_int <- make_interval_pol(wetmeadowMM, 100)

wetmMM_dat =  wetmeadowMM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetmMM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetmMM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(wetmMM_dat, "./Processed_data/LCC_data/LCC_abun/wetmMM.csv", row.names = F)

# PasMM
pastureMM <- CallSites_MM(pasture)
pastureMM_int <- make_interval_pol(pastureMM, 100)

pasMM_dat =  pastureMM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(pasMM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "pasMM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(pasMM_dat, "./Processed_data/LCC_data/LCC_abun/pasMM.csv", row.names = F)

# AraMM
arableMM <- CallSites_MM(arable)
arableMM_int <- make_interval_pol(arableMM, 100)

araMM_dat =  arableMM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(araMM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "araMM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(araMM_dat, "./Processed_data/LCC_data/LCC_abun/araMM.csv", row.names = F)

# heaMM

heathMM <- CallSites_MM(heath)
heathMM_int <- make_interval_pol(heathMM, 100)

heaMM_dat =  heathMM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(heaMM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "heaMM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(heaMM_dat, "./Processed_data/LCC_data/LCC_abun/heaMM.csv", row.names = F)


Full_list_MM = list(conMM_dat, decMM_dat, wetwMM_dat, wetmMM_dat, pasMM_dat, araMM_dat, heaMM_dat)

# SOUTHWEST ----
# conSW
coniferousSW <- CallSites_SW(coniferous_woodland)
coniferousSW_int <- make_interval_pol(coniferousSW, 100)

conSW_dat =  coniferousSW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(conSW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "conSW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(conSW_dat, "./Processed_data/LCC_data/LCC_abun/conSW.csv", row.names = F)

#DecSW
deciduousSW <- CallSites_SW(deciduous_woodland)
deciduousSW_int <- make_interval_pol(deciduousSW, 100)

decSW_dat =  deciduousSW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(decSW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "decSW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(decSW_dat, "./Processed_data/LCC_data/LCC_abun/decSW.csv", row.names = F)

#WetwSW
wetwoodlandSW <- CallSites_SW(wet_woodland)
wetwoodlandSW_int <- make_interval_pol(wetwoodlandSW, 100)

wetwSW_dat =  wetwoodlandSW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetwSW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetwSW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(wetwSW_dat, "./Processed_data/LCC_data/LCC_abun/wetwSW.csv", row.names = F)

#WetmSW
wetmeadowSW <- CallSites_SW(wet_meadow)
wetmeadowSW_int <- make_interval_pol(wetmeadowSW, 100)

wetmSW_dat =  wetmeadowSW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetmSW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetmSW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(wetmSW_dat, "./Processed_data/LCC_data/LCC_abun/wetmSW.csv", row.names = F)

# PasSW
pastureSW <- CallSites_SW(pasture)
pastureSW_int <- make_interval_pol(pastureSW, 100)

pasSW_dat =  pastureSW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(pasSW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "pasSW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(pasSW_dat, "./Processed_data/LCC_data/LCC_abun/pasSW.csv", row.names = F)

#AraSW
arableSW <- CallSites_SW(arable)
arableSW_int <- make_interval_pol(arableSW, 100)

araSW_dat =  arableSW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(araSW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "araSW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(araSW_dat, "./Processed_data/LCC_data/LCC_abun/araSW.csv", row.names = F)


# HeaSW
heathSW <- CallSites_SW(heath)
heathSW_int <- make_interval_pol(heathSW, 100)

heaSW_dat =  heathSW_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(heaSW = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "heaSW")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>600 & calBP<9800)
write.csv(heaSW_dat, "./Processed_data/LCC_data/LCC_abun/heaSW.csv", row.names = F)

Full_list_SW = list(conSW_dat, decSW_dat, wetwSW_dat, wetmSW_dat, pasSW_dat, araSW_dat, heaSW_dat)

# Southmid ----
#ConSM
coniferousSM <- CallSites_SM(coniferous_woodland)
coniferousSM_int <- make_interval_pol(coniferousSM, 100)

conSM_dat =  coniferousSM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(conSM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "conSM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(conSM_dat, "./Processed_data/LCC_data/LCC_abun/conSM.csv", row.names = F)

# DecSM
deciduousSM <- CallSites_SM(deciduous_woodland)
deciduousSM_int <- make_interval_pol(deciduousSM, 100)
BetulaSM <- rowSums(deciduousSM_int[7:13])

decSM_dat =  deciduousSM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(decSM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "decSM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(decSM_dat, "./Processed_data/LCC_data/LCC_abun/decSM.csv", row.names = F)

# WetwSM
wetwoodlandSM <- CallSites_SM(wet_woodland)
wetwoodlandSM_int <- make_interval_pol(wetwoodlandSM, 100)
AlnusSalixSM <- rowSums(wetwoodlandSM_int[c(3,4,5,6,23)])

wetwSM_dat =  wetwoodlandSM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetwSM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetwSM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(wetwSM_dat, "./Processed_data/LCC_data/LCC_abun/wetwSM.csv", row.names = F)

# WetmSM
wetmeadowSM <- CallSites_SM(wet_meadow)
wetmeadowSM_int <- make_interval_pol(wetmeadowSM, 100)

wetmSM_dat =  wetmeadowSM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(wetmSM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "wetmSM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(wetmSM_dat, "./Processed_data/LCC_data/LCC_abun/wetmSM.csv", row.names = F)

# PasSM
pastureSM <- CallSites_SM(pasture)
pastureSM_int <- make_interval_pol(pastureSM, 100)

pasSM_dat =  pastureSM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(pasSM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "pasSM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(pasSM_dat, "./Processed_data/LCC_data/LCC_abun/pasSM.csv", row.names = F)

# AraSM
arableSM <- CallSites_SM(arable)
arableSM_int <- make_interval_pol(arableSM, 100)

araSM_dat =  arableSM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(araSM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "araSM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(araSM_dat, "./Processed_data/LCC_data/LCC_abun/araSM.csv", row.names = F)

# HeaSM
heathSM <- CallSites_SM(heath)
heathSM_int <- make_interval_pol(heathSM, 100)

heaSM_dat =  heathSM_int %>%
  dplyr::select(- "meantimes") %>%
  dplyr::mutate(heaSM = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::select(c("lower_ends", "heaSM")) %>%
  dplyr::rename(calBP = lower_ends) %>%
  subset(., calBP>1300 & calBP<9800)
write.csv(heaSM_dat, "./Processed_data/LCC_data/LCC_abun/heaSM.csv", row.names = F)

Full_list_SM = list(conSM_dat, decSM_dat, wetwSM_dat, wetmSM_dat, pasSM_dat, araSM_dat, heaSM_dat)


# Merge & write data ----
merged_data_N = Full_list_N %>% reduce(full_join, by='calBP')
merged_data_SE = Full_list_SE %>% reduce(full_join, by='calBP')
merged_data_MM = Full_list_MM %>% reduce(full_join, by='calBP')
merged_data_MW = Full_list_MW %>% reduce(full_join, by='calBP')
merged_data_SW = Full_list_SW %>% reduce(full_join, by='calBP')
merged_data_SM = Full_list_SM %>% reduce(full_join, by='calBP')

write.csv(merged_data_N, "./Processed_data/LCC_data/LCC_abun/merged_data_N.csv", row.names = F)
write.csv(merged_data_SE, "./Processed_data/LCC_data/LCC_abun/merged_data_SE.csv", row.names = F)
write.csv(merged_data_MM,"./Processed_data/LCC_data/LCC_abun/merged_data_MM.csv", row.names = F)
write.csv(merged_data_MW, "./Processed_data/LCC_data/LCC_abun/merged_data_MW.csv", row.names = F)
write.csv(merged_data_SW, "./Processed_data/LCC_data/LCC_abun/merged_data_SW.csv", row.names = F)
write.csv(merged_data_SM, "./Processed_data/LCC_data/LCC_abun/merged_data_SM.csv", row.names = F)