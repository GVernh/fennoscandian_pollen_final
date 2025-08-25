### smoothing + model selection ###

libs <- c("ggplot2","tidyverse", "lubridate", "fpp2", "smooth", "zoo", "TTR", "vars")

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

#library(RRatepol)

dir.create(file.path("./Processed_data/", "Full_datasets"), showWarnings = FALSE)
#smoothing HOW TO MAKE MODEL SELECTION? median value = 7

# MODEL SELECTION - mean value = 4
# model selection according to the first value after the comma? --> 3
# select according to the full value. --> 1
# selecting according to the absolute lowest. --> 5

# TO DO (GRANT): Run smoothing on all data, write data to directory.
### NORTH

########
merged_data_N <-read.csv("./Processed_data/LCC_data/LCC_count/merged_data_N.csv")
spdN <- read.csv("./Processed_data/SPD_data/spdN.csv")
paleoviewN <- read.csv("./Processed_data/Climate/paleoviewN.csv") %>%
  dplyr::rename(calBP = Year..BP.) %>%
  dplyr::select(calBP, Area.Mean)

alldataN <- merge(merged_data_N, spdN, by="calBP", all.x=TRUE) %>%
  merge(paleoviewN, by="calBP", all.x=TRUE) %>%
  dplyr::rename(clim = Area.Mean) %>%
  dplyr::rename(SPD = SPD_med) %>%
  dplyr::rename(yearsBP = calBP) %>%
  .[order(.$yearsBP, decreasing = TRUE), ] # Data by years in descending order

#model testing smoothing
smooth::sma(alldataN$conN, ic="BIC", h=0, interval="none") #6
smooth::sma(alldataN$decN, ic="BIC", h=0, interval="none") #8
smooth::sma(alldataN$wetwN, ic="BIC", h=0, interval="none") #3
smooth::sma(alldataN$wetmN, ic="BIC", h=0, interval="none") #8
smooth::sma(alldataN$pasN, ic="BIC", h=0, interval="none") #7
smooth::sma(alldataN$araN, ic="BIC", h=0, interval="none") #2
smooth::sma(alldataN$heaN, ic="BIC", h=0, interval="none") #7

#smoothing
alldataN$conNs <- TTR::SMA(alldataN$conN, n=6)
alldataN$decNs <- TTR::SMA(alldataN$decN, n=8)
alldataN$wetwNs <- TTR::SMA(alldataN$wetwN, n=3)
alldataN$wetmNs <- TTR::SMA(alldataN$wetmN, n=8)
alldataN$pasNs <- TTR::SMA(alldataN$pasN, n=7)
alldataN$araNs <- TTR::SMA(alldataN$araN, n=2)
alldataN$heaNs <- TTR::SMA(alldataN$heaN, n=7)
alldataNs <- alldataN[8:nrow(alldataN),] # GRANT: Check this, works fine but im not sure why data is being removed
write.csv(alldataNs, file = "./Processed_data/Full_datasets/alldataNs_count.csv", row.names = FALSE)

#model testing VAR all dates
vars::VARselect(alldataNs$conNs, lag.max=7, type="none") #8 7
vars::VARselect(alldataNs$decNs, lag.max=7, type="none") #9 2
vars::VARselect(alldataNs$wetwNs, lag.max=7, type="none") #6 1
vars::VARselect(alldataNs$wetmNs, lag.max=7, type="none") #10 2
vars::VARselect(alldataNs$pasNs, lag.max=7, type="none") #1 1
vars::VARselect(alldataNs$araNs, lag.max=7, type="none") #7 5
vars::VARselect(alldataNs$heaNs, lag.max=7, type="none") #8 1

# GRANT: These variables aren't created until the cross validation script is run.
# #model testing VAR before farming
# VARselect(alldataNb$conNs, lag.max=7, type="none") #7 7
# VARselect(alldataNb$decNs, lag.max=7, type="none") #9 2
# VARselect(alldataNb$wetwNs, lag.max=7, type="none") #6 1
# VARselect(alldataNb$wetmNs, lag.max=7, type="none") #10 2
# VARselect(alldataNb$pasNs, lag.max=7, type="none") #3 1
# VARselect(alldataNb$araNs, lag.max=7, type="none") #7 1
# VARselect(alldataNb$heaNs, lag.max=7, type="none") #8 1
# 
# #model testing VAR after farming
# VARselect(alldataNa$conNs, lag.max=7, type="none") #8
# VARselect(alldataNa$decNs, lag.max=7, type="none") #9
# VARselect(alldataNa$wetwNs, lag.max=7, type="none") #6
# VARselect(alldataNa$wetmNs, lag.max=7, type="none") #10
# VARselect(alldataNa$pasNs, lag.max=7, type="none") #1
# VARselect(alldataNa$araNs, lag.max=7, type="none") #7
# VARselect(alldataNa$heaNs, lag.max=7, type="none") #8

### SOUTHEAST
merged_data_SE <-read.csv("./Processed_data/LCC_data/LCC_count/merged_data_SE.csv")
spdSE <- read.csv("./Processed_data/SPD_data/spdSE.csv")
paleoviewSE <- read.csv("./Processed_data/Climate/paleoviewSE.csv") %>%
  dplyr::rename(calBP = Year..BP.) %>%
  dplyr::select(calBP, Area.Mean)

alldataSE <- merge(merged_data_SE, spdSE, by="calBP", all.x=TRUE) %>%
  merge(paleoviewSE, by="calBP", all.x=TRUE) %>%
  dplyr::rename(clim = Area.Mean) %>%
  dplyr::rename(SPD = SPD_med) %>%
  dplyr::rename(yearsBP = calBP)%>%
  .[order(.$yearsBP, decreasing = TRUE), ]

smooth::sma(alldataSE$conSE, ic="BIC", h=0, interval="none") #4
smooth::sma(alldataSE$decSE, ic="BIC", h=0, interval="none") #4
smooth::sma(alldataSE$wetwSE, ic="BIC", h=0, interval="none") #12
smooth::sma(alldataSE$wetmSE, ic="BIC", h=0, interval="none") #9
smooth::sma(alldataSE$pasSE, ic="BIC", h=0, interval="none") #2
smooth::sma(alldataSE$araSE, ic="BIC", h=0, interval="none") #1
smooth::sma(alldataSE$heaSE, ic="BIC", h=0, interval="none") #8

alldataSE$conSEs <- TTR::SMA(alldataSE$conSE, n=4)
alldataSE$decSEs <- TTR::SMA(alldataSE$decSE, n=4)
alldataSE$wetwSEs <- TTR::SMA(alldataSE$wetwSE, n=8)
alldataSE$wetmSEs <- TTR::SMA(alldataSE$wetmSE, n=8)
alldataSE$pasSEs <- TTR::SMA(alldataSE$pasSE, n=2)
alldataSE$araSEs <- TTR::SMA(alldataSE$araSE, n=1)
alldataSE$heaSEs <- TTR::SMA(alldataSE$heaSE, n=8)
alldataSEs <- alldataSE[8:nrow(alldataSE),]
write.csv(alldataSEs, file = "./Processed_data/Full_datasets/alldataSEs_count.csv", row.names = FALSE)

vars::VARselect(alldataSEs$conSEs, lag.max=7, type="none") #10 2
vars::VARselect(alldataSEs$decSEs, lag.max=7, type="none") #10 1
vars::VARselect(alldataSEs$wetwSEs, lag.max=7, type="none") #10 1
vars::VARselect(alldataSEs$wetmSEs, lag.max=7, type="none") #2 2
vars::VARselect(alldataSEs$pasSEs, lag.max=7, type="none") #10 10
vars::VARselect(alldataSEs$araSEs, lag.max=7, type="none") #9 9
vars::VARselect(alldataSEs$heaSEs, lag.max=7, type="none") #8 2


### MIDWEST
merged_data_MW <-read.csv("./Processed_data/LCC_data/LCC_count/merged_data_MW.csv")
spdMW <- read.csv("./Processed_data/SPD_data/spdMW.csv")
paleoviewMW <- read.csv("./Processed_data/Climate/paleoviewMW.csv") %>%
  dplyr::rename(calBP = Year..BP.) %>%
  dplyr::select(calBP, Area.Mean)

alldataMW <- merge(merged_data_MW, spdMW, by="calBP", all.x=TRUE) %>%
  merge(paleoviewMW, by="calBP", all.x=TRUE) %>%
  dplyr::rename(clim = Area.Mean) %>%
  dplyr::rename(SPD = SPD_med) %>%
  dplyr::rename(yearsBP = calBP)%>%
  .[order(.$yearsBP, decreasing = TRUE), ]

smooth::sma(alldataMW$conMW, ic="BIC", h=0, interval="none") #101
smooth::sma(alldataMW$decMW, ic="BIC", h=0, interval="none") #7
smooth::sma(alldataMW$wetwMW, ic="BIC", h=0, interval="none") #5
smooth::sma(alldataMW$wetmMW, ic="BIC", h=0, interval="none") #8
smooth::sma(alldataMW$pasMW, ic="BIC", h=0, interval="none") #2
smooth::sma(alldataMW$araMW, ic="BIC", h=0, interval="none") #9
smooth::sma(alldataMW$heaMW, ic="BIC", h=0, interval="none") #8

alldataMW$conMWs <- TTR::SMA(alldataMW$conMW, n=8)
alldataMW$decMWs <- TTR::SMA(alldataMW$decMW, n=7)
alldataMW$wetwMWs <- TTR::SMA(alldataMW$wetwMW, n=5)
alldataMW$wetmMWs <- TTR::SMA(alldataMW$wetmMW, n=8)
alldataMW$pasMWs <- TTR::SMA(alldataMW$pasMW, n=2)
alldataMW$araMWs <- TTR::SMA(alldataMW$araMW, n=8)
alldataMW$heaMWs <- TTR::SMA(alldataMW$heaMW, n=8)
alldataMWs <- alldataMW[8:nrow(alldataMW),]
write.csv(alldataMWs, file = "./Processed_data/Full_datasets/alldataMWs_count.csv", row.names = FALSE)

vars::VARselect(alldataMWs$conMWs, lag.max=7, type="none") #9 9
vars::VARselect(alldataMWs$decMWs, lag.max=7, type="none") #8 8
vars::VARselect(alldataMWs$wetwMWs, lag.max=7, type="none") #8 1
vars::VARselect(alldataMWs$wetmMWs, lag.max=7, type="none") #9 3
vars::VARselect(alldataMWs$pasMWs, lag.max=7, type="none") #7 7
vars::VARselect(alldataMWs$araMWs, lag.max=7, type="none") #2 2
vars::VARselect(alldataMWs$heaMWs, lag.max=7, type="none") #9 3


### MIDMID

merged_data_MM <-read.csv("./Processed_data/LCC_data/LCC_count/merged_data_MM.csv")
spdMM <- read.csv("./Processed_data/SPD_data/spdMM.csv")
paleoviewMM <- read.csv("./Processed_data/Climate/paleoviewMM.csv") %>%
  dplyr::rename(calBP = Year..BP.) %>%
  dplyr::select(calBP, Area.Mean)

alldataMM <- merge(merged_data_MM, spdMM, by="calBP", all.x=TRUE) %>%
  merge(paleoviewMM, by="calBP", all.x=TRUE) %>%
  dplyr::rename(clim = Area.Mean) %>%
  dplyr::rename(SPD = SPD_med) %>%
  dplyr::rename(yearsBP = calBP)%>%
  .[order(.$yearsBP, decreasing = TRUE), ]

smooth::sma(alldataMM$conMM, ic="BIC", h=0, interval="none") #5
smooth::sma(alldataMM$decMM, ic="BIC", h=0, interval="none") #5
smooth::sma(alldataMM$wetwMM, ic="BIC", h=0, interval="none") #4
smooth::sma(alldataMM$wetmMM, ic="BIC", h=0, interval="none") #8
smooth::sma(alldataMM$pasMM, ic="BIC", h=0, interval="none") #3
smooth::sma(alldataMM$araMM, ic="BIC", h=0, interval="none") #5
smooth::sma(alldataMM$heaMM, ic="BIC", h=0, interval="none") #3

alldataMM$conMMs <- TTR::SMA(alldataMM$conMM, n=5)
alldataMM$decMMs <- TTR::SMA(alldataMM$decMM, n=5)
alldataMM$wetwMMs <- TTR::SMA(alldataMM$wetwMM, n=4)
alldataMM$wetmMMs <- TTR::SMA(alldataMM$wetmMM, n=8)
alldataMM$pasMMs <- TTR::SMA(alldataMM$pasMM, n=3)
alldataMM$araMMs <- TTR::SMA(alldataMM$araMM, n=5)
alldataMM$heaMMs <- TTR::SMA(alldataMM$heaMM, n=3)
alldataMMs <- alldataMM[8:nrow(alldataMM),]
write.csv(alldataMMs, file = "./Processed_data/Full_datasets/alldataMMs_count.csv", row.names = FALSE)

vars::VARselect(alldataMMs$conMMs, lag.max=7, type="none") #6 1
vars::VARselect(alldataMMs$decMMs, lag.max=7, type="none") #4 1
vars::VARselect(alldataMMs$wetwMMs, lag.max=7, type="none") #10 1
vars::VARselect(alldataMMs$wetmMMs, lag.max=7, type="none") #2 2
vars::VARselect(alldataMMs$pasMMs, lag.max=7, type="none") #9 1
vars::VARselect(alldataMMs$araMMs, lag.max=7, type="none") #6 6
vars::VARselect(alldataMMs$heaMMs, lag.max=7, type="none") #6 6


### SOUTHWEST
merged_data_SW <-read.csv("./Processed_data/LCC_data/LCC_count/merged_data_SW.csv")
spdSW <- read.csv("./Processed_data/SPD_data/spdSW.csv")
paleoviewSW <- read.csv("./Processed_data/Climate/paleoviewSW.csv") %>%
  dplyr::rename(calBP = Year..BP.) %>%
  dplyr::select(calBP, Area.Mean)

alldataSW <- merge(merged_data_SW, spdSW, by="calBP", all.x=TRUE) %>%
  merge(paleoviewSW, by="calBP", all.x=TRUE) %>%
  dplyr::rename(clim = Area.Mean) %>%
  dplyr::rename(SPD = SPD_med) %>%
  dplyr::rename(yearsBP = calBP)%>%
  .[order(.$yearsBP, decreasing = TRUE), ]

smooth::sma(alldataSW$conSW, ic="BIC", h=0, interval="none") #4
smooth::sma(alldataSW$decSW, ic="BIC", h=0, interval="none") #2
smooth::sma(alldataSW$wetwSW, ic="BIC", h=0, interval="none") #5
smooth::sma(alldataSW$wetmSW, ic="BIC", h=0, interval="none") #3
smooth::sma(alldataSW$pasSW, ic="BIC", h=0, interval="none") #2
smooth::sma(alldataSW$araSW, ic="BIC", h=0, interval="none") #2
smooth::sma(alldataSW$heaSW, ic="BIC", h=0, interval="none") #2

alldataSW$conSWs <- TTR::SMA(alldataSW$conSW, n=4)
alldataSW$decSWs <- TTR::SMA(alldataSW$decSW, n=2)
alldataSW$wetwSWs <- TTR::SMA(alldataSW$wetwSW, n=5)
alldataSW$wetmSWs <- TTR::SMA(alldataSW$wetmSW, n=3)
alldataSW$pasSWs <- TTR::SMA(alldataSW$pasSW, n=2)
alldataSW$araSWs <- TTR::SMA(alldataSW$araSW, n=2)
alldataSW$heaSWs <- TTR::SMA(alldataSW$heaSW, n=2)
alldataSWs <- alldataSW[8:nrow(alldataSW),]
write.csv(alldataSWs, file = "./Processed_data/Full_datasets/alldataSWs_count.csv", row.names = FALSE)

vars::VARselect(alldataSWs$conSWs, lag.max=7, type="none") #10 5
vars::VARselect(alldataSWs$decSWs, lag.max=7, type="none") #7 3
vars::VARselect(alldataSWs$wetwSWs, lag.max=7, type="none") #10 1
vars::VARselect(alldataSWs$wetmSWs, lag.max=7, type="none") #7 4
vars::VARselect(alldataSWs$pasSWs, lag.max=7, type="none") #5 3
vars::VARselect(alldataSWs$araSWs, lag.max=7, type="none") #7 7
vars::VARselect(alldataSWs$heaSWs, lag.max=7, type="none") #4 4


### SOUTHMID
merged_data_SM <-read.csv("./Processed_data/LCC_data/LCC_count/merged_data_SM.csv")
spdSM <- read.csv("./Processed_data/SPD_data/spdSM.csv")
paleoviewSM <- read.csv("./Processed_data/Climate/paleoviewSM.csv") %>%
  dplyr::rename(calBP = Year..BP.) %>%
  dplyr::select(calBP, Area.Mean)

alldataSM <- merge(merged_data_SM, spdSM, by="calBP", all.x=TRUE) %>%
  merge(paleoviewSM, by="calBP", all.x=TRUE) %>%
  dplyr::rename(clim = Area.Mean) %>%
  dplyr::rename(SPD = SPD_med) %>%
  dplyr::rename(yearsBP = calBP)%>%
  .[order(.$yearsBP, decreasing = TRUE), ]

smooth::sma(alldataSM$conSM, ic="BIC", h=0, interval="none") #8
smooth::sma(alldataSM$decSM, ic="BIC", h=0, interval="none") #6
smooth::sma(alldataSM$wetwSM, ic="BIC", h=0, interval="none") #4
smooth::sma(alldataSM$wetmSM, ic="BIC", h=0, interval="none") #6
smooth::sma(alldataSM$pasSM, ic="BIC", h=0, interval="none") #6
smooth::sma(alldataSM$araSM, ic="BIC", h=0, interval="none") #2
smooth::sma(alldataSM$heaSM, ic="BIC", h=0, interval="none") #2

alldataSM$conSMs <- TTR::SMA(alldataSM$conSM, n=8)
alldataSM$decSMs <- TTR::SMA(alldataSM$decSM, n=6)
alldataSM$wetwSMs <- TTR::SMA(alldataSM$wetwSM, n=4)
alldataSM$wetmSMs <- TTR::SMA(alldataSM$wetmSM, n=6)
alldataSM$pasSMs <- TTR::SMA(alldataSM$pasSM, n=6)
alldataSM$araSMs <- TTR::SMA(alldataSM$araSM, n=2)
alldataSM$heaSMs <- TTR::SMA(alldataSM$heaSM, n=2)
alldataSMs <- alldataSM[8:nrow(alldataSM),]
write.csv(alldataSMs, file = "./Processed_data/Full_datasets/alldataSMs_count.csv", row.names = FALSE)

vars::VARselect(alldataSMs$conSMs, lag.max=7, type="none") #9 1
vars::VARselect(alldataSMs$decSMs, lag.max=7, type="none") #7 7
vars::VARselect(alldataSMs$wetwSMs, lag.max=7, type="none") #9 5
vars::VARselect(alldataSMs$wetmSMs, lag.max=7, type="none") #8 2
vars::VARselect(alldataSMs$pasSMs, lag.max=7, type="none") #7 7
vars::VARselect(alldataSMs$araSMs, lag.max=7, type="none") #10 10
vars::VARselect(alldataSMs$heaSMs, lag.max=7, type="none") #8 5