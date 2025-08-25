### cross validation ###
# Last modifed by G.Vernham 14/01/2025

# LOAD LIBRARIES ----

libs <- c("dplyr","modelr", "glm2", "timetk", "purrr", "stats")

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

options(scipen=999)
dir.create(file.path("./Results/", "Cross_validation"), showWarnings = FALSE)

# LOAD DATA ----
alldataNs = read.csv("./Processed_data/Full_datasets/alldataNs_abun.csv")
alldataMMs = read.csv("./Processed_data/Full_datasets/alldataMMs_abun.csv")
alldataMWs = read.csv("./Processed_data/Full_datasets/alldataMWs_abun.csv")
alldataSEs = read.csv("./Processed_data/Full_datasets/alldataSEs_abun.csv")
alldataSMs = read.csv("./Processed_data/Full_datasets/alldataSMs_abun.csv")
alldataSWs = read.csv("./Processed_data/Full_datasets/alldataSWs_abun.csv")

# Subset: BEFORE THE ONSET OF FARMING -----
alldataNb <- alldataNs[which(alldataNs$yearsBP >= 2500),]
alldataSEb <- alldataSEs[which(alldataSEs$yearsBP >= 4000),]
alldataMWb <- alldataMWs[which(alldataMWs$yearsBP >= 3200),]
alldataMMb <- alldataMMs[which(alldataMMs$yearsBP >= 3200),]
alldataSWb <- alldataSWs[which(alldataSWs$yearsBP >= 6000),]
alldataSMb <- alldataSMs[which(alldataSMs$yearsBP >= 6000),]

# Subset: AFTER THE ONSET OF FARMING -----
alldataNa <- alldataNs[which(alldataNs$yearsBP < 2500),]
alldataSEa <- alldataSEs[which(alldataSEs$yearsBP < 4000),]
alldataMWa <- alldataMWs[which(alldataMWs$yearsBP < 3200),]
alldataMMa <- alldataMMs[which(alldataMMs$yearsBP < 3200),]
alldataSWa <- alldataSWs[which(alldataSWs$yearsBP < 6000),]
alldataSMa <- alldataSMs[which(alldataSMs$yearsBP < 6000),]

# FUNCTIONS ----
source("./Functions/Cross_validation.R")
source("./Functions/Create_vectors.R")

# Parametres ----
nfolds = 2
maxlag = 7

#results
#whole holocene ----
yearsBP = alldataNs$yearsBP
columns = c("Lag","MSE") 
Cross_val_df = data.frame(matrix(nrow = 0, ncol = 2)) 
colnames(Cross_val_df) <- columns

x = crossvalidation(conNs, SPD.N, clim.N) #2
rownames(x) <- "conNs"
Cross_val_df[1,] <- x
x = crossvalidation(decNs, SPD.N, clim.N) #1
rownames(x) <- "decNs"
Cross_val_df[2,] <- x
x = crossvalidation(wetwNs, SPD.N, clim.N) #1
rownames(x) <- "wetwNs"
Cross_val_df[3,] <- x
x = crossvalidation(wetmNs, SPD.N, clim.N) #2
rownames(x) <- "wetmNs"
Cross_val_df[4,] <- x
x = crossvalidation(pasNs, SPD.N, clim.N) #1
rownames(x) <- "pasNs"
Cross_val_df[5,] <- x
x = crossvalidation(araNs, SPD.N, clim.N) #1
rownames(x) <- "araNs"
Cross_val_df[6,] <- x
x = crossvalidation(heaNs, SPD.N, clim.N) #2
rownames(x) <- "heaNs"
Cross_val_df[7,] <- x

yearsBP = alldataSEs$yearsBP
x = crossvalidation(conSEs, SPD.SE, clim.SE) #4
rownames(x) <- "conSEs"
Cross_val_df[8,] <- x
x = crossvalidation(decSEs, SPD.SE, clim.SE) #6
rownames(x) <- "decSEs"
Cross_val_df[9,] <- x
x = crossvalidation(wetwSEs, SPD.SE, clim.SE) #1
rownames(x) <- "wetwSEs"
Cross_val_df[10,] <- x
x = crossvalidation(wetmSEs, SPD.SE, clim.SE) #4
rownames(x) <- "wetmSEs"
Cross_val_df[11,] <- x
x = crossvalidation(pasSEs, SPD.SE, clim.SE) #1
rownames(x) <- "pasSEs"
Cross_val_df[12,] <- x
x = crossvalidation(araSEs, SPD.SE, clim.SE) #3
rownames(x) <- "araSEs"
Cross_val_df[13,] <- x
x = crossvalidation(heaSEs, SPD.SE, clim.SE) #1
rownames(x) <- "heaSEs"
Cross_val_df[14,] <- x

yearsBP = alldataMWs$yearsBP
x = crossvalidation(conMWs, SPD.MW, clim.MW) #1
rownames(x) <- "conMWs"
Cross_val_df[15,] <- x
x = crossvalidation(decMWs, SPD.MW, clim.MW) #3
rownames(x) <- "decMWs"
Cross_val_df[16,] <- x
x = crossvalidation(wetwMWs, SPD.MW, clim.MW) #2
rownames(x) <- "wetwMWs"
Cross_val_df[17,] <- x
x = crossvalidation(wetmMWs, SPD.MW, clim.MW) #1
rownames(x) <- "wetmMWs"
Cross_val_df[18,] <- x
x = crossvalidation(pasMWs, SPD.MW, clim.MW) #1
rownames(x) <- "pasMWs"
Cross_val_df[19,] <- x
x = crossvalidation(araMWs, SPD.MW, clim.MW) #1
rownames(x) <- "araMWs"
Cross_val_df[20,] <- x
x = crossvalidation(heaMWs, SPD.MW, clim.MW) #2
rownames(x) <- "heaMWs"
Cross_val_df[21,] <- x

yearsBP = alldataMMs$yearsBP
x = crossvalidation(conMMs, SPD.MM, clim.MM) #4
rownames(x) <- "conMMs"
Cross_val_df[22,] <- x
x = crossvalidation(decMMs, SPD.MM, clim.MM) #4
rownames(x) <- "decMMs"
Cross_val_df[23,] <- x
x = crossvalidation(wetwMMs, SPD.MM, clim.MM) #4
rownames(x) <- "wetwMMs"
Cross_val_df[24,] <- x
x = crossvalidation(wetmMMs, SPD.MM, clim.MM) #1
rownames(x) <- "wetmMMs"
Cross_val_df[25,] <- x
x = crossvalidation(pasMMs, SPD.MM, clim.MM) #3
rownames(x) <- "pasMMs"
Cross_val_df[26,] <- x
x = crossvalidation(araMMs, SPD.MM, clim.MM) #4
rownames(x) <- "araMMs"
Cross_val_df[27,] <- x
x = crossvalidation(heaMMs, SPD.MM, clim.MM) #6
rownames(x) <- "heaMMs"
Cross_val_df[28,] <- x

yearsBP = alldataSWs$yearsBP
x = crossvalidation(conSWs, SPD.SW, clim.SW) #1
rownames(x) <- "conSWs"
Cross_val_df[29,] <- x
x = crossvalidation(decSWs, SPD.SW, clim.SW) #1
rownames(x) <- "decSWs"
Cross_val_df[30,] <- x
x = crossvalidation(wetwSWs, SPD.SW, clim.SW) #1
rownames(x) <- "wetwSWs"
Cross_val_df[31,] <- x
x = crossvalidation(wetmSWs, SPD.SW, clim.SW) #1
rownames(x) <- "wetmSWs"
Cross_val_df[32,] <- x
x = crossvalidation(pasSWs, SPD.SW, clim.SW) #5
rownames(x) <- "pasSWs"
Cross_val_df[33,] <- x
x = crossvalidation(araSWs, SPD.SW, clim.SW) #3
rownames(x) <- "araSWs"
Cross_val_df[34,] <- x
x = crossvalidation(heaSWs, SPD.SW, clim.SW) #3
rownames(x) <- "heaSWs"
Cross_val_df[35,] <- x

yearsBP = alldataSMs$yearsBP
x = crossvalidation(conSMs, SPD.SM, clim.SM) #6
rownames(x) <- "conSMs"
Cross_val_df[36,] <- x
x = crossvalidation(decSMs, SPD.SM, clim.SM) #1
rownames(x) <- "decSMs"
Cross_val_df[37,] <- x
x = crossvalidation(wetwSMs, SPD.SM, clim.SM) #1
rownames(x) <- "wetwSMs"
Cross_val_df[38,] <- x
x = crossvalidation(wetmSMs, SPD.SM, clim.SM) #1
rownames(x) <- "wetmSMs"
Cross_val_df[39,] <- x
x = crossvalidation(pasSMs, SPD.SM, clim.SM) #1
rownames(x) <- "pasSMs"
Cross_val_df[40,] <- x
x = crossvalidation(araSMs, SPD.SM, clim.SM) #5
rownames(x) <- "araSMs"
Cross_val_df[41,] <- x
x = crossvalidation(heaSMs, SPD.SM, clim.SM) #1
rownames(x) <- "heaSMs"
Cross_val_df[42,] <- x
x <- Cross_val_df

Cross_val_df$Lag <-gsub("lag","", Cross_val_df$Lag)
Cross_val_df$Lag <- as.integer(Cross_val_df$Lag)
Cross_val_df <- cbind(LCC = rownames(Cross_val_df), Cross_val_df)
write.csv(Cross_val_df, "./Results/Cross_validation/Cross_validation_results_abun.csv", row.names = F)


# Before Farming ----
yearsBP = alldataNb$yearsBP
columns = c("Lag","MSE") 
Cross_val_df = data.frame(matrix(nrow = 0, ncol = 2)) 
colnames(Cross_val_df) <- columns

x = crossvalidation(conNb, SPD.Nb, clim.Nb) #2
rownames(x) <- "conNs"
Cross_val_df[1,] <- x
x = crossvalidation(decNb, SPD.Nb, clim.Nb) #1
rownames(x) <- "decNs"
Cross_val_df[2,] <- x
x = crossvalidation(wetwNb, SPD.Nb, clim.Nb) #1
rownames(x) <- "wetwNs"
Cross_val_df[3,] <- x
x = crossvalidation(wetmNb, SPD.Nb, clim.Nb) #2
rownames(x) <- "wetmNs"
Cross_val_df[4,] <- x
x = crossvalidation(pasNb, SPD.Nb, clim.Nb) #1
rownames(x) <- "pasNs"
Cross_val_df[5,] <- x
x = crossvalidation(araNb, SPD.Nb, clim.Nb) #1
rownames(x) <- "araNs"
Cross_val_df[6,] <- x
x = crossvalidation(heaNb, SPD.Nb, clim.Nb) #2
rownames(x) <- "heaNs"
Cross_val_df[7,] <- x

yearsBP = alldataSEb$yearsBP
x = crossvalidation(conSEb, SPD.SEb, clim.SEb) #4
rownames(x) <- "conSEs"
Cross_val_df[8,] <- x
x = crossvalidation(decSEb, SPD.SEb, clim.SEb) #6
rownames(x) <- "decSEs"
Cross_val_df[9,] <- x
x = crossvalidation(wetwSEb, SPD.SEb, clim.SEb) #1
rownames(x) <- "wetwSEs"
Cross_val_df[10,] <- x
x = crossvalidation(wetmSEb, SPD.SEb, clim.SEb) #4
rownames(x) <- "wetmSEs"
Cross_val_df[11,] <- x
x = crossvalidation(pasSEb, SPD.SEb, clim.SEb) #1
rownames(x) <- "pasSEs"
Cross_val_df[12,] <- x
x = crossvalidation(araSEb, SPD.SEb, clim.SEb) #3
rownames(x) <- "araSEs"
Cross_val_df[13,] <- x
x = crossvalidation(heaSEb, SPD.SEb, clim.SEb) #1
rownames(x) <- "heaSEs"
Cross_val_df[14,] <- x

yearsBP = alldataMWb$yearsBP
x = crossvalidation(conMWb, SPD.MWb, clim.MWb) #1
rownames(x) <- "conMWs"
Cross_val_df[15,] <- x
x = crossvalidation(decMWb, SPD.MWb, clim.MWb) #3
rownames(x) <- "decMWs"
Cross_val_df[16,] <- x
x = crossvalidation(wetwMWb, SPD.MWb, clim.MWb) #2
rownames(x) <- "wetwMWs"
Cross_val_df[17,] <- x
x = crossvalidation(wetmMWb, SPD.MWb, clim.MWb) #1
rownames(x) <- "wetmMWs"
Cross_val_df[18,] <- x
x = crossvalidation(pasMWb, SPD.MWb, clim.MWb) #1
rownames(x) <- "pasMWs"
Cross_val_df[19,] <- x
x = crossvalidation(araMWb, SPD.MWb, clim.MWb) #1
rownames(x) <- "araMWs"
Cross_val_df[20,] <- x
x = crossvalidation(heaMWb, SPD.MWb, clim.MWb) #2
rownames(x) <- "heaMWs"
Cross_val_df[21,] <- x

yearsBP = alldataMMb$yearsBP
x = crossvalidation(conMMb, SPD.MMb, clim.MMb) #4
rownames(x) <- "conMMs"
Cross_val_df[22,] <- x
x = crossvalidation(decMMb, SPD.MMb, clim.MMb) #4
rownames(x) <- "decMMs"
Cross_val_df[23,] <- x
x = crossvalidation(wetwMMb, SPD.MMb, clim.MMb) #4
rownames(x) <- "wetwMMs"
Cross_val_df[24,] <- x
x = crossvalidation(wetmMMb, SPD.MMb, clim.MMb) #1
rownames(x) <- "wetmMMs"
Cross_val_df[25,] <- x
x = crossvalidation(pasMMb, SPD.MMb, clim.MMb) #3
rownames(x) <- "pasMMs"
Cross_val_df[26,] <- x
x = crossvalidation(araMMb, SPD.MMb, clim.MMb) #4
rownames(x) <- "araMMs"
Cross_val_df[27,] <- x
x = crossvalidation(heaMMb, SPD.MMb, clim.MMb) #6
rownames(x) <- "heaMMs"
Cross_val_df[28,] <- x

yearsBP = alldataSWb$yearsBP
x = crossvalidation(conSWb, SPD.SWb, clim.SWb) #1
rownames(x) <- "conSWs"
Cross_val_df[29,] <- x
x = crossvalidation(decSWb, SPD.SWb, clim.SWb) #1
rownames(x) <- "decSWs"
Cross_val_df[30,] <- x
x = crossvalidation(wetwSWb, SPD.SWb, clim.SWb) #1
rownames(x) <- "wetwSWs"
Cross_val_df[31,] <- x
x = crossvalidation(wetmSWb, SPD.SWb, clim.SWb) #1
rownames(x) <- "wetmSWs"
Cross_val_df[32,] <- x
x = crossvalidation(pasSWb, SPD.SWb, clim.SWb) #5
rownames(x) <- "pasSWs"
Cross_val_df[33,] <- x
x = crossvalidation(araSWb, SPD.SWb, clim.SWb) #3
rownames(x) <- "araSWs"
Cross_val_df[34,] <- x
x = crossvalidation(heaSWb, SPD.SWb, clim.SWb) #3
rownames(x) <- "heaSWs"
Cross_val_df[35,] <- x

yearsBP = alldataSMb$yearsBP
x = crossvalidation(conSMb, SPD.SMb, clim.SMb) #6
rownames(x) <- "conSMs"
Cross_val_df[36,] <- x
x = crossvalidation(decSMb, SPD.SMb, clim.SMb) #1
rownames(x) <- "decSMs"
Cross_val_df[37,] <- x
x = crossvalidation(wetwSMb, SPD.SMb, clim.SMb) #1
rownames(x) <- "wetwSMs"
Cross_val_df[38,] <- x
x = crossvalidation(wetmSMb, SPD.SMb, clim.SMb) #1
rownames(x) <- "wetmSMs"
Cross_val_df[39,] <- x
x = crossvalidation(pasSMb, SPD.SMb, clim.SMb) #1
rownames(x) <- "pasSMs"
Cross_val_df[40,] <- x
x = crossvalidation(araSMb, SPD.SMb, clim.SMb) #5
rownames(x) <- "araSMs"
Cross_val_df[41,] <- x
x = crossvalidation(heaSMb, SPD.SMb, clim.SMb) #1
rownames(x) <- "heaSMs"
Cross_val_df[42,] <- x
x <- Cross_val_df

Cross_val_df$Lag <-gsub("lag","", Cross_val_df$Lag)
Cross_val_df$Lag <- as.integer(Cross_val_df$Lag)
Cross_val_df <- cbind(LCC = rownames(Cross_val_df), Cross_val_df)
write.csv(Cross_val_df, "./Results/Cross_validation/Cross_validation_results_abun_before.csv", row.names = F)

# After Farming ----
yearsBP = alldataNa$yearsBP
columns = c("Lag","MSE") 
Cross_val_df = data.frame(matrix(nrow = 0, ncol = 2)) 
colnames(Cross_val_df) <- columns

x = crossvalidation(conNa, SPD.Na, clim.Na) #2
rownames(x) <- "conNs"
Cross_val_df[1,] <- x
x = crossvalidation(decNa, SPD.Na, clim.Na) #1
rownames(x) <- "decNs"
Cross_val_df[2,] <- x
x = crossvalidation(wetwNa, SPD.Na, clim.Na) #1
rownames(x) <- "wetwNs"
Cross_val_df[3,] <- x
x = crossvalidation(wetmNa, SPD.Na, clim.Na) #2
rownames(x) <- "wetmNs"
Cross_val_df[4,] <- x
x = crossvalidation(pasNa, SPD.Na, clim.Na) #1
rownames(x) <- "pasNs"
Cross_val_df[5,] <- x
x = crossvalidation(araNa, SPD.Na, clim.Na) #1
rownames(x) <- "araNs"
Cross_val_df[6,] <- x
x = crossvalidation(heaNa, SPD.Na, clim.Na) #2
rownames(x) <- "heaNs"
Cross_val_df[7,] <- x

yearsBP = alldataSEa$yearsBP
x = crossvalidation(conSEa, SPD.SEa, clim.SEa) #4
rownames(x) <- "conSEs"
Cross_val_df[8,] <- x
x = crossvalidation(decSEa, SPD.SEa, clim.SEa) #6
rownames(x) <- "decSEs"
Cross_val_df[9,] <- x
x = crossvalidation(wetwSEa, SPD.SEa, clim.SEa) #1
rownames(x) <- "wetwSEs"
Cross_val_df[10,] <- x
x = crossvalidation(wetmSEa, SPD.SEa, clim.SEa) #4
rownames(x) <- "wetmSEs"
Cross_val_df[11,] <- x
x = crossvalidation(pasSEa, SPD.SEa, clim.SEa) #1
rownames(x) <- "pasSEs"
Cross_val_df[12,] <- x
x = crossvalidation(araSEa, SPD.SEa, clim.SEa) #3
rownames(x) <- "araSEs"
Cross_val_df[13,] <- x
x = crossvalidation(heaSEa, SPD.SEa, clim.SEa) #1
rownames(x) <- "heaSEs"
Cross_val_df[14,] <- x

yearsBP = alldataMWa$yearsBP
x = crossvalidation(conMWa, SPD.MWa, clim.MWa) #1
rownames(x) <- "conMWs"
Cross_val_df[15,] <- x
x = crossvalidation(decMWa, SPD.MWa, clim.MWa) #3
rownames(x) <- "decMWs"
Cross_val_df[16,] <- x
x = crossvalidation(wetwMWa, SPD.MWa, clim.MWa) #2
rownames(x) <- "wetwMWs"
Cross_val_df[17,] <- x
x = crossvalidation(wetmMWa, SPD.MWa, clim.MWa) #1
rownames(x) <- "wetmMWs"
Cross_val_df[18,] <- x
x = crossvalidation(pasMWa, SPD.MWa, clim.MWa) #1
rownames(x) <- "pasMWs"
Cross_val_df[19,] <- x
x = crossvalidation(araMWa, SPD.MWa, clim.MWa) #1
rownames(x) <- "araMWs"
Cross_val_df[20,] <- x
x = crossvalidation(heaMWa, SPD.MWa, clim.MWa) #2
rownames(x) <- "heaMWs"
Cross_val_df[21,] <- x

yearsBP = alldataMMa$yearsBP
x = crossvalidation(conMMa, SPD.MMa, clim.MMa) #4
rownames(x) <- "conMMs"
Cross_val_df[22,] <- x
x = crossvalidation(decMMa, SPD.MMa, clim.MMa) #4
rownames(x) <- "decMMs"
Cross_val_df[23,] <- x
x = crossvalidation(wetwMMa, SPD.MMa, clim.MMa) #4
rownames(x) <- "wetwMMs"
Cross_val_df[24,] <- x
x = crossvalidation(wetmMMa, SPD.MMa, clim.MMa) #1
rownames(x) <- "wetmMMs"
Cross_val_df[25,] <- x
x = crossvalidation(pasMMa, SPD.MMa, clim.MMa) #3
rownames(x) <- "pasMMs"
Cross_val_df[26,] <- x
x = crossvalidation(araMMa, SPD.MMa, clim.MMa) #4
rownames(x) <- "araMMs"
Cross_val_df[27,] <- x
x = crossvalidation(heaMMa, SPD.MMa, clim.MMa) #6
rownames(x) <- "heaMMs"
Cross_val_df[28,] <- x

yearsBP = alldataSWa$yearsBP
x = crossvalidation(conSWa, SPD.SWa, clim.SWa) #1
rownames(x) <- "conSWs"
Cross_val_df[29,] <- x
x = crossvalidation(decSWa, SPD.SWa, clim.SWa) #1
rownames(x) <- "decSWs"
Cross_val_df[30,] <- x
x = crossvalidation(wetwSWa, SPD.SWa, clim.SWa) #1
rownames(x) <- "wetwSWs"
Cross_val_df[31,] <- x
x = crossvalidation(wetmSWa, SPD.SWa, clim.SWa) #1
rownames(x) <- "wetmSWs"
Cross_val_df[32,] <- x
x = crossvalidation(pasSWa, SPD.SWa, clim.SWa) #5
rownames(x) <- "pasSWs"
Cross_val_df[33,] <- x
x = crossvalidation(araSWa, SPD.SWa, clim.SWa) #3
rownames(x) <- "araSWs"
Cross_val_df[34,] <- x
x = crossvalidation(heaSWa, SPD.SWa, clim.SWa) #3
rownames(x) <- "heaSWs"
Cross_val_df[35,] <- x

yearsBP = alldataSMa$yearsBP
x = crossvalidation(conSMa, SPD.SMa, clim.SMa) #6
rownames(x) <- "conSMs"
Cross_val_df[36,] <- x
x = crossvalidation(decSMa, SPD.SMa, clim.SMa) #1
rownames(x) <- "decSMs"
Cross_val_df[37,] <- x
x = crossvalidation(wetwSMa, SPD.SMa, clim.SMa) #1
rownames(x) <- "wetwSMs"
Cross_val_df[38,] <- x
x = crossvalidation(wetmSMa, SPD.SMa, clim.SMa) #1
rownames(x) <- "wetmSMs"
Cross_val_df[39,] <- x
x = crossvalidation(pasSMa, SPD.SMa, clim.SMa) #1
rownames(x) <- "pasSMs"
Cross_val_df[40,] <- x
x = crossvalidation(araSMa, SPD.SMa, clim.SMa) #5
rownames(x) <- "araSMs"
Cross_val_df[41,] <- x
x = crossvalidation(heaSMa, SPD.SMa, clim.SMa) #1
rownames(x) <- "heaSMs"
Cross_val_df[42,] <- x
x <- Cross_val_df

Cross_val_df$Lag <-gsub("lag","", Cross_val_df$Lag)
Cross_val_df$Lag <- as.integer(Cross_val_df$Lag)
Cross_val_df <- cbind(LCC = rownames(Cross_val_df), Cross_val_df)
write.csv(Cross_val_df, "./Results/Cross_validation/Cross_validation_results_abun_after.csv", row.names = F)