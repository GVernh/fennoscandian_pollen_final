### Granger causality test ###

# Load library ----
libs <- c("dplyr","vars", "bruceR", "lmtest", "stats", "glm2")

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
dir.create(file.path("./", "Results"), showWarnings = FALSE)
dir.create(file.path("./Results/", "Commonality_analysis"), showWarnings = FALSE)

# Data ----
alldataNs <- read.csv("./Processed_data/Full_datasets/alldataNs_abun.csv")
alldataMMs <- read.csv("./Processed_data/Full_datasets/alldataMMs_abun.csv")
alldataMWs <- read.csv("./Processed_data/Full_datasets/alldataMWs_abun.csv")
alldataSEs <- read.csv("./Processed_data/Full_datasets/alldataSEs_abun.csv")
alldataSMs <- read.csv("./Processed_data/Full_datasets/alldataSMs_abun.csv")
alldataSWs <- read.csv("./Processed_data/Full_datasets/alldataSWs_abun.csv")

Cross_val_df <- read.csv("./Results/Cross_validation/Cross_validation_results_abun.csv")
Cross_val_df_before <- read.csv("./Results/Cross_validation/Cross_validation_results_abun_before.csv")
Cross_val_df_after <- read.csv("./Results/Cross_validation/Cross_validation_results_abun_after.csv")

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

# Functions ----
source("./Functions/Custom_commonality_analysis.R")
source("./Functions/Create_vectors.R")

### NORTH ### ----
# NORTH (ALL)

coefs <- data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("SPD", "Climate", "All","SPD_before", "Climate_before", 
                                                                "All_before","SPD_after", "Climate_after", "All_after"))))

vm <- vars::VAR(alldataNs[c("SPD", "clim", "conNs")], p = Cross_val_df$Lag[Cross_val_df$LCC== "conNs"]) 
y = bruceR::granger_causality(varmodel = vm, var.y = "conNs", var.x = c("SPD", "clim")) # No sig
com = custom_commonality_analysis(conNs, SPD.N, clim.N, Cross_val_df$Lag[Cross_val_df$LCC== "conNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Coniferous woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "decNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decNs", var.x = c("SPD", "clim")) # No sig
com = custom_commonality_analysis(decNs, SPD.N, clim.N, Cross_val_df$Lag[Cross_val_df$LCC== "decNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Deciduous woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)


vm <- vars::VAR(alldataNs[c("SPD", "clim", "wetwNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwNs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetwNs, SPD.N, clim.N, Cross_val_df$Lag[Cross_val_df$LCC== "wetwNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Wet woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "wetmNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmNs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetmNs, SPD.N, clim.N, Cross_val_df$Lag[Cross_val_df$LCC== "wetmNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Wet meadow"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "pasNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasNs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(pasNs, SPD.N, clim.N, Cross_val_df$Lag[Cross_val_df$LCC== "pasNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Pasture"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "araNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araNs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(araNs, SPD.N, clim.N, Cross_val_df$Lag[Cross_val_df$LCC== "araNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Arable land"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "heaNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaNs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(heaNs, SPD.N, clim.N, Cross_val_df$Lag[Cross_val_df$LCC== "heaNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Heath"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

# North (BEFORE)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "conNs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "conNs"]) 
y = bruceR::granger_causality(varmodel = vm, var.y = "conNs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(conNb, SPD.Nb, clim.Nb, Cross_val_df$Lag[Cross_val_df_before$LCC== "conNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[1,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[1,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[1,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "decNs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "decNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decNs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(decNb, SPD.Nb, clim.Nb, Cross_val_df$Lag[Cross_val_df_before$LCC== "decNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[2,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[2,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[2,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "wetwNs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetwNs"])
y = granger_causality(varmodel = vm, var.y = "wetwNs", var.x = c("SPD", "clim")) # All sig 
com = custom_commonality_analysis(wetwNb, SPD.Nb, clim.Nb, Cross_val_df$Lag[Cross_val_df_before$LCC== "wetwNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[3,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[3,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[3,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "wetmNs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetmNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmNs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetmNb, SPD.Nb, clim.Nb, Cross_val_df$Lag[Cross_val_df_before$LCC== "wetmNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[4,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[4,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[4,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "pasNs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "pasNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasNs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(pasNb, SPD.Nb, clim.Nb, Cross_val_df$Lag[Cross_val_df_before$LCC== "pasNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[5,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[5,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[5,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "araNs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "araNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araNs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(araNb, SPD.Nb, clim.Nb, Cross_val_df$Lag[Cross_val_df_before$LCC== "araNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[6,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[6,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[6,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "heaNs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "heaNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaNs", var.x = c("SPD", "clim")) # SPD/clim/all sig
com = custom_commonality_analysis(heaNb, SPD.Nb, clim.Nb, Cross_val_df$Lag[Cross_val_df_before$LCC== "heaNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[7,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[7,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[7,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

# North (AFTER)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "conNs")], p=Cross_val_df$Lag[Cross_val_df_after$LCC== "conNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conNs", var.x = c("SPD", "clim")) # NOT ENOUGH DATA
com = custom_commonality_analysis(conNa, SPD.Na, clim.Na, Cross_val_df$Lag[Cross_val_df_after$LCC== "conNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[1,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[1,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[1,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "decNs")], p=Cross_val_df$Lag[Cross_val_df_after$LCC== "decNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decNs", var.x = c("SPD", "clim")) # NOT ENOUGH DATA
com = custom_commonality_analysis(decNa, SPD.Na, clim.Na, Cross_val_df$Lag[Cross_val_df_after$LCC== "decNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[2,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[2,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[2,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "wetwNs")], p=Cross_val_df$Lag[Cross_val_df_after$LCC== "wetwNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwNs", var.x = c("SPD", "clim")) # NOT ENOUGH DATA
com = custom_commonality_analysis(wetwNa, SPD.Na, clim.Na, Cross_val_df$Lag[Cross_val_df_after$LCC== "wetwNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[3,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[3,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[3,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "wetmNs")], p=Cross_val_df$Lag[Cross_val_df_after$LCC== "wetmNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmNs", var.x = c("SPD", "clim")) # NOT ENOUGH DATA
com = custom_commonality_analysis(wetmNa, SPD.Na, clim.Na, Cross_val_df$Lag[Cross_val_df_after$LCC== "wetmNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[4,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[4,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[4,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "pasNs")], p=Cross_val_df$Lag[Cross_val_df_after$LCC== "pasNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasNs", var.x = c("SPD", "clim")) # NOT ENOUGH DATA
com = custom_commonality_analysis(pasNa, SPD.Na, clim.Na, Cross_val_df$Lag[Cross_val_df_after$LCC== "pasNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[5,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[5,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[5,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)


vm <- vars::VAR(alldataNa[c("SPD", "clim", "araNs")], p=Cross_val_df$Lag[Cross_val_df_after$LCC== "araNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araNs", var.x = c("SPD", "clim")) # NOT ENOUGH DATA
com = custom_commonality_analysis(araNa, SPD.Na, clim.Na, Cross_val_df$Lag[Cross_val_df_after$LCC== "araNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[6,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[6,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[6,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "heaNs")], p=Cross_val_df$Lag[Cross_val_df_after$LCC== "heaNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaNs", var.x = c("SPD", "clim")) # NOT ENOUGH DATA
com = custom_commonality_analysis(heaNa, SPD.Na, clim.Na, Cross_val_df$Lag[Cross_val_df_after$LCC== "heaNs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[7,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[7,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[7,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

write.csv(as.data.frame(t(coefs)), "./Results/Commonality_analysis/Com_analysis_NORTH_abun.csv", row.names = T)

### SOUTHEAST ### ----
# SOUTHEAST ALL
rm(coefs)
coefs <- data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("SPD", "Climate", "All","SPD_before", "Climate_before", 
                                                                "All_before","SPD_after", "Climate_after", "All_after"))))

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "conSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSEs", var.x = c("SPD", "clim")) # sig all/clim
com = custom_commonality_analysis(conSEs, SPD.SE, clim.SE, Cross_val_df$Lag[Cross_val_df$LCC== "conSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Coniferous woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "decSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSEs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(decSEs, SPD.SE, clim.SE, Cross_val_df$Lag[Cross_val_df$LCC== "decSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Deciduous woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "wetwSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSEs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetwSEs, SPD.SE, clim.SE, Cross_val_df$Lag[Cross_val_df$LCC== "wetwSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Wet woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "wetmSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSEs", var.x = c("SPD", "clim")) # Clim/all sig
com = custom_commonality_analysis(wetmSEs, SPD.SE, clim.SE, Cross_val_df$Lag[Cross_val_df$LCC== "wetmSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Wet meadow"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "pasSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSEs", var.x = c("SPD", "clim")) # spd/all sig
com = custom_commonality_analysis(pasSEs, SPD.SE, clim.SE, Cross_val_df$Lag[Cross_val_df$LCC== "pasSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Pasture"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "araSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSEs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(araSEs, SPD.SE, clim.SE, Cross_val_df$Lag[Cross_val_df$LCC== "araSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Arable land"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "heaSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSEs", var.x = c("SPD", "clim")) # SPD/all sig
com = custom_commonality_analysis(heaSEs, SPD.SE, clim.SE, Cross_val_df$Lag[Cross_val_df$LCC== "heaSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Heath"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

# SOUTHEAST BEFORE
vm <- vars::VAR(alldataSEb[c("SPD", "clim", "conSEs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "conSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSEs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(conSEb, SPD.SEb, clim.SEb, Cross_val_df$Lag[Cross_val_df_before$LCC== "conSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[1,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[1,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[1,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "decSEs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "decSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSEs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(decSEb, SPD.SEb, clim.SEb, Cross_val_df$Lag[Cross_val_df_before$LCC== "decSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[2,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[2,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[2,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "wetwSEs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetwSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSEs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetwSEb, SPD.SEb, clim.SEb, Cross_val_df$Lag[Cross_val_df_before$LCC== "wetwSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[3,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[3,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[3,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "wetmSEs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetmSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSEs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetmSEb, SPD.SEb, clim.SEb, Cross_val_df$Lag[Cross_val_df_before$LCC== "wetmSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[4,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[4,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[4,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "pasSEs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "pasSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSEs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(pasSEb, SPD.SEb, clim.SEb, Cross_val_df$Lag[Cross_val_df_before$LCC== "pasSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[5,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[5,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[5,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "araSEs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "araSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSEs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(araSEb, SPD.SEb, clim.SEb, Cross_val_df$Lag[Cross_val_df_before$LCC== "araSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[6,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[6,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[6,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "heaSEs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "heaSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSEs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(heaSEb, SPD.SEb, clim.SEb, Cross_val_df$Lag[Cross_val_df_before$LCC== "heaSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[7,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[7,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[7,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

# SOUTHEAST AFTER

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "conSEs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "conSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSEs", var.x = c("SPD", "clim"))
com = custom_commonality_analysis(conSEa, SPD.SEa, clim.SEa, Cross_val_df$Lag[Cross_val_df_after$LCC== "conSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[1,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[1,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[1,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "decSEs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "decSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSEs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(decSEa, SPD.SEa, clim.SEa, Cross_val_df$Lag[Cross_val_df_after$LCC== "decSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[2,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[2,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[2,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "wetwSEs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetwSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSEs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(wetwSEa, SPD.SEa, clim.SEa, Cross_val_df$Lag[Cross_val_df_after$LCC== "wetwSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[3,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[3,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[3,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "wetmSEs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetmSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSEs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(wetmSEa, SPD.SEa, clim.SEa, Cross_val_df$Lag[Cross_val_df_after$LCC== "wetmSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[4,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[4,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[4,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "pasSEs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "pasSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSEs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(pasSEa, SPD.SEa, clim.SEa, Cross_val_df$Lag[Cross_val_df_after$LCC== "pasSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[5,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[5,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[5,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "araSEs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "araSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSEs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(araSEa, SPD.SEa, clim.SEa, Cross_val_df$Lag[Cross_val_df_after$LCC== "araSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[6,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[6,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[6,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "heaSEs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "heaSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSEs", var.x = c("SPD", "clim")) # SPD/all sig
com = custom_commonality_analysis(heaSEa, SPD.SEa, clim.SEa, Cross_val_df$Lag[Cross_val_df_after$LCC== "heaSEs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[7,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[7,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[7,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

write.csv(as.data.frame(t(coefs)), "./Results/Commonality_analysis/Com_analysis_SOUTHEAST_abun.csv", row.names = T)

### MIDWEST ### ----
# MIDWEST ALL
rm(coefs)
coefs <- data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("SPD", "Climate", "All","SPD_before", "Climate_before", 
                                                                "All_before","SPD_after", "Climate_after", "All_after"))))

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "conMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conMWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(conMWs, SPD.MW, clim.MW, Cross_val_df$Lag[Cross_val_df$LCC== "conMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Coniferous woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "decMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decMWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(decMWs, SPD.MW, clim.MW, Cross_val_df$Lag[Cross_val_df$LCC== "decMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Deciduous woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "wetwMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetwMWs, SPD.MW, clim.MW, Cross_val_df$Lag[Cross_val_df$LCC== "wetwMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Wet woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "wetmMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetmMWs, SPD.MW, clim.MW, Cross_val_df$Lag[Cross_val_df$LCC== "wetmMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Wet meadow"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "pasMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMWs", var.x = c("SPD", "clim")) # SPD/all sig
com = custom_commonality_analysis(pasMWs, SPD.MW, clim.MW, Cross_val_df$Lag[Cross_val_df$LCC== "pasMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Pasture"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "araMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araMWs", var.x = c("SPD", "clim")) # all sig
com = custom_commonality_analysis(araMWs, SPD.MW, clim.MW, Cross_val_df$Lag[Cross_val_df$LCC== "araMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Arable land"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "heaMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(heaMWs, SPD.MW, clim.MW, Cross_val_df$Lag[Cross_val_df$LCC== "heaMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Heath"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)


# MIDWEST BEFORE

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "conMWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "conMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conMWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(conMWb, SPD.MWb, clim.MWb, Cross_val_df_before$Lag[Cross_val_df_before$LCC== "conMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[1,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[1,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[1,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "decMWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "decMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decMWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(decMWb, SPD.MWb, clim.MWb, Cross_val_df_before$Lag[Cross_val_df_before$LCC== "decMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[2,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[2,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[2,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)


vm <- vars::VAR(alldataMWb[c("SPD", "clim", "wetwMWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetwMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMWs", var.x = c("SPD", "clim")) # clim sig
com = custom_commonality_analysis(wetwMWb, SPD.MWb, clim.MWb, Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetwMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[3,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[3,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[3,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "wetmMWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetmMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMWs", var.x = c("SPD", "clim")) # SPD sig
com = custom_commonality_analysis(wetmMWb, SPD.MWb, clim.MWb, Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetmMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[4,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[4,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[4,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "pasMWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "pasMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(pasMWb, SPD.MWb, clim.MWb, Cross_val_df_before$Lag[Cross_val_df_before$LCC== "pasMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[5,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[5,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[5,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)


vm <- vars::VAR(alldataMWb[c("SPD", "clim", "araMWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "araMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araMWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(araMWb, SPD.MWb, clim.MWb, Cross_val_df_before$Lag[Cross_val_df_before$LCC== "araMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[6,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[6,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[6,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "heaMWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "heaMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMWs", var.x = c("SPD", "clim")) # clim/all sig
com = custom_commonality_analysis(heaMWb, SPD.MWb, clim.MWb, Cross_val_df_before$Lag[Cross_val_df_before$LCC== "heaMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[7,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[7,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[7,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

# MIDWEST AFTER

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "conMWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "conMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conMWs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(conMWa, SPD.MWa, clim.MWa, Cross_val_df_before$Lag[Cross_val_df_after$LCC== "conMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[1,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[1,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[1,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)


vm <- vars::VAR(alldataMWa[c("SPD", "clim", "decMWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "decMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decMWs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(decMWa, SPD.MWa, clim.MWa, Cross_val_df_before$Lag[Cross_val_df_after$LCC== "decMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[2,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[2,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[2,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "wetwMWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetwMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMWs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(wetwMWa, SPD.MWa, clim.MWa, Cross_val_df_before$Lag[Cross_val_df_after$LCC== "wetwMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[3,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[3,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[3,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "wetmMWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetmMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMWs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(wetmMWa, SPD.MWa, clim.MWa, Cross_val_df_before$Lag[Cross_val_df_after$LCC== "wetmMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[4,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[4,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[4,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)


vm <- vars::VAR(alldataMWa[c("SPD", "clim", "pasMWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "pasMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMWs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(pasMWa, SPD.MWa, clim.MWa, Cross_val_df_before$Lag[Cross_val_df_after$LCC== "pasMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[5,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[5,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[5,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "araMWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "araMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araMWs", var.x = c("SPD", "clim")) # SPD sig
com = custom_commonality_analysis(araMWa, SPD.MWa, clim.MWa, Cross_val_df_before$Lag[Cross_val_df_after$LCC== "araMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[6,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[6,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[6,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "heaMWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "heaMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMWs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(heaMWa, SPD.MWa, clim.MWa, Cross_val_df_before$Lag[Cross_val_df_after$LCC== "heaMWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[7,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[7,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[7,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

write.csv(as.data.frame(t(coefs)), "./Results/Commonality_analysis/Com_analysis_MIDWEST_abun.csv", row.names = T)

### MIDMID ### ----
# MIDMID ALL
rm(coefs)
coefs <- data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("SPD", "Climate", "All","SPD_before", "Climate_before", 
                                                                "All_before","SPD_after", "Climate_after", "All_after"))))

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "conMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conMMs", var.x = c("SPD", "clim")) # No sig
com = custom_commonality_analysis(conMMs, SPD.MM, clim.MM, Cross_val_df$Lag[Cross_val_df$LCC== "conMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Coniferous woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "decMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decMMs", var.x = c("SPD", "clim")) # SPD/all sig
com = custom_commonality_analysis(decMMs, SPD.MM, clim.MM, Cross_val_df$Lag[Cross_val_df$LCC== "decMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Deciduous woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "wetwMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMMs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetwMMs, SPD.MM, clim.MM, Cross_val_df$Lag[Cross_val_df$LCC== "wetwMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Wet woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "wetmMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMMs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetmMMs, SPD.MM, clim.MM, Cross_val_df$Lag[Cross_val_df$LCC== "wetmMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Wet meadow"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "pasMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMMs", var.x = c("SPD", "clim")) # SPD/all sig
com = custom_commonality_analysis(pasMMs, SPD.MM, clim.MM, Cross_val_df$Lag[Cross_val_df$LCC== "pasMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Pasture"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "araMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araMMs", var.x = c("SPD", "clim")) # SPD/all sig
com = custom_commonality_analysis(araMMs, SPD.MM, clim.MM, Cross_val_df$Lag[Cross_val_df$LCC== "araMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Arable land"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "heaMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMMs", var.x = c("SPD", "clim")) # all sig
com = custom_commonality_analysis(heaMMs, SPD.MM, clim.MM, Cross_val_df$Lag[Cross_val_df$LCC== "heaMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Heath"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

# MIDMID BEFORE

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "conMMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "conMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conMMs", var.x = c("SPD", "clim")) # SPD/clim/all sig
com = custom_commonality_analysis(conMMb, SPD.MMb, clim.MMb, Cross_val_df$Lag[Cross_val_df_before$LCC== "conMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[1,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[1,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[1,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "decMMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "decMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decMMs", var.x = c("SPD", "clim")) # SPD sig
com = custom_commonality_analysis(decMMb, SPD.MMb, clim.MMb, Cross_val_df$Lag[Cross_val_df_before$LCC== "decMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[2,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[2,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[2,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "wetwMMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetwMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMMs", var.x = c("SPD", "clim")) # all sig
com = custom_commonality_analysis(wetwMMb, SPD.MMb, clim.MMb, Cross_val_df$Lag[Cross_val_df_before$LCC== "wetwMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[3,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[3,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[3,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "wetmMMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetmMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMMs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetmMMb, SPD.MMb, clim.MMb, Cross_val_df$Lag[Cross_val_df_before$LCC== "wetmMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[4,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[4,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[4,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "pasMMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "pasMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMMs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(pasMMb, SPD.MMb, clim.MMb, Cross_val_df$Lag[Cross_val_df_before$LCC== "pasMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[5,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[5,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[5,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "araMMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "araMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araMMs", var.x = c("SPD", "clim")) # clim sig
com = custom_commonality_analysis(araMMb, SPD.MMb, clim.MMb, Cross_val_df$Lag[Cross_val_df_before$LCC== "araMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[6,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[6,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[6,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "heaMMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "heaMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMMs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(heaMMb, SPD.MMb, clim.MMb, Cross_val_df$Lag[Cross_val_df_before$LCC== "heaMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[7,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[7,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[7,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

# MIDMID AFTER

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "conMMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "conMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conMMs", var.x = c("SPD", "clim")) # clim sig
com = custom_commonality_analysis(conMMa, SPD.MMa, clim.MMa, Cross_val_df$Lag[Cross_val_df_after$LCC== "conMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[1,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[1,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[1,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "decMMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "decMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decMMs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(decMMa, SPD.MMa, clim.MMa, Cross_val_df$Lag[Cross_val_df_after$LCC== "decMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[2,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[2,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[2,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "wetwMMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetwMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMMs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(wetwMMa, SPD.MMa, clim.MMa, Cross_val_df$Lag[Cross_val_df_after$LCC== "wetwMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[3,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[3,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[3,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "wetmMMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetmMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMMs", var.x = c("SPD", "clim")) # SPD sig
com = custom_commonality_analysis(wetmMMa, SPD.MMa, clim.MMa, Cross_val_df$Lag[Cross_val_df_after$LCC== "wetmMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[4,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[4,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[4,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "pasMMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "pasMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMMs", var.x = c("SPD", "clim")) # SPD/all sig
com = custom_commonality_analysis(pasMMa, SPD.MMa, clim.MMa, Cross_val_df$Lag[Cross_val_df_after$LCC== "pasMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[5,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[5,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[5,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "araMMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "araMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araMMs", var.x = c("SPD", "clim")) # SPD/all sig
com = custom_commonality_analysis(araMMa, SPD.MMa, clim.MMa, Cross_val_df$Lag[Cross_val_df_after$LCC== "araMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[6,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[6,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[6,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "heaMMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "heaMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMMs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(heaMMa, SPD.MMa, clim.MMa, Cross_val_df$Lag[Cross_val_df_after$LCC== "heaMMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[7,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[7,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[7,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

write.csv(as.data.frame(t(coefs)), "./Results/Commonality_analysis/Com_analysis_MIDMID_abun.csv", row.names = T)

### SOUTHWEST ### ----
# SOUTHWEST ALL
rm(coefs)
coefs <- data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("SPD", "Climate", "All","SPD_before", "Climate_before", 
                                                                "All_before","SPD_after", "Climate_after", "All_after"))))

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "conSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(conSWs, SPD.SW, clim.SW, Cross_val_df$Lag[Cross_val_df$LCC== "conSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Coniferous woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "decSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSWs", var.x = c("SPD", "clim")) # SPD/clim/all sig
com = custom_commonality_analysis(decSWs, SPD.SW, clim.SW, Cross_val_df$Lag[Cross_val_df$LCC== "decSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Deciduous woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "wetwSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetwSWs, SPD.SW, clim.SW, Cross_val_df$Lag[Cross_val_df$LCC== "wetwSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Wet woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "wetmSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetmSWs, SPD.SW, clim.SW, Cross_val_df$Lag[Cross_val_df$LCC== "wetmSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Wet meadow"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "pasSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(pasSWs, SPD.SW, clim.SW, Cross_val_df$Lag[Cross_val_df$LCC== "pasSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Pasture"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "araSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSWs", var.x = c("SPD", "clim")) # SPD/all sig
com = custom_commonality_analysis(araSWs, SPD.SW, clim.SW, Cross_val_df$Lag[Cross_val_df$LCC== "araSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Arable land"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "heaSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(heaSWs, SPD.SW, clim.SW, Cross_val_df$Lag[Cross_val_df$LCC== "heaSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Heath"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

# SOUTHWEST BEFORE
vm <- vars::VAR(alldataSWb[c("SPD", "clim", "conSWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "conSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSWs", var.x = c("SPD", "clim")) #all sig
com = custom_commonality_analysis(conSWb, SPD.SWb, clim.SWb, Cross_val_df$Lag[Cross_val_df_before$LCC== "conSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[1,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[1,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[1,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "decSWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "decSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(decSWb, SPD.SWb, clim.SWb, Cross_val_df$Lag[Cross_val_df_before$LCC== "decSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[2,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[2,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[2,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "wetwSWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetwSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetwSWb, SPD.SWb, clim.SWb, Cross_val_df$Lag[Cross_val_df_before$LCC== "wetwSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[3,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[3,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[3,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "wetmSWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetmSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetmSWb, SPD.SWb, clim.SWb, Cross_val_df$Lag[Cross_val_df_before$LCC== "wetmSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[4,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[4,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[4,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "pasSWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "pasSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(pasSWb, SPD.SWb, clim.SWb, Cross_val_df$Lag[Cross_val_df_before$LCC== "pasSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[5,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[5,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[5,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "araSWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "araSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(araSWb, SPD.SWb, clim.SWb, Cross_val_df$Lag[Cross_val_df_before$LCC== "araSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[6,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[6,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[6,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "heaSWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "heaSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSWs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(heaSWb, SPD.SWb, clim.SWb, Cross_val_df$Lag[Cross_val_df_before$LCC== "heaSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[7,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[7,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[7,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

# SOUTHWEST AFTER

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "conSWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "conSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSWs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(conSWa, SPD.SWa, clim.SWa, Cross_val_df$Lag[Cross_val_df_after$LCC== "conSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[1,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[1,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[1,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "decSWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "decSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSWs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(decSWa, SPD.SWa, clim.SWa, Cross_val_df$Lag[Cross_val_df_after$LCC== "decSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[2,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[2,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[2,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "wetwSWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetwSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSWs", var.x = c("SPD", "clim")) # SPD/all sig
com = custom_commonality_analysis(wetwSWa, SPD.SWa, clim.SWa, Cross_val_df$Lag[Cross_val_df_after$LCC== "wetwSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[3,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[3,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[3,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "wetmSWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetmSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSWs", var.x = c("SPD", "clim")) # clim/all sig
com = custom_commonality_analysis(wetmSWa, SPD.SWa, clim.SWa, Cross_val_df$Lag[Cross_val_df_after$LCC== "wetmSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[4,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[4,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[4,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "pasSWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "pasSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSWs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(pasSWa, SPD.SWa, clim.SWa, Cross_val_df$Lag[Cross_val_df_after$LCC== "pasSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[5,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[5,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[5,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "araSWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "araSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSWs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(araSWa, SPD.SWa, clim.SWa, Cross_val_df$Lag[Cross_val_df_after$LCC== "araSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[6,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[6,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[6,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "heaSWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "heaSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSWs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(heaSWa, SPD.SWa, clim.SWa, Cross_val_df$Lag[Cross_val_df_after$LCC== "heaSWs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[7,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[7,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[7,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

write.csv(as.data.frame(t(coefs)), "./Results/Commonality_analysis/Com_analysis_SOUTHWEST_abun.csv", row.names = T)

### SOUTHMID ### ----
# SOUTHMID ALL
rm(coefs)
coefs <- data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("SPD", "Climate", "All","SPD_before", "Climate_before", 
                                                                "All_before","SPD_after", "Climate_after", "All_after"))))

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "conSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSMs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(conSMs, SPD.SM, clim.SM, Cross_val_df$Lag[Cross_val_df$LCC== "conSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Coniferous woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "decSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSMs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(decSMs, SPD.SM, clim.SM, Cross_val_df$Lag[Cross_val_df$LCC== "decSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Deciduous woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "wetwSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSMs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetwSMs, SPD.SM, clim.SM, Cross_val_df$Lag[Cross_val_df$LCC== "wetwSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Wet woodland"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "wetmSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSMs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetmSMs, SPD.SM, clim.SM, Cross_val_df$Lag[Cross_val_df$LCC== "wetmSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Wet meadow"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "pasSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSMs", var.x = c("SPD", "clim")) #SPD/all sig
com = custom_commonality_analysis(pasSMs, SPD.SM, clim.SM, Cross_val_df$Lag[Cross_val_df$LCC== "pasSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Pasture"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "araSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSMs", var.x = c("SPD", "clim")) # SPD sig
com = custom_commonality_analysis(araSMs, SPD.SM, clim.SM, Cross_val_df$Lag[Cross_val_df$LCC== "araSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Arable land"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "heaSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSMs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(heaSMs, SPD.SM, clim.SM, Cross_val_df$Lag[Cross_val_df$LCC== "heaSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[nrow(coefs)+1,] <- NA
rownames(coefs)[rownames(coefs) == nrow(coefs)] <- "Heath"
coefs[nrow(coefs),1] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[nrow(coefs),2] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[nrow(coefs),3] <- paste0(round(com$Csc*100, 3), " ", sig_all)

# SOUTHMID BEFORE
vm <- vars::VAR(alldataSMb[c("SPD", "clim", "conSMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "conSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSMs", var.x = c("SPD", "clim")) # SPD sig
com = custom_commonality_analysis(conSMb, SPD.SMb, clim.SMb, Cross_val_df$Lag[Cross_val_df_before$LCC== "conSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[1,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[1,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[1,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "decSMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "decSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSMs", var.x = c("SPD", "clim")) # all sig
com = custom_commonality_analysis(decSMb, SPD.SMb, clim.SMb, Cross_val_df$Lag[Cross_val_df_before$LCC== "decSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[2,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[2,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[2,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "wetwSMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetwSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSMs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(wetwSMb, SPD.SMb, clim.SMb, Cross_val_df$Lag[Cross_val_df_before$LCC== "wetwSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[3,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[3,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[3,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "wetmSMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetmSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSMs", var.x = c("SPD", "clim")) # SPD/all sig
com = custom_commonality_analysis(wetmSMb, SPD.SMb, clim.SMb, Cross_val_df$Lag[Cross_val_df_before$LCC== "wetmSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[4,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[4,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[4,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "pasSMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "pasSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSMs", var.x = c("SPD", "clim")) # No sig 
com = custom_commonality_analysis(pasSMb, SPD.SMb, clim.SMb, Cross_val_df$Lag[Cross_val_df_before$LCC== "pasSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[5,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[5,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[5,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "araSMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "araSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSMs", var.x = c("SPD", "clim")) # all sig
com = custom_commonality_analysis(araSMb, SPD.SMb, clim.SMb, Cross_val_df$Lag[Cross_val_df_before$LCC== "araSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[6,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[6,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[6,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "heaSMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "heaSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSMs", var.x = c("SPD", "clim")) #clim sig
com = custom_commonality_analysis(heaSMb, SPD.SMb, clim.SMb, Cross_val_df$Lag[Cross_val_df_before$LCC== "heaSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[7,4] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[7,5] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[7,6] <- paste0(round(com$Csc*100, 3), " ", sig_all)

#SOUTHMID AFTER
vm <- vars::VAR(alldataSMa[c("SPD", "clim", "conSMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "conSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSMs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(conSMa, SPD.SMa, clim.SMa, Cross_val_df$Lag[Cross_val_df_after$LCC== "conSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[1,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[1,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[1,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "decSMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "decSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSMs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(decSMa, SPD.SMa, clim.SMa, Cross_val_df$Lag[Cross_val_df_after$LCC== "decSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[2,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[2,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[2,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "wetwSMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetwSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSMs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(wetwSMa, SPD.SMa, clim.SMa, Cross_val_df$Lag[Cross_val_df_after$LCC== "wetwSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[3,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[3,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[3,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "wetmSMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetmSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSMs", var.x = c("SPD", "clim")) # SPD sig
com = custom_commonality_analysis(wetmSMa, SPD.SMa, clim.SMa, Cross_val_df$Lag[Cross_val_df_after$LCC== "wetmSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[4,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[4,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[4,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "pasSMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "pasSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSMs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(pasSMa, SPD.SMa, clim.SMa, Cross_val_df$Lag[Cross_val_df_after$LCC== "pasSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[5,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[5,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[5,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "araSMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "araSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSMs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(araSMa, SPD.SMa, clim.SMa, Cross_val_df$Lag[Cross_val_df_after$LCC== "araSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[6,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[6,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[6,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "heaSMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "heaSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSMs", var.x = c("SPD", "clim")) # no sig
com = custom_commonality_analysis(heaSMa, SPD.SMa, clim.SMa, Cross_val_df$Lag[Cross_val_df_after$LCC== "heaSMs"])

sig_SPD <- gsub(" ", '', y$result$sig.F[1])
sig_clim <- gsub(" ", '', y$result$sig.F[2])
sig_all <- gsub(" ", '', y$result$sig.F[3])

coefs[7,7] <- paste0(round(com$Us*100, 3), " ", sig_SPD)
coefs[7,8] <- paste0(round(com$Uc*100, 3), " ", sig_clim)
coefs[7,9] <- paste0(round(com$Csc*100, 3), " ", sig_all)

write.csv(as.data.frame(t(coefs)), "./Results/Commonality_analysis/Com_analysis_SOUTHMID_abun.csv", row.names = T)