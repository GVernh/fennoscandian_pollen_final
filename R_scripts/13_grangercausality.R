### Granger causality test ###

# TO DO:
  # May be able to create a nested loop for each category (before farming, after farming and all data)
  # Break key words into two parts with LCC as inner loop and area as outer loops (e.g. "con" and "Ns")


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
dir.create(file.path("./Results/", "Granger_causality"), showWarnings = FALSE)

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

# multivariate Granger causality test from the bruceR package

### NORTH ###
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataNs[c("SPD", "clim", "conNs")], p = Cross_val_df$Lag[Cross_val_df$LCC== "conNs"]) 
y = bruceR::granger_causality(varmodel = vm, var.y = "conNs", var.x = c("SPD", "clim")) # No sig
results = as.data.frame(y$result)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "decNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decNs", var.x = c("SPD", "clim")) # No sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "wetwNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwNs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "wetmNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmNs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "pasNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasNs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "araNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araNs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "heaNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaNs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

### SOUTHEAST ###
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSEs[c("SPD", "clim", "conSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSEs", var.x = c("SPD", "clim")) # sig all/clim
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "decSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSEs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "wetwSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSEs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "wetmSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSEs", var.x = c("SPD", "clim")) # Clim/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "pasSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSEs", var.x = c("SPD", "clim")) # spd/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "araSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSEs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "heaSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSEs", var.x = c("SPD", "clim")) # SPD/all sig
results = rbind(y$result, results)

### MIDWEST ###
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataMWs[c("SPD", "clim", "conMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conMWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "decMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decMWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "wetwMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "wetmMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "pasMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMWs", var.x = c("SPD", "clim")) # SPD/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "araMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araMWs", var.x = c("SPD", "clim")) # all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "heaMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

### MIDMID ###
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataMMs[c("SPD", "clim", "conMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conMMs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "decMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decMMs", var.x = c("SPD", "clim")) # SPD/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "wetwMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMMs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "wetmMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMMs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "pasMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMMs", var.x = c("SPD", "clim")) # SPD/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "araMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araMMs", var.x = c("SPD", "clim")) # SPD/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "heaMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMMs", var.x = c("SPD", "clim")) # all sig
results = rbind(y$result, results)

### SOUTHWEST ###
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSWs[c("SPD", "clim", "conSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "decSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSWs", var.x = c("SPD", "clim")) # SPD/clim/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "wetwSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "wetmSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "pasSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "araSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSWs", var.x = c("SPD", "clim")) # SPD/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "heaSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

### SOUTHMID ###
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSMs[c("SPD", "clim", "conSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSMs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "decSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSMs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "wetwSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSMs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "wetmSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSMs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "pasSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSMs", var.x = c("SPD", "clim")) #SPD/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "araSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSMs", var.x = c("SPD", "clim")) # SPD sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "heaSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSMs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

write.csv(results, "./Results/Granger_causality/Granger_results_allData_abun.csv", row.names = F)
rm(results)
### BEFORE THE ONSET OF FARMING #-----------------------------------------------

### NORTH ### before 2300 BP (9000 - 2400 BP)
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataNb[c("SPD", "clim", "conNs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "conNs"]) 
y = bruceR::granger_causality(varmodel = vm, var.y = "conNs", var.x = c("SPD", "clim")) # No sig 
results = as.data.frame(y$result)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "decNs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "decNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decNs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "wetwNs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetwNs"])
y = granger_causality(varmodel = vm, var.y = "wetwNs", var.x = c("SPD", "clim")) # All sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "wetmNs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetmNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmNs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "pasNs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "pasNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasNs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "araNs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "araNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araNs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "heaNs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "heaNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaNs", var.x = c("SPD", "clim")) # SPD/clim/all sig
results = rbind(y$result, results)

### SOUTHEAST ### 4000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSEb[c("SPD", "clim", "conSEs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "conSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSEs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "decSEs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "decSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSEs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "wetwSEs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetwSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSEs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "wetmSEs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetmSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSEs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "pasSEs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "pasSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSEs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "araSEs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "araSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSEs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "heaSEs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "heaSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSEs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

### MIDWEST ### 3000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataMWb[c("SPD", "clim", "conMWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "conMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conMWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "decMWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "decMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decMWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "wetwMWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetwMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMWs", var.x = c("SPD", "clim")) # clim sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "wetmMWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetmMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMWs", var.x = c("SPD", "clim")) # SPD sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "pasMWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "pasMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "araMWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "araMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araMWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "heaMWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "heaMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMWs", var.x = c("SPD", "clim")) # clim/all sig
results = rbind(y$result, results)

### MIDMID ### 3000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataMMb[c("SPD", "clim", "conMMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "conMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conMMs", var.x = c("SPD", "clim")) # SPD/clim/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "decMMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "decMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decMMs", var.x = c("SPD", "clim")) # SPD sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "wetwMMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetwMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMMs", var.x = c("SPD", "clim")) # all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "wetmMMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetmMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMMs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "pasMMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "pasMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMMs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "araMMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "araMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araMMs", var.x = c("SPD", "clim")) # clim sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "heaMMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "heaMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMMs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

### SOUTHWEST ### (6000 BP)
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSWb[c("SPD", "clim", "conSWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "conSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSWs", var.x = c("SPD", "clim")) #all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "decSWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "decSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "wetwSWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetwSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "wetmSWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetmSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "pasSWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "pasSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "araSWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "araSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results) 

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "heaSWs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "heaSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSWs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

### SOUTHMID ### (600 BP)
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSMb[c("SPD", "clim", "conSMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "conSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSMs", var.x = c("SPD", "clim")) # SPD sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "decSMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "decSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSMs", var.x = c("SPD", "clim")) # all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "wetwSMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetwSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSMs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "wetmSMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "wetmSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSMs", var.x = c("SPD", "clim")) # SPD/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "pasSMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "pasSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSMs", var.x = c("SPD", "clim")) # No sig 
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "araSMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "araSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSMs", var.x = c("SPD", "clim")) # all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "heaSMs")], p=Cross_val_df_before$Lag[Cross_val_df_before$LCC== "heaSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSMs", var.x = c("SPD", "clim")) #clim sig
results = rbind(y$result, results)

write.csv(results, "./Results/Granger_causality/Granger_results_beforeFarming_abun.csv", row.names = F)
rm(results)

### AFTER THE ONSET OF FARMING #------------------------------------------------

### NORTH ### (2300 - 1400 BP) DATASET TOO SHORT !!!
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataNa[c("SPD", "clim", "conNs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "conNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conNs", var.x = c("SPD", "clim")) # NOT ENOUGH DATA
results = as.data.frame(y$result)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "decNs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "decNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decNs", var.x = c("SPD", "clim")) # NOT ENOUGH DATA
results = rbind(y$result, results)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "wetwNs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetwNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwNs", var.x = c("SPD", "clim")) # NOT ENOUGH DATA
results = rbind(y$result, results)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "wetmNs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetmNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmNs", var.x = c("SPD", "clim")) # NOT ENOUGH DATA
results = rbind(y$result, results)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "pasNs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "pasNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasNs", var.x = c("SPD", "clim")) # NOT ENOUGH DATA
results = rbind(y$result, results)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "araNs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "araNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araNs", var.x = c("SPD", "clim")) # NOT ENOUGH DATA
results = rbind(y$result, results)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "heaNs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "heaNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaNs", var.x = c("SPD", "clim")) # NOT ENOUGH DATA
results = rbind(y$result, results)

### SOUTHEAST ### 4000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSEa[c("SPD", "clim", "conSEs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "conSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSEs", var.x = c("SPD", "clim")) # clim sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "decSEs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "decSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSEs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "wetwSEs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetwSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSEs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "wetmSEs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetmSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSEs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "pasSEs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "pasSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSEs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "araSEs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "araSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSEs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "heaSEs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "heaSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSEs", var.x = c("SPD", "clim")) # SPD/all sig
results = rbind(y$result, results)

### MIDWEST ### 3000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataMWa[c("SPD", "clim", "conMWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "conMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conMWs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "decMWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "decMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decMWs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "wetwMWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetwMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMWs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "wetmMWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetmMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMWs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "pasMWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "pasMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMWs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "araMWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "araMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araMWs", var.x = c("SPD", "clim")) # SPD sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "heaMWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "heaMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMWs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

### MIDMID ### 3000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataMMa[c("SPD", "clim", "conMMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "conMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conMMs", var.x = c("SPD", "clim")) # clim sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "decMMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "decMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decMMs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "wetwMMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetwMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMMs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "wetmMMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetmMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMMs", var.x = c("SPD", "clim")) # SPD sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "pasMMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "pasMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMMs", var.x = c("SPD", "clim")) # SPD/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "araMMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "araMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araMMs", var.x = c("SPD", "clim")) # SPD/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "heaMMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "heaMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMMs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

### SOUTHWEST ### 6000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSWa[c("SPD", "clim", "conSWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "conSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSWs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "decSWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "decSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSWs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "wetwSWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetwSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSWs", var.x = c("SPD", "clim")) # SPD/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "wetmSWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetmSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSWs", var.x = c("SPD", "clim")) # clim/all sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "pasSWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "pasSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSWs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "araSWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "araSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSWs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "heaSWs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "heaSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSWs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

### SOUTHMID ### 6000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSMa[c("SPD", "clim", "conSMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "conSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSMs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "decSMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "decSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSMs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "wetwSMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetwSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSMs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)
custom_commonality_analysis(wetwSMa, SPD.SMa, clim.SMa, 1)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "wetmSMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "wetmSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSMs", var.x = c("SPD", "clim")) # SPD sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "pasSMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "pasSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSMs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "araSMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "araSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSMs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "heaSMs")], p=Cross_val_df_after$Lag[Cross_val_df_after$LCC== "heaSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSMs", var.x = c("SPD", "clim")) # no sig
results = rbind(y$result, results)

write.csv(results, "./Results/Granger_causality/Granger_results_afterFarming_abun.csv", row.names = F)
rm(results)