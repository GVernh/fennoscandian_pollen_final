### Granger causality test ###
# Load librarird ----
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

dir.create(file.path("./", "Results"), showWarnings = FALSE)
dir.create(file.path("./Results/", "Granger_causality"), showWarnings = FALSE)
# Data ----
alldataNs <- read.csv("./Processed_data/Full_datasets/alldataNs_count.csv")
alldataMMs <- read.csv("./Processed_data/Full_datasets/alldataMMs_count.csv")
alldataMWs <- read.csv("./Processed_data/Full_datasets/alldataMWs_count.csv")
alldataSEs <- read.csv("./Processed_data/Full_datasets/alldataSEs_count.csv")
alldataSMs <- read.csv("./Processed_data/Full_datasets/alldataSMs_count.csv")
alldataSWs <- read.csv("./Processed_data/Full_datasets/alldataSWs_count.csv")

Cross_val_df <- read.csv("./Results/Cross_validation/Cross_validation_results_count.csv")

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

# multivariate Granger causality test from the bruceR package.----

### NORTH ###
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataNs[c("SPD", "clim", "conNs")], p = Cross_val_df$Lag[Cross_val_df$LCC== "conNs"]) 
y = bruceR::granger_causality(varmodel = vm, var.y = "conNs", var.x = c("SPD", "clim"))
results = as.data.frame(y$result)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "decNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "wetwNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "wetmNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "pasNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "araNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNs[c("SPD", "clim", "heaNs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaNs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)


### SOUTHEAST ###
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSEs[c("SPD", "clim", "conSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSEs", var.x = c("SPD", "clim")) #sig all
results = rbind(y$result, results)
custom_commonality_analysis(conSEs, SPD.SE, clim.SE, Cross_val_df$Lag[Cross_val_df$LCC== "conSEs"]) #all

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "decSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSEs", var.x = c("SPD", "clim")) #low for clim
results = rbind(y$result, results)
custom_commonality_analysis(decSEs, SPD.SE, clim.SE, Cross_val_df$Lag[Cross_val_df$LCC== "decSEs"]) #all

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "wetwSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "wetmSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "pasSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSEs", var.x = c("SPD", "clim")) #spd
results = rbind(y$result, results)
custom_commonality_analysis(pasSEs, SPD.SE, clim.SE, Cross_val_df$Lag[Cross_val_df$LCC== "pasSEs"])

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "araSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSEs", var.x = c("SPD", "clim")) #low spd
results = rbind(y$result, results)
custom_commonality_analysis(araSEs, SPD.SE, clim.SE, Cross_val_df$Lag[Cross_val_df$LCC== "araSEs"])

vm <- vars::VAR(alldataSEs[c("SPD", "clim", "heaSEs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaSEs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)
custom_commonality_analysis(heaSEs, SPD.SE, clim.SE, Cross_val_df$Lag[Cross_val_df$LCC== "heaSEs"])


### MIDWEST ###
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataMWs[c("SPD", "clim", "conMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conMWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "decMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decMWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "wetwMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMWs", var.x = c("SPD", "clim")) #clim
results = rbind(y$result, results)
custom_commonality_analysis(wetwMWs, SPD.MW, clim.MW, Cross_val_df$Lag[Cross_val_df$LCC== "wetwMWs"])

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "wetmMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "pasMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMWs", var.x = c("SPD", "clim")) #low for spd
results = rbind(y$result, results)
custom_commonality_analysis(pasMWs, SPD.MW, clim.MW, Cross_val_df$Lag[Cross_val_df$LCC== "pasMWs"])

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "araMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araMWs", var.x = c("SPD", "clim")) #sig both
results = rbind(y$result, results)
custom_commonality_analysis(araMWs, SPD.MW, clim.MW, Cross_val_df$Lag[Cross_val_df$LCC== "araMWs"]) 

vm <- vars::VAR(alldataMWs[c("SPD", "clim", "heaMWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaMWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMWs", var.x = c("SPD", "clim")) #low for both
results = rbind(y$result, results)
custom_commonality_analysis(heaMWs, SPD.MW, clim.MW, Cross_val_df$Lag[Cross_val_df$LCC== "heaMWs"]) 

### MIDMID ###
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataMMs[c("SPD", "clim", "conMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conMMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "decMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decMMs", var.x = c("SPD", "clim")) #sig spd
results = rbind(y$result, results)
custom_commonality_analysis(decMMs, SPD.MM, clim.MM, Cross_val_df$Lag[Cross_val_df$LCC== "decMMs"]) 

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "wetwMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMMs", var.x = c("SPD", "clim")) #low spd
results = rbind(y$result, results)
custom_commonality_analysis(wetwMMs, SPD.MM, clim.MM, Cross_val_df$Lag[Cross_val_df$LCC== "wetwMMs"]) 

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "wetmMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMMs", var.x = c("SPD", "clim")) #spd
results = rbind(y$result, results)
custom_commonality_analysis(wetmMMs, SPD.MM, clim.MM, Cross_val_df$Lag[Cross_val_df$LCC== "wetmMMs"]) 

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "pasMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMMs", var.x = c("SPD", "clim")) #sig spd
results = rbind(y$result, results)
custom_commonality_analysis(pasMMs, SPD.MM, clim.MM, Cross_val_df$Lag[Cross_val_df$LCC== "pasMMs"])

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "araMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araMMs", var.x = c("SPD", "clim")) #sig spd
results = rbind(y$result, results)
custom_commonality_analysis(araMMs, SPD.MM, clim.MM, Cross_val_df$Lag[Cross_val_df$LCC== "araMMs"])

vm <- vars::VAR(alldataMMs[c("SPD", "clim", "heaMMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaMMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMMs", var.x = c("SPD", "clim")) #sig spd
results = rbind(y$result, results)
custom_commonality_analysis(heaMMs, SPD.MM, clim.MM, Cross_val_df$Lag[Cross_val_df$LCC== "heaMMs"])


### SOUTHWEST ###
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSWs[c("SPD", "clim", "conSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSWs", var.x = c("SPD", "clim")) #low spd and clim
results = rbind(y$result, results)
custom_commonality_analysis(conSWs, SPD.SW, clim.SW, Cross_val_df$Lag[Cross_val_df$LCC== "conSWs"])

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "decSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSWs", var.x = c("SPD", "clim")) #sig all
results = rbind(y$result, results)
custom_commonality_analysis(decSWs, SPD.SW, clim.SW, Cross_val_df$Lag[Cross_val_df$LCC== "decSWs"])

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "wetwSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSWs", var.x = c("SPD", "clim")) #low all
results = rbind(y$result, results)
custom_commonality_analysis(wetwSWs, SPD.SW, clim.SW, Cross_val_df$Lag[Cross_val_df$LCC== "wetwSWs"])

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "wetmSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "pasSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSWs", var.x = c("SPD", "clim")) #low both
results = rbind(y$result, results)
custom_commonality_analysis(pasSWs, SPD.SW, clim.SW, Cross_val_df$Lag[Cross_val_df$LCC== "pasSWs"])

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "araSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSWs", var.x = c("SPD", "clim")) #sig spd
results = rbind(y$result, results)
custom_commonality_analysis(araSWs, SPD.SW, clim.SW, Cross_val_df$Lag[Cross_val_df$LCC== "araSWs"])

vm <- vars::VAR(alldataSWs[c("SPD", "clim", "heaSWs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaSWs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

### SOUTHMID ###
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSMs[c("SPD", "clim", "conSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "conSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "conSMs", var.x = c("SPD", "clim")) #spd
results = rbind(y$result, results)
custom_commonality_analysis(conSMs, SPD.SM, clim.SM, Cross_val_df$Lag[Cross_val_df$LCC== "conSMs"])

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "decSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "decSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "decSMs", var.x = c("SPD", "clim")) #spd
results = rbind(y$result, results)
custom_commonality_analysis(decSMs, SPD.SM, clim.SM, Cross_val_df$Lag[Cross_val_df$LCC== "decSMs"])

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "wetwSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetwSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "wetmSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "wetmSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSMs", var.x = c("SPD", "clim")) #spd
results = rbind(y$result, results)
custom_commonality_analysis(wetmSMs, SPD.SM, clim.SM, Cross_val_df$Lag[Cross_val_df$LCC== "wetmSMs"])

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "pasSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "pasSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "araSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "araSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "araSMs", var.x = c("SPD", "clim")) #spd
results = rbind(y$result, results)
custom_commonality_analysis(araSMs, SPD.SM, clim.SM, Cross_val_df$Lag[Cross_val_df$LCC== "araSMs"])

vm <- vars::VAR(alldataSMs[c("SPD", "clim", "heaSMs")], p=Cross_val_df$Lag[Cross_val_df$LCC== "heaSMs"])
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

write.csv(results, "./Results/Granger_causality/Granger_results_allData_count.csv", row.names = F)
rm(results)
### BEFORE THE ONSET OF FARMING #-----------------------------------------------

### NORTH ### before 2300 BP (9000 - 2400 BP)
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataNb[c("SPD", "clim", "conNs")], p=2) 
y = bruceR::granger_causality(varmodel = vm, var.y = "conNs", var.x = c("SPD", "clim"))
results = as.data.frame(y$result)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "decNs")], p=2)
y = bruceR::granger_causality(varmodel = vm, var.y = "decNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "wetwNs")], p=1)
y = granger_causality(varmodel = vm, var.y = "wetwNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "wetmNs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "pasNs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "pasNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "araNs")], p=2)
y = bruceR::granger_causality(varmodel = vm, var.y = "araNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNb[c("SPD", "clim", "heaNs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "heaNs", var.x = c("SPD", "clim")) # clim
results = rbind(y$result, results)
custom_commonality_analysis(heaNb, SPD.Nb, clim.Nb, 1)

### SOUTHEAST ### 4000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSEb[c("SPD", "clim", "conSEs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "conSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "decSEs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "decSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)
custom_commonality_analysis(decSEb, SPD.SEb, clim.SEb, 1)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "wetwSEs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "wetmSEs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "pasSEs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "araSEs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "araSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEb[c("SPD", "clim", "heaSEs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)


### MIDWEST ### 3000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataMWb[c("SPD", "clim", "conMWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "conMWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "decMWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "decMWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "wetwMWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)
custom_commonality_analysis(wetwMWb, SPD.MWb, clim.MWb, 1)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "wetmMWs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMWs", var.x = c("SPD", "clim")) # sig spd
results = rbind(y$result, results)
custom_commonality_analysis(wetmMWb, SPD.MWb, clim.MWb, 3)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "pasMWs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "araMWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "araMWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWb[c("SPD", "clim", "heaMWs")], p=2)
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMWs", var.x = c("SPD", "clim")) #clim
results = rbind(y$result, results)
custom_commonality_analysis(heaMWb, SPD.MWb, clim.MWb, 2)

### MIDMID ### 3000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataMMb[c("SPD", "clim", "conMMs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "conMMs", var.x = c("SPD", "clim")) #sig all
results = rbind(y$result, results)
custom_commonality_analysis(conMMb, SPD.MMb, clim.MMb, 1)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "decMMs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "decMMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "wetwMMs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)
custom_commonality_analysis(wetwMMb, SPD.MMb, clim.MMb, 1)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "wetmMMs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "pasMMs")], p=2)
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "araMMs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "araMMs", var.x = c("SPD", "clim")) # clim
results = rbind(y$result, results)
custom_commonality_analysis(araMMb, SPD.MMb, clim.MMb, 1)

vm <- vars::VAR(alldataMMb[c("SPD", "clim", "heaMMs")], p=2)
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)


### SOUTHWEST ### (6000 BP)
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSWb[c("SPD", "clim", "conSWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "conSWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)
custom_commonality_analysis(conSWb, SPD.SWb, clim.SWb, 1)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "decSWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "decSWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)
custom_commonality_analysis(decSWb, SPD.SWb, clim.SWb, 1)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "wetwSWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "wetmSWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "pasSWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "araSWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "araSWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWb[c("SPD", "clim", "heaSWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)


### SOUTHMID ### (600 BP)
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSMb[c("SPD", "clim", "conSMs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "conSMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "decSMs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "decSMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "wetwSMs")], p=2)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSMs", var.x = c("SPD", "clim")) #clim
results = rbind(y$result, results)
custom_commonality_analysis(wetwSMb, SPD.SMb, clim.SMb, Cross_val_df$Lag[Cross_val_df$LCC== "wetwSMs"])

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "wetmSMs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSMs", var.x = c("SPD", "clim")) #spd
results = rbind(y$result, results)
custom_commonality_analysis(wetmSMb, SPD.SMb, clim.SMb, 3)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "pasSMs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSMs", var.x = c("SPD", "clim")) #both
results = rbind(y$result, results)
custom_commonality_analysis(pasSMb, SPD.SMb, clim.SMb, 3)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "araSMs")], p=2)
y = bruceR::granger_causality(varmodel = vm, var.y = "araSMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMb[c("SPD", "clim", "heaSMs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

write.csv(results, "./Results/Granger_causality/Granger_results_beforeFarming_count.csv", row.names = F)
rm(results)

### AFTER THE ONSET OF FARMING #------------------------------------------------

### NORTH ### (2300 - 1400 BP) DATASET TOO SHORT !!!
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataNa[c("SPD", "clim", "conNs")], p=4) 
y = bruceR::granger_causality(varmodel = vm, var.y = "conNs", var.x = c("SPD", "clim"))
results = as.data.frame(y$result)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "decNs")], p=4)
y = bruceR::granger_causality(varmodel = vm, var.y = "decNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "wetwNs")], p=4)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "wetmNs")], p=4)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "pasNs")], p=4)
y = bruceR::granger_causality(varmodel = vm, var.y = "pasNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "araNs")], p=4)
y = bruceR::granger_causality(varmodel = vm, var.y = "araNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataNa[c("SPD", "clim", "heaNs")], p=4)
y = bruceR::granger_causality(varmodel = vm, var.y = "heaNs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

### SOUTHEAST ### 4000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSEa[c("SPD", "clim", "conSEs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "conSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)
custom_commonality_analysis(conSEa, SPD.SEa, clim.SEa, 3)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "decSEs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "decSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "wetwSEs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "wetmSEs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "pasSEs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)
custom_commonality_analysis(pasSEa, SPD.SEa, clim.SEa, 1)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "araSEs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "araSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSEa[c("SPD", "clim", "heaSEs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSEs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)
custom_commonality_analysis(pasSEa, SPD.SEa, clim.SEa, 1)


### MIDWEST ### 3000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataMWa[c("SPD", "clim", "conMWs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "conMWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "decMWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "decMWs", var.x = c("SPD", "clim")) #spd
results = rbind(y$result, results)
custom_commonality_analysis(decMWa, SPD.MWa, clim.MWa, 1)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "wetwMWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMWs", var.x = c("SPD", "clim")) #sig spd
results = rbind(y$result, results)
custom_commonality_analysis(wetwMWa, SPD.MWa, clim.MWa, 1)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "wetmMWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMWs", var.x = c("SPD", "clim")) # sig all
results = rbind(y$result, results)
custom_commonality_analysis(wetmMWa, SPD.MWa, clim.MWa, 1) 

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "pasMWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "araMWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "araMWs", var.x = c("SPD", "clim")) #sig spd
results = rbind(y$result, results)
custom_commonality_analysis(araMWa, SPD.MWa, clim.MWa, 1)

vm <- vars::VAR(alldataMWa[c("SPD", "clim", "heaMWs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

### MIDMID ### 3000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataMMa[c("SPD", "clim", "conMMs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "conMMs", var.x = c("SPD", "clim")) #clim
results = rbind(y$result, results)
custom_commonality_analysis(conMMa, SPD.MMa, clim.MMa, 1)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "decMMs")], p=2)
y = bruceR::granger_causality(varmodel = vm, var.y = "decMMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "wetwMMs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwMMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "wetmMMs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmMMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "pasMMs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "pasMMs", var.x = c("SPD", "clim")) #sig spd
results = rbind(y$result, results)
custom_commonality_analysis(pasMMa, SPD.MMa, clim.MMa, 1)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "araMMs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "araMMs", var.x = c("SPD", "clim")) #sig spd
results = rbind(y$result, results)
custom_commonality_analysis(araMMa, SPD.MMa, clim.MMa, 1)

vm <- vars::VAR(alldataMMa[c("SPD", "clim", "heaMMs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "heaMMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)
custom_commonality_analysis(heaMMa, SPD.MMa, clim.MMa, 1)


### SOUTHWEST ### 6000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSWa[c("SPD", "clim", "conSWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "conSWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "decSWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "decSWs", var.x = c("SPD", "clim")) #sig spd
results = rbind(y$result, results)
custom_commonality_analysis(decSWa, SPD.SWa, clim.SWa, 1)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "wetwSWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSWs", var.x = c("SPD", "clim")) #sig spd
results = rbind(y$result, results)
custom_commonality_analysis(wetwSWa, SPD.SWa, clim.SWa, 1)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "wetmSWs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSWs", var.x = c("SPD", "clim")) #all
results = rbind(y$result, results)
custom_commonality_analysis(wetmSWa, SPD.SWa, clim.SWa, 1)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "pasSWs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSWs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "araSWs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "araSWs", var.x = c("SPD", "clim")) #
results = rbind(y$result, results)
custom_commonality_analysis(heaSWa, SPD.SWa, clim.SWa, 3)

vm <- vars::VAR(alldataSWa[c("SPD", "clim", "heaSWs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSWs", var.x = c("SPD", "clim")) #sig all
results = rbind(y$result, results)


### SOUTHMID ### 6000 BP
#-------------------------------------------------------------------------------
vm <- vars::VAR(alldataSMa[c("SPD", "clim", "conSMs")], p=3)
y = bruceR::granger_causality(varmodel = vm, var.y = "conSMs", var.x = c("SPD", "clim")) #sig spd
results = rbind(y$result, results)
custom_commonality_analysis(conSMa, SPD.SMa, clim.SMa, 3)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "decSMs")], p=2)
y = bruceR::granger_causality(varmodel = vm, var.y = "decSMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "wetwSMs")], p=1)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetwSMs", var.x = c("SPD", "clim")) #clim
results = rbind(y$result, results)
custom_commonality_analysis(wetwSMa, SPD.SMa, clim.SMa, 1)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "wetmSMs")], p=2)
y = bruceR::granger_causality(varmodel = vm, var.y = "wetmSMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "pasSMs")], p=2)
y = bruceR::granger_causality(varmodel = vm, var.y = "pasSMs", var.x = c("SPD", "clim")) #clim
results = rbind(y$result, results)
custom_commonality_analysis(pasSMa, SPD.SMa, clim.SMa, 2)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "araSMs")], p=2)
y = bruceR::granger_causality(varmodel = vm, var.y = "araSMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

vm <- vars::VAR(alldataSMa[c("SPD", "clim", "heaSMs")], p=2)
y = bruceR::granger_causality(varmodel = vm, var.y = "heaSMs", var.x = c("SPD", "clim"))
results = rbind(y$result, results)

write.csv(results, "./Results/Granger_causality/Granger_results_afterFarming_count.csv", row.names = F)
rm(results)