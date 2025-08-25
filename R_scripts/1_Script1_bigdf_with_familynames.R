### bigdf_with_familynames ###

# – Setting up libraries ####
#–––––––––––––––––––––––––––––––––––––––––––––––––
libs <- c("stringr","tidyr", "dplyr")

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

if (!(paste0("p3k14c_original.csv") %in% list.files("./Raw_Data/"))) {
zipF<- "./Raw_Data/p3k14c_original.csv.zip"
outDir<-"./Raw_Data/"
unzip(zipF,exdir=outDir)
}

# Source functions
source("./Functions/MakestandardDF.R")

## Load temporal and pollen data
All_Age_Depth_Curves = readRDS(file = "./Raw_Data/All_Age_Depth_Curves_2021-11-23") 
pollen_all = readRDS(file = "./Raw_Data/KMH_S1_pollen_all_2021-11-22")
#pollen_all_copy = readRDS(file = "./Output/KMH_S1_pollen_all_2021-11-22 copy with original taxon names")

# Merge time and pollen data to single data frame
bigdf = NULL
for(name in names(All_Age_Depth_Curves))
{
  namedf = makeStandardDF(All_Age_Depth_Curves, pollen_all, name)
  if(name=="12"){
    bigdf = namedf
  }
  else{
    bigdf = dplyr::bind_rows(bigdf, namedf)
  }
}

### Sort ###
bigdf <- bigdf %>% dplyr::select(dataset_ID, meantimes, everything())


### clean bigdf ###
attach(bigdf)

#Alnus#
aa <- apply(is.na(bigdf[,c("Alnus..shrub..pale.shallow.pores.", "Alnus..tree..robust.deep.pores.","Alnus")]),1,all)
f_sum <- rowSums(bigdf[,c("Alnus..tree..robust.deep.pores.", "Alnus..shrub..pale.shallow.pores.","Alnus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Alnus..tree..robust.deep.pores.", "Alnus..shrub..pale.shallow.pores.","Alnus")])
Alnus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Alnus_sum[i] = t_sum[i]
  }
  else{
    Alnus_sum[i] = f_sum[i]
  }
}
bigdf$Alnus <- Alnus_sum
bigdf <- subset(bigdf, select = -c(Alnus..tree..robust.deep.pores., Alnus..shrub..pale.shallow.pores.))

#Abies
aa <- apply(is.na(bigdf[,c("Abies", "Abies.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Abies", "Abies.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Abies", "Abies.undiff.")])
Abies_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Abies_sum[i] = t_sum[i]
  }
  else{
    Abies_sum[i] = f_sum[i]
  }
}
bigdf$Abies <- Abies_sum
bigdf <- subset(bigdf, select = -c(Abies.undiff.))

#Achillea#
aa <- apply(is.na(bigdf[,c("Achillea", "Achillea.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Achillea", "Achillea.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Achillea", "Achillea.type")])
Achillea_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Achillea_sum[i] = t_sum[i]
  }
  else{
    Achillea_sum[i] = f_sum[i]
  }
}
bigdf$Achillea <- Achillea_sum
bigdf <- subset(bigdf, select = -c(Achillea.type))

#Allium
aa <- apply(is.na(bigdf[,c("Allium", "Allium.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Allium", "Allium.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Allium", "Allium.type")])
Allium_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Allium_sum[i] = t_sum[i]
  }
  else{
    Allium_sum[i] = f_sum[i]
  }
}
bigdf$Allium <- Allium_sum
bigdf <- subset(bigdf, select = -c(Allium.type))

#Anagallis arvensis
aa <- apply(is.na(bigdf[,c("Anagallis.arvensis", "Anagallis.cf..A..arvensis")]),1,all)
f_sum <- rowSums(bigdf[,c("Anagallis.arvensis", "Anagallis.cf..A..arvensis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Anagallis.arvensis", "Anagallis.cf..A..arvensis")])
Anagallis.arvensis_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Anagallis.arvensis_sum[i] = t_sum[i]
  }
  else{
    Anagallis.arvensis_sum[i] = f_sum[i]
  }
}
bigdf$Anagallis.arvensis <- Anagallis.arvensis_sum
bigdf <- subset(bigdf, select = -c(Anagallis.cf..A..arvensis))

#Anemone
aa <- apply(is.na(bigdf[,c("Anemone", "Anemone.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Anemone", "Anemone.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Anemone", "Anemone.type")])
Anemone_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Anemone_sum[i] = t_sum[i]
  }
  else{
    Anemone_sum[i] = f_sum[i]
  }
}
bigdf$Anemone <- Anemone_sum
bigdf <- subset(bigdf, select = -c(Anemone.type))

#Arctium
aa <- apply(is.na(bigdf[,c("Arctium", "Arctium.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Arctium", "Arctium.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Arctium", "Arctium.type")])
Arctium_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Arctium_sum[i] = t_sum[i]
  }
  else{
    Arctium_sum[i] = f_sum[i]
  }
}
bigdf$Arctium <- Arctium_sum
bigdf <- subset(bigdf, select = -c(Arctium.type))

#Arenaria
aa <- apply(is.na(bigdf[,c("Arenaria", "Arenaria.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Arenaria", "Arenaria.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Arenaria", "Arenaria.type")])
Arenaria_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Arenaria_sum[i] = t_sum[i]
  }
  else{
    Arenaria_sum[i] = f_sum[i]
  }
}
bigdf$Arenaria <- Arenaria_sum
bigdf <- subset(bigdf, select = -c(Arenaria.type))

#Artemisia
aa <- apply(is.na(bigdf[,c("Artemisia", "Artemisia.undiff.", "Artemisia.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Artemisia", "Artemisia.undiff.", "Artemisia.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Artemisia", "Artemisia.undiff.", "Artemisia.type")])
Artemisia_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Artemisia_sum[i] = t_sum[i]
  }
  else{
    Artemisia_sum[i] = f_sum[i]
  }
}
bigdf$Artemisia <- Artemisia_sum
bigdf <- subset(bigdf, select = -c(Artemisia.undiff., Artemisia.type))

#Artemisia.norvegica
aa <- apply(is.na(bigdf[,c("Artemisia.norvegica", "Artemisia.norvegica.type", "Artemisia.cf..A..norvegica")]),1,all)
f_sum <- rowSums(bigdf[,c("Artemisia.norvegica", "Artemisia.norvegica.type", "Artemisia.cf..A..norvegica")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Artemisia.norvegica", "Artemisia.norvegica.type", "Artemisia.cf..A..norvegica")])
Artemisia.norvegica_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Artemisia.norvegica_sum[i] = t_sum[i]
  }
  else{
    Artemisia.norvegica_sum[i] = f_sum[i]
  }
}
bigdf$Artemisia.norvegica <- Artemisia.norvegica_sum
bigdf <- subset(bigdf, select = -c(Artemisia.norvegica.type, Artemisia.cf..A..norvegica))

#Aster
aa <- apply(is.na(bigdf[,c("Aster", "Aster.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Aster", "Aster.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Aster", "Aster.type")])
Aster_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Aster_sum[i] = t_sum[i]
  }
  else{
    Aster_sum[i] = f_sum[i]
  }
}
bigdf$Aster <- Aster_sum
bigdf <- subset(bigdf, select = -c(Aster.type))

#Astragalus.alpinus
aa <- apply(is.na(bigdf[,c("Astragalus.alpinus", "Astragalus.cf..A..alpinus")]),1,all)
f_sum <- rowSums(bigdf[,c("Astragalus.alpinus", "Astragalus.cf..A..alpinus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Astragalus.alpinus", "Astragalus.cf..A..alpinus")])
Astragalus.alpinus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Astragalus.alpinus_sum[i] = t_sum[i]
  }
  else{
    Astragalus.alpinus_sum[i] = f_sum[i]
  }
}
bigdf$Astragalus.alpinus <- Astragalus.alpinus_sum
bigdf <- subset(bigdf, select = -c(Astragalus.cf..A..alpinus))

#Betula
aa <- apply(is.na(bigdf[,c("Betula", "Betula..tree.", "Betula.undiff.", "Betula..hybrid.")]),1,all)
f_sum <- rowSums(bigdf[,c("Betula", "Betula..tree.", "Betula.undiff.", "Betula..hybrid.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Betula", "Betula..tree.", "Betula.undiff.", "Betula..hybrid.")])
Betula_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Betula_sum[i] = t_sum[i]
  }
  else{
    Betula_sum[i] = f_sum[i]
  }
}
bigdf$Betula <- Betula_sum
bigdf <- subset(bigdf, select = -c(Betula..tree., Betula.undiff., Betula..hybrid.))

#Betula nana
aa <- apply(is.na(bigdf[,c("Betula.nana", "Betula.nana.subsp..exilis")]),1,all)
f_sum <- rowSums(bigdf[,c("Betula.nana", "Betula.nana.subsp..exilis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Betula.nana", "Betula.nana.subsp..exilis")])
Betula.nana_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Betula.nana_sum[i] = t_sum[i]
  }
  else{
    Betula.nana_sum[i] = f_sum[i]
  }
}
bigdf$Betula.nana <- Betula.nana_sum
bigdf <- subset(bigdf, select = -c(Betula.nana.subsp..exilis))

#Betula pubescens
aa <- apply(is.na(bigdf[,c("Betula.pubescens", "Betula.pubescens.var..pumila")]),1,all)
f_sum <- rowSums(bigdf[,c("Betula.pubescens", "Betula.pubescens.var..pumila")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Betula.pubescens", "Betula.pubescens.var..pumila")])
Betula.pubescens_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Betula.pubescens_sum[i] = t_sum[i]
  }
  else{
    Betula.pubescens_sum[i] = f_sum[i]
  }
}
bigdf$Betula.pubescens <- Betula.pubescens_sum
bigdf <- subset(bigdf, select = -c(Betula.pubescens.var..pumila))

#Bistorta.officinalis
aa <- apply(is.na(bigdf[,c("Bistorta.officinalis.type", "Bistorta.officinalis")]),1,all)
f_sum <- rowSums(bigdf[,c("Bistorta.officinalis.type", "Bistorta.officinalis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Bistorta.officinalis.type", "Bistorta.officinalis")])
Bistorta.officinalis_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Bistorta.officinalis_sum[i] = t_sum[i]
  }
  else{
    Bistorta.officinalis_sum[i] = f_sum[i]
  }
}
bigdf$Bistorta.officinalis <- Bistorta.officinalis_sum
bigdf <- subset(bigdf, select = -c(Bistorta.officinalis.type))

#Botrychium.lunaria
aa <- apply(is.na(bigdf[,c("Botrychium.lunaria", "Botrychium.lunaria.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Botrychium.lunaria", "Botrychium.lunaria.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Botrychium.lunaria", "Botrychium.lunaria.type")])
Botrychium.lunaria_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Botrychium.lunaria_sum[i] = t_sum[i]
  }
  else{
    Botrychium.lunaria_sum[i] = f_sum[i]
  }
}
bigdf$Botrychium.lunaria <- Botrychium.lunaria_sum
bigdf <- subset(bigdf, select = -c(Botrychium.lunaria.type))

#Callitriche
aa <- apply(is.na(bigdf[,c("Callitriche", "cf..Callitriche")]),1,all)
f_sum <- rowSums(bigdf[,c("Callitriche", "cf..Callitriche")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Callitriche", "cf..Callitriche")])
Callitriche_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Callitriche_sum[i] = t_sum[i]
  }
  else{
    Callitriche_sum[i] = f_sum[i]
  }
}
bigdf$Callitriche <- Callitriche_sum
bigdf <- subset(bigdf, select = -c(cf..Callitriche))

#Calluna
aa <- apply(is.na(bigdf[,c("Calluna", "Ericaceae.cf..Calluna")]),1,all)
f_sum <- rowSums(bigdf[,c("Calluna", "Ericaceae.cf..Calluna")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Calluna", "Ericaceae.cf..Calluna")])
Calluna_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Calluna_sum[i] = t_sum[i]
  }
  else{
    Calluna_sum[i] = f_sum[i]
  }
}
bigdf$Calluna <- Calluna_sum
bigdf <- subset(bigdf, select = -c(Ericaceae.cf..Calluna))

#Chrysosplenium
aa <- apply(is.na(bigdf[,c("Chrysosplenium", "Chrysosplenium.type", "cf..Chrysosplenium")]),1,all)
f_sum <- rowSums(bigdf[,c("Chrysosplenium", "Chrysosplenium.type", "cf..Chrysosplenium")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Chrysosplenium", "Chrysosplenium.type", "cf..Chrysosplenium")])
Chrysosplenium_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Chrysosplenium_sum[i] = t_sum[i]
  }
  else{
    Chrysosplenium_sum[i] = f_sum[i]
  }
}
bigdf$Chrysosplenium <- Chrysosplenium_sum
bigdf <- subset(bigdf, select = -c(Chrysosplenium.type, cf..Chrysosplenium))

#Cladium.mariscus
aa <- apply(is.na(bigdf[,c("Cladium.mariscus", "cf..Cladium.mariscus")]),1,all)
f_sum <- rowSums(bigdf[,c("Cladium.mariscus", "cf..Cladium.mariscus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Cladium.mariscus", "cf..Cladium.mariscus")])
Cladium.mariscus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Cladium.mariscus_sum[i] = t_sum[i]
  }
  else{
    Cladium.mariscus_sum[i] = f_sum[i]
  }
}
bigdf$Cladium.mariscus <- Cladium.mariscus_sum
bigdf <- subset(bigdf, select = -c(cf..Cladium.mariscus))

#Elatine
aa <- apply(is.na(bigdf[,c("Elatine", "cf..Elatine")]),1,all)
f_sum <- rowSums(bigdf[,c("Elatine", "cf..Elatine")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Elatine", "cf..Elatine")])
Elatine_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Elatine_sum[i] = t_sum[i]
  }
  else{
    Elatine_sum[i] = f_sum[i]
  }
}
bigdf$Elatine <- Elatine_sum
bigdf <- subset(bigdf, select = -c(cf..Elatine))

#Empetrum
aa <- apply(is.na(bigdf[,c("Empetrum", "cf..Empetrum")]),1,all)
f_sum <- rowSums(bigdf[,c("Empetrum", "cf..Empetrum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Empetrum", "cf..Empetrum")])
Empetrum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Empetrum_sum[i] = t_sum[i]
  }
  else{
    Empetrum_sum[i] = f_sum[i]
  }
}
bigdf$Empetrum <- Empetrum_sum
bigdf <- subset(bigdf, select = -c(cf..Empetrum))

#Frangula.alnus
aa <- apply(is.na(bigdf[,c("Frangula.alnus", "cf..Frangula.alnus")]),1,all)
f_sum <- rowSums(bigdf[,c("Frangula.alnus", "cf..Frangula.alnus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Frangula.alnus", "cf..Frangula.alnus")])
Frangula.alnus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Frangula.alnus_sum[i] = t_sum[i]
  }
  else{
    Frangula.alnus_sum[i] = f_sum[i]
  }
}
bigdf$Frangula.alnus <- Frangula.alnus_sum
bigdf <- subset(bigdf, select = -c(cf..Frangula.alnus))

#Galeopsis
aa <- apply(is.na(bigdf[,c("Galeopsis.type", "cf..Galeopsis")]),1,all)
f_sum <- rowSums(bigdf[,c("Galeopsis.type", "cf..Galeopsis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Galeopsis.type", "cf..Galeopsis")])
Galeopsis_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Galeopsis_sum[i] = t_sum[i]
  }
  else{
    Galeopsis_sum[i] = f_sum[i]
  }
}
bigdf$Galeopsis <- Galeopsis_sum
bigdf <- subset(bigdf, select = -c(Galeopsis.type, cf..Galeopsis))

#Helianthemum
aa <- apply(is.na(bigdf[,c("Helianthemum", "cf..Helianthemum")]),1,all)
f_sum <- rowSums(bigdf[,c("Helianthemum", "cf..Helianthemum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Helianthemum", "cf..Helianthemum")])
Helianthemum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Helianthemum_sum[i] = t_sum[i]
  }
  else{
    Helianthemum_sum[i] = f_sum[i]
  }
}
bigdf$Helianthemum <- Helianthemum_sum
bigdf <- subset(bigdf, select = -c(cf..Helianthemum))

#Humulus.lupulus
aa <- apply(is.na(bigdf[,c("Humulus.lupulus", "cf..Humulus.lupulus")]),1,all)
f_sum <- rowSums(bigdf[,c("Humulus.lupulus", "cf..Humulus.lupulus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Humulus.lupulus", "cf..Humulus.lupulus")])
Humulus.lupulus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Humulus.lupulus_sum[i] = t_sum[i]
  }
  else{
    Humulus.lupulus_sum[i] = f_sum[i]
  }
}
bigdf$Humulus.lupulus <- Humulus.lupulus_sum
bigdf <- subset(bigdf, select = -c(cf..Humulus.lupulus))

#Hypericum
aa <- apply(is.na(bigdf[,c("Hypericum", "cf..Hypericum")]),1,all)
f_sum <- rowSums(bigdf[,c("Hypericum", "cf..Hypericum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Hypericum", "cf..Hypericum")])
Hypericum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Hypericum_sum[i] = t_sum[i]
  }
  else{
    Hypericum_sum[i] = f_sum[i]
  }
}
bigdf$Hypericum <- Hypericum_sum
bigdf <- subset(bigdf, select = -c(cf..Hypericum))

#Larix
aa <- apply(is.na(bigdf[,c("Larix", "cf..Larix.pollen.NISP", "cf..Larix.pollen.NISP.1")]),1,all)
f_sum <- rowSums(bigdf[,c("Larix", "cf..Larix.pollen.NISP", "cf..Larix.pollen.NISP.1")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Larix", "cf..Larix.pollen.NISP", "cf..Larix.pollen.NISP.1")])
Larix_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Larix_sum[i] = t_sum[i]
  }
  else{
    Larix_sum[i] = f_sum[i]
  }
}
bigdf$Larix <- Larix_sum
bigdf <- subset(bigdf, select = -c(cf..Larix.pollen.NISP, cf..Larix.pollen.NISP.1))

#Melampyrum
aa <- apply(is.na(bigdf[,c("Melampyrum", "cf..Melampyrum")]),1,all)
f_sum <- rowSums(bigdf[,c("Melampyrum", "cf..Melampyrum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Melampyrum", "cf..Melampyrum")])
Melampyrum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Melampyrum_sum[i] = t_sum[i]
  }
  else{
    Melampyrum_sum[i] = f_sum[i]
  }
}
bigdf$Melampyrum <- Melampyrum_sum
bigdf <- subset(bigdf, select = -c(cf..Melampyrum))

#Pedicularis
aa <- apply(is.na(bigdf[,c("Pedicularis", "cf..Pedicularis")]),1,all)
f_sum <- rowSums(bigdf[,c("Pedicularis", "cf..Pedicularis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Pedicularis", "cf..Pedicularis")])
Pedicularis_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Pedicularis_sum[i] = t_sum[i]
  }
  else{
    Pedicularis_sum[i] = f_sum[i]
  }
}
bigdf$Pedicularis <- Pedicularis_sum
bigdf <- subset(bigdf, select = -c(cf..Pedicularis))

#Pteridium
aa <- apply(is.na(bigdf[,c("Pteridium", "cf..Pteridium")]),1,all)
f_sum <- rowSums(bigdf[,c("Pteridium", "cf..Pteridium")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Pteridium", "cf..Pteridium")])
Pteridium_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Pteridium_sum[i] = t_sum[i]
  }
  else{
    Pteridium_sum[i] = f_sum[i]
  }
}
bigdf$Pteridium <- Pteridium_sum
bigdf <- subset(bigdf, select = -c(cf..Pteridium))

#Saxifraga.granulata
aa <- apply(is.na(bigdf[,c("Saxifraga.granulata.type", "cf..Saxifraga.granulata")]),1,all)
f_sum <- rowSums(bigdf[,c("Saxifraga.granulata.type", "cf..Saxifraga.granulata")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Saxifraga.granulata.type", "cf..Saxifraga.granulata")])
Saxifraga.granulata_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Saxifraga.granulata_sum[i] = t_sum[i]
  }
  else{
    Saxifraga.granulata_sum[i] = f_sum[i]
  }
}
bigdf$Saxifraga.granulata <- Saxifraga.granulata_sum
bigdf <- subset(bigdf, select = -c(Saxifraga.granulata.type, cf..Saxifraga.granulata))

#Scutellaria
aa <- apply(is.na(bigdf[,c("Scutellaria.type", "cf..Scutellaria")]),1,all)
f_sum <- rowSums(bigdf[,c("Scutellaria.type", "cf..Scutellaria")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Scutellaria.type", "cf..Scutellaria")])
Scutellaria_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Scutellaria_sum[i] = t_sum[i]
  }
  else{
    Scutellaria_sum[i] = f_sum[i]
  }
}
bigdf$Scutellaria <- Scutellaria_sum
bigdf <- subset(bigdf, select = -c(Scutellaria.type, cf..Scutellaria))

#Sorbus.aucuparia
aa <- apply(is.na(bigdf[,c("Sorbus.aucuparia", "cf..Sorbus.aucuparia")]),1,all)
f_sum <- rowSums(bigdf[,c("Sorbus.aucuparia", "cf..Sorbus.aucuparia")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Sorbus.aucuparia", "cf..Sorbus.aucuparia")])
Sorbus.aucuparia_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Sorbus.aucuparia_sum[i] = t_sum[i]
  }
  else{
    Sorbus.aucuparia_sum[i] = f_sum[i]
  }
}
bigdf$Sorbus.aucuparia <- Sorbus.aucuparia_sum
bigdf <- subset(bigdf, select = -c(cf..Sorbus.aucuparia))

#Valeriana
aa <- apply(is.na(bigdf[,c("Valeriana", "cf..Valeriana")]),1,all)
f_sum <- rowSums(bigdf[,c("Valeriana", "cf..Valeriana")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Valeriana", "cf..Valeriana")])
Valeriana_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Valeriana_sum[i] = t_sum[i]
  }
  else{
    Valeriana_sum[i] = f_sum[i]
  }
}
bigdf$Valeriana <- Valeriana_sum
bigdf <- subset(bigdf, select = -c(cf..Valeriana))

#Caltha
aa <- apply(is.na(bigdf[,c("Caltha", "Caltha.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Caltha", "Caltha.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Caltha", "Caltha.type")])
Caltha_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Caltha_sum[i] = t_sum[i]
  }
  else{
    Caltha_sum[i] = f_sum[i]
  }
}
bigdf$Caltha <- Caltha_sum
bigdf <- subset(bigdf, select = -c(Caltha.type))

#Campanula
aa <- apply(is.na(bigdf[,c("Campanula", "Campanula.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Campanula", "Campanula.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Campanula", "Campanula.type")])
Campanula_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Campanula_sum[i] = t_sum[i]
  }
  else{
    Campanula_sum[i] = f_sum[i]
  }
}
bigdf$Campanula <- Campanula_sum
bigdf <- subset(bigdf, select = -c(Campanula.type))

#Cannabaceae
aa <- apply(is.na(bigdf[,c("Cannabaceae", "Cannabaceae.type", "Cannabaceae.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Cannabaceae", "Cannabaceae.type", "Cannabaceae.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Cannabaceae", "Cannabaceae.type", "Cannabaceae.undiff.")])
Cannabaceae_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Cannabaceae_sum[i] = t_sum[i]
  }
  else{
    Cannabaceae_sum[i] = f_sum[i]
  }
}
bigdf$Cannabaceae <- Cannabaceae_sum
bigdf <- subset(bigdf, select = -c(Cannabaceae.type, Cannabaceae.undiff.))

#Cannabis.sativa
aa <- apply(is.na(bigdf[,c("Cannabis.sativa", "Cannabaceae.cf..Cannabis.sativa")]),1,all)
f_sum <- rowSums(bigdf[,c("Cannabis.sativa", "Cannabaceae.cf..Cannabis.sativa")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Cannabis.sativa", "Cannabaceae.cf..Cannabis.sativa")])
Cannabis.sativa_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Cannabis.sativa_sum[i] = t_sum[i]
  }
  else{
    Cannabis.sativa_sum[i] = f_sum[i]
  }
}
bigdf$Cannabis.sativa <- Cannabis.sativa_sum
bigdf <- subset(bigdf, select = -c(Cannabaceae.cf..Cannabis.sativa))

#Centaurea.cyanus
aa <- apply(is.na(bigdf[,c("Centaurea.cyanus", "Centaurea.cyanus.type", "Centaurea.cf..C..cyanus")]),1,all)
f_sum <- rowSums(bigdf[,c("Centaurea.cyanus", "Centaurea.cyanus.type", "Centaurea.cf..C..cyanus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Centaurea.cyanus", "Centaurea.cyanus.type", "Centaurea.cf..C..cyanus")])
Centaurea.cyanus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Centaurea.cyanus_sum[i] = t_sum[i]
  }
  else{
    Centaurea.cyanus_sum[i] = f_sum[i]
  }
}
bigdf$Centaurea.cyanus <- Centaurea.cyanus_sum
bigdf <- subset(bigdf, select = -c(Centaurea.cyanus.type, Centaurea.cf..C..cyanus))

#Centaurea.scabiosa
aa <- apply(is.na(bigdf[,c("Centaurea.scabiosa", "Centaurea.scabiosa.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Centaurea.scabiosa", "Centaurea.scabiosa.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Centaurea.scabiosa", "Centaurea.scabiosa.type")])
Centaurea.scabiosa_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Centaurea.scabiosa_sum[i] = t_sum[i]
  }
  else{
    Centaurea.scabiosa_sum[i] = f_sum[i]
  }
}
bigdf$Centaurea.scabiosa <- Centaurea.scabiosa_sum
bigdf <- subset(bigdf, select = -c(Centaurea.scabiosa.type))

#Cirsium
aa <- apply(is.na(bigdf[,c("Cirsium", "Cirsium.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Cirsium", "Cirsium.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Cirsium", "Cirsium.type")])
Cirsium_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Cirsium_sum[i] = t_sum[i]
  }
  else{
    Cirsium_sum[i] = f_sum[i]
  }
}
bigdf$Cirsium <- Cirsium_sum
bigdf <- subset(bigdf, select = -c(Cirsium.type))

#Convallaria
aa <- apply(is.na(bigdf[,c("Convallaria", "Convallaria.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Convallaria", "Convallaria.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Convallaria", "Convallaria.type")])
Convallaria_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Convallaria_sum[i] = t_sum[i]
  }
  else{
    Convallaria_sum[i] = f_sum[i]
  }
}
bigdf$Convallaria <- Convallaria_sum
bigdf <- subset(bigdf, select = -c(Convallaria.type))

#Cornus.suecica
aa <- apply(is.na(bigdf[,c("Cornus.suecica", "Cornus.suecica..thick.wall.")]),1,all)
f_sum <- rowSums(bigdf[,c("Cornus.suecica", "Cornus.suecica..thick.wall.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Cornus.suecica", "Cornus.suecica..thick.wall.")])
Cornus.suecica_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Cornus.suecica_sum[i] = t_sum[i]
  }
  else{
    Cornus.suecica_sum[i] = f_sum[i]
  }
}
bigdf$Cornus.suecica <- Cornus.suecica_sum
bigdf <- subset(bigdf, select = -c(Cornus.suecica..thick.wall.))

#Corylus.avellana
aa <- apply(is.na(bigdf[,c("Corylus.avellana", "Corylus.avellana.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Corylus.avellana", "Corylus.avellana.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Corylus.avellana", "Corylus.avellana.type")])
Corylus.avellana_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Corylus.avellana_sum[i] = t_sum[i]
  }
  else{
    Corylus.avellana_sum[i] = f_sum[i]
  }
}
bigdf$Corylus.avellana <- Corylus.avellana_sum
bigdf <- subset(bigdf, select = -c(Corylus.avellana.type))

#Corylus
aa <- apply(is.na(bigdf[,c("Corylus", "Corylus.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Corylus", "Corylus.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Corylus", "Corylus.type")])
Corylus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Corylus_sum[i] = t_sum[i]
  }
  else{
    Corylus_sum[i] = f_sum[i]
  }
}
bigdf$Corylus <- Corylus.avellana_sum
bigdf <- subset(bigdf, select = -c(Corylus.type))

#Diphasiastrum.alpinum
aa <- apply(is.na(bigdf[,c("Diphasiastrum.alpinum.type", "Diphasiastrum.alpinum")]),1,all)
f_sum <- rowSums(bigdf[,c("Diphasiastrum.alpinum.type", "Diphasiastrum.alpinum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Diphasiastrum.alpinum.type", "Diphasiastrum.alpinum")])
Diphasiastrum.alpinum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Diphasiastrum.alpinum_sum[i] = t_sum[i]
  }
  else{
    Diphasiastrum.alpinum_sum[i] = f_sum[i]
  }
}
bigdf$Diphasiastrum.alpinum <- Diphasiastrum.alpinum_sum
bigdf <- subset(bigdf, select = -c(Diphasiastrum.alpinum.type))

#Diphasiastrum
aa <- apply(is.na(bigdf[,c("Diphasiastrum", "Diphasiastrum.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Diphasiastrum", "Diphasiastrum.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Diphasiastrum", "Diphasiastrum.type")])
Diphasiastrum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Diphasiastrum_sum[i] = t_sum[i]
  }
  else{
    Diphasiastrum_sum[i] = f_sum[i]
  }
}
bigdf$Diphasiastrum <- Diphasiastrum_sum
bigdf <- subset(bigdf, select = -c(Diphasiastrum.type))

#Dryopteris.dilatata
aa <- apply(is.na(bigdf[,c("Dryopteris.dilatata", "Dryopteris.dilatata.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Dryopteris.dilatata", "Dryopteris.dilatata.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Dryopteris.dilatata", "Dryopteris.dilatata.type")])
Dryopteris.dilatata_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Dryopteris.dilatata_sum[i] = t_sum[i]
  }
  else{
    Dryopteris.dilatata_sum[i] = f_sum[i]
  }
}
bigdf$Dryopteris.dilatata <- Dryopteris.dilatata_sum
bigdf <- subset(bigdf, select = -c(Dryopteris.dilatata.type))

#Dryopteris.filix-mas
aa <- apply(is.na(bigdf[,c("Dryopteris.filix.mas", "Dryopteris.filix.mas.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Dryopteris.filix.mas", "Dryopteris.filix.mas.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Dryopteris.filix.mas", "Dryopteris.filix.mas.type")])
Dryopteris.filix.mas_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Dryopteris.filix.mas_sum[i] = t_sum[i]
  }
  else{
    Dryopteris.filix.mas_sum[i] = f_sum[i]
  }
}
bigdf$Dryopteris.filix.mas <- Dryopteris.filix.mas_sum
bigdf <- subset(bigdf, select = -c(Dryopteris.filix.mas.type))

#Dryopteris
aa <- apply(is.na(bigdf[,c("Dryopteris", "Dryopteris.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Dryopteris", "Dryopteris.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Dryopteris", "Dryopteris.type")])
Dryopteris_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Dryopteris_sum[i] = t_sum[i]
  }
  else{
    Dryopteris_sum[i] = f_sum[i]
  }
}
bigdf$Dryopteris <- Dryopteris_sum
bigdf <- subset(bigdf, select = -c(Dryopteris.type))

bigdf$dry

#Ephedra.distachya
aa <- apply(is.na(bigdf[,c("Ephedra.distachya", "Ephedra.distachya.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Ephedra.distachya", "Ephedra.distachya.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Ephedra.distachya", "Ephedra.distachya.type")])
Ephedra.distachya_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Ephedra.distachya_sum[i] = t_sum[i]
  }
  else{
    Ephedra.distachya_sum[i] = f_sum[i]
  }
}
bigdf$Ephedra.distachya <- Ephedra.distachya_sum
bigdf <- subset(bigdf, select = -c(Ephedra.distachya.type))

#Ephedra.fragilis
aa <- apply(is.na(bigdf[,c("Ephedra.fragilis", "Ephedra.fragilis.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Ephedra.fragilis", "Ephedra.fragilis.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Ephedra.fragilis", "Ephedra.fragilis.type")])
Ephedra.fragilis_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Ephedra.fragilis_sum[i] = t_sum[i]
  }
  else{
    Ephedra.fragilis_sum[i] = f_sum[i]
  }
}
bigdf$Ephedra.fragilis <- Ephedra.fragilis_sum
bigdf <- subset(bigdf, select = -c(Ephedra.fragilis.type))

#Epilobium
aa <- apply(is.na(bigdf[,c("Epilobium", "Onagraceae.cf..Epilobium")]),1,all)
f_sum <- rowSums(bigdf[,c("Epilobium", "Onagraceae.cf..Epilobium")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Epilobium", "Onagraceae.cf..Epilobium")])
Epilobium_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Epilobium_sum[i] = t_sum[i]
  }
  else{
    Epilobium_sum[i] = f_sum[i]
  }
}
bigdf$Epilobium <- Epilobium_sum
bigdf <- subset(bigdf, select = -c(Onagraceae.cf..Epilobium))

#Galium
aa <- apply(is.na(bigdf[,c("Galium", "Galium.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Galium", "Galium.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Galium", "Galium.type")])
Galium_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Galium_sum[i] = t_sum[i]
  }
  else{
    Galium_sum[i] = f_sum[i]
  }
}
bigdf$Galium <- Galium_sum
bigdf <- subset(bigdf, select = -c(Galium.type))

#Geranium.sylvaticum
aa <- apply(is.na(bigdf[,c("Geranium.sylvaticum", "Geranium.sylvaticum.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Geranium.sylvaticum", "Geranium.sylvaticum.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Geranium.sylvaticum", "Geranium.sylvaticum.type")])
Geranium.sylvaticum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Geranium.sylvaticum_sum[i] = t_sum[i]
  }
  else{
    Geranium.sylvaticum_sum[i] = f_sum[i]
  }
}
bigdf$Geranium.sylvaticum <- Geranium.sylvaticum_sum
bigdf <- subset(bigdf, select = -c(Geranium.sylvaticum.type))

#Geum
aa <- apply(is.na(bigdf[,c("Geum", "Geum.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Geum", "Geum.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Geum", "Geum.type")])
Geum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Geum_sum[i] = t_sum[i]
  }
  else{
    Geum_sum[i] = f_sum[i]
  }
}
bigdf$Geum <- Geum_sum
bigdf <- subset(bigdf, select = -c(Geum.type))

#Geum.rivale
aa <- apply(is.na(bigdf[,c("Geum.cf..G..rivale", "Geum.rivale")]),1,all)
f_sum <- rowSums(bigdf[,c("Geum.cf..G..rivale", "Geum.rivale")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Geum.cf..G..rivale", "Geum.rivale")])
Geum.rivale_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Geum.rivale_sum[i] = t_sum[i]
  }
  else{
    Geum.rivale_sum[i] = f_sum[i]
  }
}
bigdf$Geum.rivale <- Geum.rivale_sum
bigdf <- subset(bigdf, select = -c(Geum.cf..G..rivale))

#Hordeum
aa <- apply(is.na(bigdf[,c("Hordeum", "Hordeum.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Hordeum", "Hordeum.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Hordeum", "Hordeum.type")])
Hordeum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Hordeum_sum[i] = t_sum[i]
  }
  else{
    Hordeum_sum[i] = f_sum[i]
  }
}
bigdf$Hordeum <- Hordeum_sum
bigdf <- subset(bigdf, select = -c(Hordeum.type))

#Huperzia.selago
aa <- apply(is.na(bigdf[,c("Huperzia.selago", "Huperzia.selago.subsp..arctica")]),1,all)
f_sum <- rowSums(bigdf[,c("Huperzia.selago", "Huperzia.selago.subsp..arctica")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Huperzia.selago", "Huperzia.selago.subsp..arctica")])
Huperzia.selago_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Huperzia.selago_sum[i] = t_sum[i]
  }
  else{
    Huperzia.selago_sum[i] = f_sum[i]
  }
}
bigdf$Huperzia.selago <- Huperzia.selago_sum
bigdf <- subset(bigdf, select = -c(Huperzia.selago.subsp..arctica))

#Hypericum.perforatum
aa <- apply(is.na(bigdf[,c("Hypericum.perforatum", "Hypericum.perforatum.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Hypericum.perforatum", "Hypericum.perforatum.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Hypericum.perforatum", "Hypericum.perforatum.type")])
Hypericum.perforatum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Hypericum.perforatum_sum[i] = t_sum[i]
  }
  else{
    Hypericum.perforatum_sum[i] = f_sum[i]
  }
}
bigdf$Hypericum.perforatum <- Hypericum.perforatum_sum
bigdf <- subset(bigdf, select = -c(Hypericum.perforatum.type))


#Isoetes.lacustris
aa <- apply(is.na(bigdf[,c("Isoëtes.lacustris", "Isoëtes.cf..I..lacustris")]),1,all)
f_sum <- rowSums(bigdf[,c("Isoëtes.lacustris", "Isoëtes.cf..I..lacustris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Isoëtes.lacustris", "Isoëtes.cf..I..lacustris")])
Isoetes.lacustris_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Isoetes.lacustris_sum[i] = t_sum[i]
  }
  else{
    Isoetes.lacustris_sum[i] = f_sum[i]
  }
}
bigdf$Isoetes.lacustris <- Isoetes.lacustris_sum
bigdf <- subset(bigdf, select = -c(Isoëtes.lacustris, Isoëtes.cf..I..lacustris))

#Isoetes
aa <- apply(is.na(bigdf[,c("Isoëtes", "Isoëtes.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Isoëtes", "Isoëtes.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Isoëtes", "Isoëtes.undiff.")])
Isoetes_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Isoetes_sum[i] = t_sum[i]
  }
  else{
    Isoetes_sum[i] = f_sum[i]
  }
}
bigdf$Isoetes <- Isoetes_sum
bigdf <- subset(bigdf, select = -c(Isoëtes, Isoëtes.undiff.))

#Jasione
aa <- apply(is.na(bigdf[,c("Jasione", "Jasione.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Jasione", "Jasione.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Jasione", "Jasione.type")])
Jasione_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Jasione_sum[i] = t_sum[i]
  }
  else{
    Jasione_sum[i] = f_sum[i]
  }
}
bigdf$Jasione <- Jasione_sum
bigdf <- subset(bigdf, select = -c(Jasione.type))

#Lathyrus
aa <- apply(is.na(bigdf[,c("Lathyrus", "Lathyrus.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Lathyrus", "Lathyrus.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Lathyrus", "Lathyrus.type")])
Lathyrus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Lathyrus_sum[i] = t_sum[i]
  }
  else{
    Lathyrus_sum[i] = f_sum[i]
  }
}
bigdf$Lathyrus <- Lathyrus_sum
bigdf <- subset(bigdf, select = -c(Lathyrus.type))

#Lotus
aa <- apply(is.na(bigdf[,c("Lotus", "Lotus.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Lotus", "Lotus.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Lotus", "Lotus.type")])
Lotus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Lotus_sum[i] = t_sum[i]
  }
  else{
    Lotus_sum[i] = f_sum[i]
  }
}
bigdf$Lotus <- Lotus_sum
bigdf <- subset(bigdf, select = -c(Lotus.type))

#Lycopodium
aa <- apply(is.na(bigdf[,c("Lycopodium", "Lycopodium.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Lycopodium", "Lycopodium.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Lycopodium", "Lycopodium.undiff.")])
Lycopodium_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Lycopodium_sum[i] = t_sum[i]
  }
  else{
    Lycopodium_sum[i] = f_sum[i]
  }
}
bigdf$Lycopodium <- Lycopodium_sum
bigdf <- subset(bigdf, select = -c(Lycopodium.undiff.))

#Lycopodium.annotinum
aa <- apply(is.na(bigdf[,c("Lycopodium.annotinum", "Lycopodium.annotinum.type", "Lycopodium.annotinum.subsp..alpestre")]),1,all)
f_sum <- rowSums(bigdf[,c("Lycopodium.annotinum", "Lycopodium.annotinum.type", "Lycopodium.annotinum.subsp..alpestre")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Lycopodium.annotinum", "Lycopodium.annotinum.type", "Lycopodium.annotinum.subsp..alpestre")])
Lycopodium.annotinum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Lycopodium.annotinum_sum[i] = t_sum[i]
  }
  else{
    Lycopodium.annotinum_sum[i] = f_sum[i]
  }
}
bigdf$Lycopodium.annotinum <- Lycopodium.annotinum_sum
bigdf <- subset(bigdf, select = -c(Lycopodium.annotinum.type, Lycopodium.annotinum.subsp..alpestre))

#Lysimachia
aa <- apply(is.na(bigdf[,c("Lysimachia", "Lysimachia.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Lysimachia", "Lysimachia.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Lysimachia", "Lysimachia.type")])
Lysimachia_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Lysimachia_sum[i] = t_sum[i]
  }
  else{
    Lysimachia_sum[i] = f_sum[i]
  }
}
bigdf$Lysimachia <- Lysimachia_sum
bigdf <- subset(bigdf, select = -c(Lysimachia.type))

#Lysimachia.vulgaris
aa <- apply(is.na(bigdf[,c("Lysimachia.vulgaris", "Lysimachia.vulgaris.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Lysimachia.vulgaris", "Lysimachia.vulgaris.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Lysimachia.vulgaris", "Lysimachia.vulgaris.type")])
Lysimachia.vulgaris_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Lysimachia.vulgaris_sum[i] = t_sum[i]
  }
  else{
    Lysimachia.vulgaris_sum[i] = f_sum[i]
  }
}
bigdf$Lysimachia.vulgaris <- Lysimachia.vulgaris_sum
bigdf <- subset(bigdf, select = -c(Lysimachia.vulgaris.type))

#Micranthes.stellaris
aa <- apply(is.na(bigdf[,c("Micranthes.stellaris", "Micranthes.stellaris.type", "Micranthes.cf..M..stellaris")]),1,all)
f_sum <- rowSums(bigdf[,c("Micranthes.stellaris", "Micranthes.stellaris.type", "Micranthes.cf..M..stellaris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Micranthes.stellaris", "Micranthes.stellaris.type", "Micranthes.cf..M..stellaris")])
Micranthes.stellaris_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Micranthes.stellaris_sum[i] = t_sum[i]
  }
  else{
    Micranthes.stellaris_sum[i] = f_sum[i]
  }
}
bigdf$Micranthes.stellaris <- Micranthes.stellaris_sum
bigdf <- subset(bigdf, select = -c(Micranthes.cf..M..stellaris, Micranthes.stellaris.type))

#Myriophyllum.alterniflorum
aa <- apply(is.na(bigdf[,c("Myriophyllum.alterniflorum", "Myriophyllum.alterniflorum.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Myriophyllum.alterniflorum", "Myriophyllum.alterniflorum.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Myriophyllum.alterniflorum", "Myriophyllum.alterniflorum.type")])
Myriophyllum.alterniflorum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Myriophyllum.alterniflorum_sum[i] = t_sum[i]
  }
  else{
    Myriophyllum.alterniflorum_sum[i] = f_sum[i]
  }
}
bigdf$Myriophyllum.alterniflorum <- Myriophyllum.alterniflorum_sum
bigdf <- subset(bigdf, select = -c(Myriophyllum.alterniflorum.type))

#Mentha
aa <- apply(is.na(bigdf[,c("Mentha", "Mentha.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Mentha", "Mentha.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Mentha", "Mentha.type")])
Mentha_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Mentha_sum[i] = t_sum[i]
  }
  else{
    Mentha_sum[i] = f_sum[i]
  }
}
bigdf$Mentha <- Mentha_sum
bigdf <- subset(bigdf, select = -c(Mentha.type))

#Mercurialis
aa <- apply(is.na(bigdf[,c("Euphorbiaceae.cf..Mercurialis", "Mercurialis")]),1,all)
f_sum <- rowSums(bigdf[,c("Euphorbiaceae.cf..Mercurialis", "Mercurialis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Euphorbiaceae.cf..Mercurialis", "Mercurialis")])
Mercurialis_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Mercurialis_sum[i] = t_sum[i]
  }
  else{
    Mercurialis_sum[i] = f_sum[i]
  }
}
bigdf$Mercurialis <- Mercurialis_sum
bigdf <- subset(bigdf, select = -c(Euphorbiaceae.cf..Mercurialis))

#Nymphaceae
aa <- apply(is.na(bigdf[,c("Nymphaeaceae.hair.NISP", "Nymphaeaceae.sclereid.cell..star.shaped.NISP", "Nymphaeaceae.trichoblast.NISP", "Nymphaeaceae.suberized.basal.cells.NISP")]),1,all)
f_sum <- rowSums(bigdf[,c("Nymphaeaceae.hair.NISP", "Nymphaeaceae.sclereid.cell..star.shaped.NISP", "Nymphaeaceae.trichoblast.NISP", "Nymphaeaceae.suberized.basal.cells.NISP")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Nymphaeaceae.hair.NISP", "Nymphaeaceae.sclereid.cell..star.shaped.NISP", "Nymphaeaceae.trichoblast.NISP", "Nymphaeaceae.suberized.basal.cells.NISP")])
Nymphaeaceae_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Nymphaeaceae_sum[i] = t_sum[i]
  }
  else{
    Nymphaeaceae_sum[i] = f_sum[i]
  }
}
bigdf$Nymphaeaceae <- Nymphaeaceae_sum
bigdf <- subset(bigdf, select = -c(Nymphaeaceae.hair.NISP, Nymphaeaceae.sclereid.cell..star.shaped.NISP, Nymphaeaceae.suberized.basal.cells.NISP, Nymphaeaceae.trichoblast.NISP))

#Oxyria
aa <- apply(is.na(bigdf[,c("Oxyria", "Oxyria.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Oxyria", "Oxyria.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Oxyria", "Oxyria.type")])
Oxyria_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Oxyria_sum[i] = t_sum[i]
  }
  else{
    Oxyria_sum[i] = f_sum[i]
  }
}
bigdf$Oxyria <- Oxyria_sum
bigdf <- subset(bigdf, select = -c(Oxyria.type))

#Persicaria.maculosa
aa <- apply(is.na(bigdf[,c("Persicaria.maculosa", "Persicaria.maculosa.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Persicaria.maculosa", "Persicaria.maculosa.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Persicaria.maculosa", "Persicaria.maculosa.type")])
Persicaria.maculosa_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Persicaria.maculosa_sum[i] = t_sum[i]
  }
  else{
    Persicaria.maculosa_sum[i] = f_sum[i]
  }
}
bigdf$Persicaria.maculosa <- Persicaria.maculosa_sum
bigdf <- subset(bigdf, select = -c(Persicaria.maculosa.type))

#Picea.abies
aa <- apply(is.na(bigdf[,c("Picea.abies", "Picea.abies.stomate.NISP", "Picea.abies.pollen.NISP")]),1,all)
f_sum <- rowSums(bigdf[,c("Picea.abies", "Picea.abies.stomate.NISP", "Picea.abies.pollen.NISP")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Picea.abies", "Picea.abies.stomate.NISP", "Picea.abies.pollen.NISP")])
Picea.abies_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Picea.abies_sum[i] = t_sum[i]
  }
  else{
    Picea.abies_sum[i] = f_sum[i]
  }
}
bigdf$Picea.abies <- Picea.abies_sum
bigdf <- subset(bigdf, select = -c(Picea.abies.pollen.NISP, Picea.abies.stomate.NISP))

#Pinus.sylvestris
aa <- apply(is.na(bigdf[,c("Pinus.sylvestris", "Pinus.sylvestris.stomate.NISP", "Pinus.sylvestris.pollen.NISP")]),1,all)
f_sum <- rowSums(bigdf[,c("Pinus.sylvestris", "Pinus.sylvestris.stomate.NISP", "Pinus.sylvestris.pollen.NISP")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Pinus.sylvestris", "Pinus.sylvestris.stomate.NISP", "Pinus.sylvestris.pollen.NISP")])
Pinus.sylvestris_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Pinus.sylvestris_sum[i] = t_sum[i]
  }
  else{
    Pinus.sylvestris_sum[i] = f_sum[i]
  }
}
bigdf$Pinus.sylvestris <- Pinus.sylvestris_sum
bigdf <- subset(bigdf, select = -c(Pinus.sylvestris.stomate.NISP, Pinus.sylvestris.pollen.NISP))

#Plantago.coronopus
aa <- apply(is.na(bigdf[,c("Plantago.coronopus", "Plantago.cf..P..coronopus")]),1,all)
f_sum <- rowSums(bigdf[,c("Plantago.coronopus", "Plantago.cf..P..coronopus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Plantago.coronopus", "Plantago.cf..P..coronopus")])
Plantago.coronopus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Plantago.coronopus_sum[i] = t_sum[i]
  }
  else{
    Plantago.coronopus_sum[i] = f_sum[i]
  }
}
bigdf$Plantago.coronopus <- Plantago.coronopus_sum
bigdf <- subset(bigdf, select = -c(Plantago.cf..P..coronopus))

#Plantago
aa <- apply(is.na(bigdf[,c("Plantago", "Plantago.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Plantago", "Plantago.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Plantago", "Plantago.undiff.")])
Plantago_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Plantago_sum[i] = t_sum[i]
  }
  else{
    Plantago_sum[i] = f_sum[i]
  }
}
bigdf$Plantago <- Plantago_sum
bigdf <- subset(bigdf, select = -c(Plantago.undiff.))

#Poaceae
aa <- apply(is.na(bigdf[,c("Poaceae", "Poaceae.undiff.", "Poaceae..annulus.8.10.µm.")]),1,all)
f_sum <- rowSums(bigdf[,c("Poaceae", "Poaceae.undiff.", "Poaceae..annulus.8.10.µm.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Poaceae", "Poaceae.undiff.", "Poaceae..annulus.8.10.µm.")])
Poaceae_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Poaceae_sum[i] = t_sum[i]
  }
  else{
    Poaceae_sum[i] = f_sum[i]
  }
}
bigdf$Poaceae <- Poaceae_sum
bigdf <- subset(bigdf, select = -c(Poaceae.undiff., Poaceae..annulus.8.10.µm.))

#Cerealia
aa <- apply(is.na(bigdf[,c("Poaceae..Cerealia..undiff.", "Poaceae..Cerealia.type.")]),1,all)
f_sum <- rowSums(bigdf[,c("Poaceae..Cerealia..undiff.", "Poaceae..Cerealia.type.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Poaceae..Cerealia..undiff.", "Poaceae..Cerealia.type.")])
Cerealia_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Cerealia_sum[i] = t_sum[i]
  }
  else{
    Cerealia_sum[i] = f_sum[i]
  }
}
bigdf$Cerealia <- Cerealia_sum
bigdf <- subset(bigdf, select = -c(Poaceae..Cerealia..undiff., Poaceae..Cerealia.type.))

#Polygonum.aviculare
aa <- apply(is.na(bigdf[,c("Polygonum.aviculare", "Polygonum.cf..P..aviculare", "Polygonum.aviculare.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Polygonum.aviculare", "Polygonum.cf..P..aviculare", "Polygonum.aviculare.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Polygonum.aviculare", "Polygonum.cf..P..aviculare", "Polygonum.aviculare.type")])
Polygonum.aviculare_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Polygonum.aviculare_sum[i] = t_sum[i]
  }
  else{
    Polygonum.aviculare_sum[i] = f_sum[i]
  }
}
bigdf$Polygonum.aviculare <- Polygonum.aviculare_sum
bigdf <- subset(bigdf, select = -c(Polygonum.cf..P..aviculare, Polygonum.aviculare.type))

#Polypodium
aa <- apply(is.na(bigdf[,c("Polypodium", "Polypodium.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Polypodium", "Polypodium.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Polypodium", "Polypodium.type")])
Polypodium_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Polypodium_sum[i] = t_sum[i]
  }
  else{
    Polypodium_sum[i] = f_sum[i]
  }
}
bigdf$Polypodium <- Polypodium_sum
bigdf <- subset(bigdf, select = -c(Polypodium.type))

#Polystichum
aa <- apply(is.na(bigdf[,c("Polystichum", "Polystichum.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Polystichum", "Polystichum.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Polystichum", "Polystichum.type")])
Polystichum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Polystichum_sum[i] = t_sum[i]
  }
  else{
    Polystichum_sum[i] = f_sum[i]
  }
}
bigdf$Polystichum <- Polystichum_sum
bigdf <- subset(bigdf, select = -c(Polystichum.type))

#Potamogeton
aa <- apply(is.na(bigdf[,c("Potamogeton", "Potamogeton.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Potamogeton", "Potamogeton.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Potamogeton", "Potamogeton.type")])
Potamogeton_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Potamogeton_sum[i] = t_sum[i]
  }
  else{
    Potamogeton_sum[i] = f_sum[i]
  }
}
bigdf$Potamogeton <- Potamogeton_sum
bigdf <- subset(bigdf, select = -c(Potamogeton.type))

#Potentilla
aa <- apply(is.na(bigdf[,c("Potentilla", "Potentilla.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Potentilla", "Potentilla.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Potentilla", "Potentilla.type")])
Potentilla_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Potentilla_sum[i] = t_sum[i]
  }
  else{
    Potentilla_sum[i] = f_sum[i]
  }
}
bigdf$Potentilla <- Potentilla_sum
bigdf <- subset(bigdf, select = -c(Potentilla.type))

#Pteridium
aa <- apply(is.na(bigdf[,c("Pteridium", "Pteridium.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Pteridium", "Pteridium.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Pteridium", "Pteridium.type")])
Pteridium_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Pteridium_sum[i] = t_sum[i]
  }
  else{
    Pteridium_sum[i] = f_sum[i]
  }
}
bigdf$Pteridium <- Pteridium_sum
bigdf <- subset(bigdf, select = -c(Pteridium.type))

#Ranunculus.acris
aa <- apply(is.na(bigdf[,c("Ranunculus.acris", "Ranunculus.acris.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Ranunculus.acris", "Ranunculus.acris.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Ranunculus.acris", "Ranunculus.acris.type")])
Ranunculus.acris_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Ranunculus.acris_sum[i] = t_sum[i]
  }
  else{
    Ranunculus.acris_sum[i] = f_sum[i]
  }
}
bigdf$Ranunculus.acris <- Ranunculus.acris_sum
bigdf <- subset(bigdf, select = -c(Ranunculus.acris.type))

#Ranunculus
aa <- apply(is.na(bigdf[,c("Ranunculus", "Ranunculus.undiff.", "Ranunculus.sp.", "Ranunculus.type", "Ranunculus.sect..Batrachium")]),1,all)
f_sum <- rowSums(bigdf[,c("Ranunculus", "Ranunculus.undiff.", "Ranunculus.sp.", "Ranunculus.type", "Ranunculus.sect..Batrachium")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Ranunculus", "Ranunculus.undiff.", "Ranunculus.sp.", "Ranunculus.type", "Ranunculus.sect..Batrachium")])
Ranunculus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Ranunculus_sum[i] = t_sum[i]
  }
  else{
    Ranunculus_sum[i] = f_sum[i]
  }
}
bigdf$Ranunculus <- Ranunculus_sum
bigdf <- subset(bigdf, select = -c(Ranunculus.undiff., Ranunculus.sp., Ranunculus.type, Ranunculus.sect..Batrachium))

#Rhinanthus
aa <- apply(is.na(bigdf[,c("Rhinanthus", "Rhinanthus.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Rhinanthus", "Rhinanthus.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Rhinanthus", "Rhinanthus.type")])
Rhinanthus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Rhinanthus_sum[i] = t_sum[i]
  }
  else{
    Rhinanthus_sum[i] = f_sum[i]
  }
}
bigdf$Rhinanthus <- Rhinanthus_sum
bigdf <- subset(bigdf, select = -c(Rhinanthus.type))

#Rubus
aa <- apply(is.na(bigdf[,c("Rubus", "Rubus.sp.")]),1,all)
f_sum <- rowSums(bigdf[,c("Rubus", "Rubus.sp.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Rubus", "Rubus.sp.")])
Rubus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Rubus_sum[i] = t_sum[i]
  }
  else{
    Rubus_sum[i] = f_sum[i]
  }
}
bigdf$Rubus <- Rubus_sum
bigdf <- subset(bigdf, select = -c(Rubus.sp.))

#Rubus.saxatilis
aa <- apply(is.na(bigdf[,c("Rubus.saxatilis", "Rubus.cf..R..saxatilis")]),1,all)
f_sum <- rowSums(bigdf[,c("Rubus.saxatilis", "Rubus.cf..R..saxatilis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Rubus.saxatilis", "Rubus.cf..R..saxatilis")])
Rubus.saxatilis_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Rubus.saxatilis_sum[i] = t_sum[i]
  }
  else{
    Rubus.saxatilis_sum[i] = f_sum[i]
  }
}
bigdf$Rubus.saxatilis <- Rubus.saxatilis_sum
bigdf <- subset(bigdf, select = -c(Rubus.cf..R..saxatilis))

#Rumex.acetosa
aa <- apply(is.na(bigdf[,c("Rumex.acetosa", "Rumex.acetosa.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Rumex.acetosa", "Rumex.acetosa.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Rumex.acetosa", "Rumex.acetosa.type")])
Rumex.acetosa_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Rumex.acetosa_sum[i] = t_sum[i]
  }
  else{
    Rumex.acetosa_sum[i] = f_sum[i]
  }
}
bigdf$Rumex.acetosa <- Rumex.acetosa_sum
bigdf <- subset(bigdf, select = -c(Rumex.acetosa.type))

#Rumex.longifolius
aa <- apply(is.na(bigdf[,c("Rumex.longifolius", "Rumex.longifolius.type", "Rumex.cf..R..longifolius")]),1,all)
f_sum <- rowSums(bigdf[,c("Rumex.longifolius", "Rumex.longifolius.type", "Rumex.cf..R..longifolius")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Rumex.longifolius", "Rumex.longifolius.type", "Rumex.cf..R..longifolius")])
Rumex.longifolius_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Rumex.longifolius_sum[i] = t_sum[i]
  }
  else{
    Rumex.longifolius_sum[i] = f_sum[i]
  }
}
bigdf$Rumex.longifolius <- Rumex.longifolius_sum
bigdf <- subset(bigdf, select = -c(Rumex.cf..R..longifolius, Rumex.longifolius.type))

#Rumex
aa <- apply(is.na(bigdf[,c("Rumex", "Rumex.subg..Acetosa")]),1,all)
f_sum <- rowSums(bigdf[,c("Rumex", "Rumex.subg..Acetosa")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Rumex", "Rumex.subg..Acetosa")])
Rumex_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Rumex_sum[i] = t_sum[i]
  }
  else{
    Rumex_sum[i] = f_sum[i]
  }
}
bigdf$Rumex <- Rumex_sum
bigdf <- subset(bigdf, select = -c(Rumex.subg..Acetosa))

#Sagina
aa <- apply(is.na(bigdf[,c("Sagina", "Sagina.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Sagina", "Sagina.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Sagina", "Sagina.type")])
Sagina_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Sagina_sum[i] = t_sum[i]
  }
  else{
    Sagina_sum[i] = f_sum[i]
  }
}
bigdf$Sagina <- Sagina_sum
bigdf <- subset(bigdf, select = -c(Sagina.type))

#Salix
aa <- apply(is.na(bigdf[,c("Salix", "Salix.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Salix", "Salix.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Salix", "Salix.undiff.")])
Salix_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Salix_sum[i] = t_sum[i]
  }
  else{
    Salix_sum[i] = f_sum[i]
  }
}
bigdf$Salix <- Salix_sum
bigdf <- subset(bigdf, select = -c(Salix.undiff.))

#Salix.herbacea
aa <- apply(is.na(bigdf[,c("Salix.herbacea", "Salix.herbacea.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Salix.herbacea", "Salix.herbacea.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Salix.herbacea", "Salix.herbacea.type")])
Salix.herbacea_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Salix.herbacea_sum[i] = t_sum[i]
  }
  else{
    Salix.herbacea_sum[i] = f_sum[i]
  }
}
bigdf$Salix.herbacea <- Salix.herbacea_sum
bigdf <- subset(bigdf, select = -c(Salix.herbacea.type))

#Sambucus
aa <- apply(is.na(bigdf[,c("Sambucus", "Sambucus.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Sambucus", "Sambucus.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Sambucus", "Sambucus.type")])
Sambucus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Sambucus_sum[i] = t_sum[i]
  }
  else{
    Sambucus_sum[i] = f_sum[i]
  }
}
bigdf$Sambucus <- Sambucus_sum
bigdf <- subset(bigdf, select = -c(Sambucus.type))

#Saussurea
aa <- apply(is.na(bigdf[,c("Saussurea", "Saussurea.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Saussurea", "Saussurea.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Saussurea", "Saussurea.type")])
Saussurea_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Saussurea_sum[i] = t_sum[i]
  }
  else{
    Saussurea_sum[i] = f_sum[i]
  }
}
bigdf$Saussurea <- Saussurea_sum
bigdf <- subset(bigdf, select = -c(Saussurea.type))

#Saxifraga.oppositifolia
aa <- apply(is.na(bigdf[,c("Saxifraga.oppositifolia", "Saxifraga.oppositifolia.type", "Saxifraga.cf..S..oppositifolia")]),1,all)
f_sum <- rowSums(bigdf[,c("Saxifraga.oppositifolia", "Saxifraga.oppositifolia.type", "Saxifraga.cf..S..oppositifolia")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Saxifraga.oppositifolia", "Saxifraga.oppositifolia.type", "Saxifraga.cf..S..oppositifolia")])
Saxifraga.oppositifolia_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Saxifraga.oppositifolia_sum[i] = t_sum[i]
  }
  else{
    Saxifraga.oppositifolia_sum[i] = f_sum[i]
  }
}
bigdf$Saxifraga.oppositifolia <- Saxifraga.oppositifolia_sum
bigdf <- subset(bigdf, select = -c(Saxifraga.oppositifolia.type, Saxifraga.cf..S..oppositifolia))

#Scrophularia
aa <- apply(is.na(bigdf[,c("Scrophularia", "Scrophularia.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Scrophularia", "Scrophularia.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Scrophularia", "Scrophularia.type")])
Scrophularia_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Scrophularia_sum[i] = t_sum[i]
  }
  else{
    Scrophularia_sum[i] = f_sum[i]
  }
}
bigdf$Scrophularia <- Scrophularia_sum
bigdf <- subset(bigdf, select = -c(Scrophularia.type))

#Sedum
aa <- apply(is.na(bigdf[,c("Sedum", "Sedum.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Sedum", "Sedum.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Sedum", "Sedum.type")])
Sedum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Sedum_sum[i] = t_sum[i]
  }
  else{
    Sedum_sum[i] = f_sum[i]
  }
}
bigdf$Sedum <- Sedum_sum
bigdf <- subset(bigdf, select = -c(Sedum.type))

#Silene.acaulis
aa <- apply(is.na(bigdf[,c("Silene.acaulis", "Silene.acaulis.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Silene.acaulis", "Silene.acaulis.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Silene.acaulis", "Silene.acaulis.type")])
Silene.acaulis_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Silene.acaulis_sum[i] = t_sum[i]
  }
  else{
    Silene.acaulis_sum[i] = f_sum[i]
  }
}
bigdf$Silene.acaulis <- Silene.acaulis_sum
bigdf <- subset(bigdf, select = -c(Silene.acaulis.type))

#Silene.dioica
aa <- apply(is.na(bigdf[,c("Silene.dioica", "Silene.dioica.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Silene.dioica", "Silene.dioica.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Silene.dioica", "Silene.dioica.type")])
Silene.dioica_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Silene.dioica_sum[i] = t_sum[i]
  }
  else{
    Silene.dioica_sum[i] = f_sum[i]
  }
}
bigdf$Silene.dioica <- Silene.dioica_sum
bigdf <- subset(bigdf, select = -c(Silene.dioica.type))

#Sinapis
aa <- apply(is.na(bigdf[,c("Sinapis", "Sinapis.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Sinapis", "Sinapis.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Sinapis", "Sinapis.type")])
Sinapis_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Sinapis_sum[i] = t_sum[i]
  }
  else{
    Sinapis_sum[i] = f_sum[i]
  }
}
bigdf$Sinapis <- Sinapis_sum
bigdf <- subset(bigdf, select = -c(Sinapis.type))

#Sorbus
aa <- apply(is.na(bigdf[,c("Sorbus", "Sorbus.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Sorbus", "Sorbus.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Sorbus", "Sorbus.type")])
Sorbus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Sorbus_sum[i] = t_sum[i]
  }
  else{
    Sorbus_sum[i] = f_sum[i]
  }
}
bigdf$Sorbus <- Sorbus_sum
bigdf <- subset(bigdf, select = -c(Sorbus.type))

#Sparganium
aa <- apply(is.na(bigdf[,c("Sparganium", "Sparganium.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Sparganium", "Sparganium.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Sparganium", "Sparganium.type")])
Sparganium_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Sparganium_sum[i] = t_sum[i]
  }
  else{
    Sparganium_sum[i] = f_sum[i]
  }
}
bigdf$Sparganium <- Sparganium_sum
bigdf <- subset(bigdf, select = -c(Sparganium.type))

#Spergula
aa <- apply(is.na(bigdf[,c("Spergula", "Spergula.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Spergula", "Spergula.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Spergula", "Spergula.type")])
Spergula_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Spergula_sum[i] = t_sum[i]
  }
  else{
    Spergula_sum[i] = f_sum[i]
  }
}
bigdf$Spergula <- Spergula_sum
bigdf <- subset(bigdf, select = -c(Spergula.type))

#Stachys
aa <- apply(is.na(bigdf[,c("Stachys", "Stachys.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Stachys", "Stachys.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Stachys", "Stachys.type")])
Stachys_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Stachys_sum[i] = t_sum[i]
  }
  else{
    Stachys_sum[i] = f_sum[i]
  }
}
bigdf$Stachys <- Stachys_sum
bigdf <- subset(bigdf, select = -c(Stachys.type))

#Trifolium
aa <- apply(is.na(bigdf[,c("Trifolium", "Trifolium.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Trifolium", "Trifolium.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Trifolium", "Trifolium.type")])
Trifolium_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Trifolium_sum[i] = t_sum[i]
  }
  else{
    Trifolium_sum[i] = f_sum[i]
  }
}
bigdf$Trifolium <- Trifolium_sum
bigdf <- subset(bigdf, select = -c(Trifolium.type))

#Trifolium.pratense
aa <- apply(is.na(bigdf[,c("Trifolium.pratense", "Trifolium.pratense.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Trifolium.pratense", "Trifolium.pratense.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Trifolium.pratense", "Trifolium.pratense.type")])
Trifolium.pratense_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Trifolium.pratense_sum[i] = t_sum[i]
  }
  else{
    Trifolium.pratense_sum[i] = f_sum[i]
  }
}
bigdf$Trifolium.pratense <- Trifolium.pratense_sum
bigdf <- subset(bigdf, select = -c(Trifolium.pratense.type))

#Veronica
aa <- apply(is.na(bigdf[,c("Veronica", "Veronica.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Veronica", "Veronica.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Veronica", "Veronica.type")])
Veronica_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Veronica_sum[i] = t_sum[i]
  }
  else{
    Veronica_sum[i] = f_sum[i]
  }
}
bigdf$Veronica <- Veronica_sum
bigdf <- subset(bigdf, select = -c(Veronica.type))

#Vicia
aa <- apply(is.na(bigdf[,c("Vicia", "Vicia.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Vicia", "Vicia.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Vicia", "Vicia.type")])
Vicia_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Vicia_sum[i] = t_sum[i]
  }
  else{
    Vicia_sum[i] = f_sum[i]
  }
}
bigdf$Vicia <- Vicia_sum
bigdf <- subset(bigdf, select = -c(Vicia.type))


### Family names ###

#Apiaceae
aa <- apply(is.na(bigdf[,c("Apiaceae", "Apiaceae.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Apiaceae", "Apiaceae.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Apiaceae", "Apiaceae.undiff.")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Apiaceae <- new_sum
bigdf <- subset(bigdf, select = -c(Apiaceae.undiff.))


#Cyperaceae # DOESN'T WORK!!!
#which( colnames(bigdf_familynames)=="Cyperaceae.undiff." ) #304
#which( colnames(bigdf_familynames)=="Cyperaceae" ) #20, 332
#aa <- apply(is.na(bigdf[,c("Cyperaceae", "Cyperaceae", "Cyperaceae.undiff.")]),1,all)
#f_sum <- rowSums(bigdf[,c("Cyperaceae", "Cyperaceae", "Cyperaceae.undiff.")],na.rm=TRUE)
#t_sum <- rowSums(bigdf[,c("Cyperaceae", "Cyperaceae", "Cyperaceae.undiff.")])
#new_sum = f_sum # create a new vector the same size as f_sum
#for(i in 1:length(aa)){
#  if(aa[i]){
#    new_sum[i] = t_sum[i]
#  }
#  else{
#    new_sum[i] = f_sum[i]
#  }
#}
#bigdf <- subset(bigdf, select = -c(304, 332))
#bigdf$Cyperaceae <- new_sum
bigdf <- subset(bigdf, select = -c(Cyperaceae.undiff.))



#Caryophyllaceae
aa <- apply(is.na(bigdf[,c("Caryophyllaceae", "Caryophyllaceae.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Caryophyllaceae", "Caryophyllaceae.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Caryophyllaceae", "Caryophyllaceae.undiff.")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Caryophyllaceae <- new_sum
bigdf <- subset(bigdf, select = -c(Caryophyllaceae.undiff.))

#Dipsacoideae
aa <- apply(is.na(bigdf[,c("Dipsacoideae", "Dipsacoideae.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Dipsacoideae", "Dipsacoideae.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Dipsacoideae", "Dipsacoideae.undiff.")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Dipsacoideae <- new_sum
bigdf <- subset(bigdf, select = -c(Dipsacoideae.undiff.))

#Ericaceae
aa <- apply(is.na(bigdf[,c("Ericaceae", "Ericaceae.type", "Ericaceae.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Ericaceae", "Ericaceae.type", "Ericaceae.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Ericaceae", "Ericaceae.type", "Ericaceae.undiff.")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Ericaceae <- new_sum
bigdf <- subset(bigdf, select = -c(Ericaceae.type, Ericaceae.undiff.))

#Ericales
aa <- apply(is.na(bigdf[,c("Ericales", "Ericales.undiff.", "Ericales..tetrad.")]),1,all)
f_sum <- rowSums(bigdf[,c("Ericales", "Ericales.undiff.", "Ericales..tetrad.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Ericales", "Ericales.undiff.", "Ericales..tetrad.")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Ericales <- new_sum
bigdf <- subset(bigdf, select = -c(Ericales.undiff., Ericales..tetrad.))

#Fabaceae
aa <- apply(is.na(bigdf[,c("Fabaceae", "Fabaceae.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Fabaceae", "Fabaceae.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Fabaceae", "Fabaceae.undiff.")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Fabaceae <- new_sum
bigdf <- subset(bigdf, select = -c(Fabaceae.undiff.))

#Liliaceae
aa <- apply(is.na(bigdf[,c("Liliaceae", "Liliaceae.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Liliaceae", "Liliaceae.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Liliaceae", "Liliaceae.undiff.")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Liliaceae <- new_sum
bigdf <- subset(bigdf, select = -c(Liliaceae.undiff.))

#Lycopodiaceae
aa <- apply(is.na(bigdf[,c("Lycopodiaceae", "Lycopodiaceae.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Lycopodiaceae", "Lycopodiaceae.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Lycopodiaceae", "Lycopodiaceae.undiff.")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Lycopodiaceae <- new_sum
bigdf <- subset(bigdf, select = -c(Lycopodiaceae.undiff.))

#Ranunculaceae
aa <- apply(is.na(bigdf[,c("Ranunculaceae", "Ranunculaceae.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Ranunculaceae", "Ranunculaceae.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Ranunculaceae", "Ranunculaceae.undiff.")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Ranunculaceae <- new_sum
bigdf <- subset(bigdf, select = -c(Ranunculaceae.undiff.))

#Rosaceae
aa <- apply(is.na(bigdf[,c("Rosaceae", "Rosaceae.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Rosaceae", "Rosaceae.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Rosaceae", "Rosaceae.undiff.")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Rosaceae <- new_sum
bigdf <- subset(bigdf, select = -c(Rosaceae.undiff.))

#Scrophulariaceae
aa <- apply(is.na(bigdf[,c("Scrophulariaceae", "Scrophulariaceae.undiff.")]),1,all)
f_sum <- rowSums(bigdf[,c("Scrophulariaceae", "Scrophulariaceae.undiff.")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Scrophulariaceae", "Scrophulariaceae.undiff.")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Scrophulariaceae <- new_sum

### Unclear genera/species ###

#Avena.Triticum
aa <- apply(is.na(bigdf[,c("Avena.Triticum", "Avena.Triticum.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Avena.Triticum", "Avena.Triticum.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Avena.Triticum", "Avena.Triticum.type")])
Avena.Triticum_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Avena.Triticum_sum[i] = t_sum[i]
  }
  else{
    Avena.Triticum_sum[i] = f_sum[i]
  }
}
bigdf$Avena.Triticum <- Avena.Triticum_sum
bigdf <- subset(bigdf, select = -c(Avena.Triticum.type))

#Cirsium.Carduus
aa <- apply(is.na(bigdf[,c("Cirsium.Carduus", "Cirsium.Carduus.type")]),1,all)
f_sum <- rowSums(bigdf[,c("Cirsium.Carduus", "Cirsium.Carduus.type")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Cirsium.Carduus", "Cirsium.Carduus.type")])
Cirsium.Carduus_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    Cirsium.Carduus_sum[i] = t_sum[i]
  }
  else{
    Cirsium.Carduus_sum[i] = f_sum[i]
  }
}
bigdf$Cirsium.Carduus <- Cirsium.Carduus_sum
bigdf <- subset(bigdf, select = -c(Cirsium.Carduus.type))

### Data cleaning ###
bigdf <- bigdf %>% dplyr::rename(Aconitum.lycoctonum = Aconitum.lycoctonum.subsp..septentrionale) %>%
  dplyr::rename(Alnus.viridis = Alnus.viridis.subsp..fruticosa) %>% 
  dplyr::rename(Valeriana.excelsa = Valeriana.excelsa.subsp..sambucifolia.type) %>% 
  dplyr::rename(Anagallis.tenella = Anagallis.cf..A..tenella) %>% 
  dplyr::rename(Astragalus.glycyphyllos = Astragalus.cf..A..glycyphyllos) %>% 
  dplyr::rename(Cassiope = Ericaceae.cf..Cassiope) %>% 
  dplyr::rename(Lingulodinium.machaerophorum = Lingulodinium.cf..L..machaerophorum) %>%
  dplyr::rename(Lotus.corniculatus = Lotus.cf..L..corniculatus) %>% 
  dplyr::rename(Operculodinium.centrocarpum = Operculodinium.cf..O..centrocarpum) %>% 
  dplyr::rename(Rubus.idaeus = Rubus.cf..R..idaeus) %>% 
  dplyr::rename(Hippophae = Hippophaë) %>% 
  dplyr::rename(Hippophae.rhamnoides = Hippophaë.rhamnoides) %>% 
  dplyr::rename(Isoetes.echinospora = Isoëtes.echinospora) %>% 
  dplyr::rename(Isoetes.setacea = Isoëtes.setacea) %>% 
  dplyr::rename(Tetraedron.minimum = Tetraëdron.minimum)

bigdf <- bigdf %>% 
  dplyr::select(!c(Tracheophyta.undiff., Unknown, Unknown..trilete., Unknown..trilete..undiff., 
                                   Unknown..monolete..undiff.,Spermatophyta.undiff., Pteridophyta.undiff., 
                                   Pteridophyta..trilete., Pteridophyta..trilete..undiff., Pteridophyta..monolete.,
                                   Polypodiophyta.undiff...monolete..without.perine.,Scrophulariaceae.undiff.
                                   )) %>%
  dplyr::select(!c(Indeterminable..degraded., Indeterminable.Neotoma.Pollen.degraded.pollen.spore.NISP.Neotoma.Pollen.degraded.pollen.spore.NISP.Neotoma.Pollen.degraded.pollen.spore.NISP.Neotoma.Pollen.degraded.pollen.spore.NISP, Indeterminable,
                                   Indeterminable.Neotoma.Pollen.broken.pollen.spore.NISP.Neotoma.Pollen.broken.pollen.spore.NISP.Neotoma.Pollen.broken.pollen.spore.NISP.Neotoma.Pollen.broken.pollen.spore.NISP, Indeterminable..hidden.,
                                   Indeterminable..crumpled., Indeterminable.Neotoma.Pollen.corroded.pollen.spore.NISP.Neotoma.Pollen.corroded.pollen.spore.NISP.Neotoma.Pollen.corroded.pollen.spore.NISP.Neotoma.Pollen.corroded.pollen.spore.NISP, 
                                   Indeterminable.Neotoma.Pollen.crumpled.pollen.spore.NISP.Neotoma.Pollen.crumpled.pollen.spore.NISP.Neotoma.Pollen.crumpled.pollen.spore.NISP.Neotoma.Pollen.crumpled.pollen.spore.NISP,
                                   Indeterminable..broken., Indeterminable..corroded., Indeterminable.Neotoma.Pollen.hidden.pollen.spore.NISP.Neotoma.Pollen.hidden.pollen.spore.NISP.Neotoma.Pollen.hidden.pollen.spore.NISP.Neotoma.Pollen.hidden.pollen.spore.NISP))


names(bigdf) <- sub("\\.type", "", names(bigdf))
names(bigdf) <- sub("\\cf..", "", names(bigdf))


### Save the file ###
bigdf_familynames <- bigdf
save(bigdf_familynames,file="./Raw_Data/bigdf_familynames.Rda")
