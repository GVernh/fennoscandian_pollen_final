### single LCC mcp (functions and files) ###

#install.packages("plyr")
#install.packages("dplyr")
library(plyr)
library(dplyr)
library(tidyverse)
library(ade4)
library(funrar)
library(ecp)
library(ggplot2)
library(mcp)

#FILES
#coniferous_woodland <- read.csv("coniferous_woodland.csv")
#deciduous_woodland <- read.csv("deciduous_woodland.csv")
#wet_woodland <- read.csv("wet_woodland.csv")
#wet_meadow <- read.csv("wet_meadow.csv")
#pasture <- read.csv("pasture.csv")
#arable <- read.csv("arable.csv")
#heath <- read.csv("heath.csv")

#FUNCTIONS
#CallSites_N
CallSites_N = function(df){
  pol15_1 <-  subset(df, dataset_ID == "20")
  pol15_2 <-  subset(df, dataset_ID == "317")
  pol15_3 <-  subset(df,dataset_ID == "720")
  pol15_4 <-  subset(df,dataset_ID == "4257")
  pol15_5 <-  subset(df,dataset_ID == "4286")
  pol15_6 <-  subset(df,dataset_ID == "4372")
  pol15_7 <-  subset(df,dataset_ID == "4468")
  pol15_8 <-  subset(df,dataset_ID == "20034")
  pol15_9 <-  subset(df,dataset_ID == "20279")
  pol15_10 <-  subset(df,dataset_ID == "20285")
  pol15_11 <-  subset(df,dataset_ID == "20293")
  pol15_12 <-  subset(df,dataset_ID == "44941")
  pol15_13 <-  subset(df,dataset_ID == "45311")
  pol15_14 <-  subset(df,dataset_ID == "45636")
  pol15_15 <-  subset(df,dataset_ID == "45639")
  pol15_16 <-  subset(df,dataset_ID == "45642")
  pol15_17 <-  subset(df,dataset_ID == "24757")
  pol15_18 <-  subset(df,dataset_ID == "4169")
  pol15_all <- rbind(pol15_1,
                     pol15_2,
                     pol15_3,
                     pol15_4,
                     pol15_5,
                     pol15_6,
                     pol15_7,
                     pol15_8,
                     pol15_9,
                     pol15_10,
                     pol15_11,
                     pol15_12,
                     pol15_13,
                     pol15_14,
                     pol15_15,
                     pol15_16,
                     pol15_17,
                     pol15_18)
}

#CallSites_SW
CallSites_SW = function(df){
  pol1_1 <-  subset(df, dataset_ID == "20050")
  pol1_2 <-  subset(df, dataset_ID == "45704")
  pol1_3 <-  subset(df,dataset_ID == "45707")
  pol1_4 <-  subset(df,dataset_ID == "45710")
  pol1_5 <-  subset(df,dataset_ID == "45713")
  pol1_6 <-  subset(df,dataset_ID == "45716")
  pol1_7 <-  subset(df,dataset_ID == "45719")
  pol1_8 <-  subset(df,dataset_ID == "45722")
  pol1_9 <-  subset(df,dataset_ID == "45725")
  pol1_10 <-  subset(df,dataset_ID == "45728")
  pol1_11 <-  subset(df,dataset_ID == "45731")
  pol1_all <- rbind(pol1_1,
                    pol1_2,
                    pol1_3,
                    pol1_4,
                    pol1_5,
                    pol1_6,
                    pol1_7,
                    pol1_8,
                    pol1_9,
                    pol1_10,
                    pol1_11)
}

#CallSites_MW
CallSites_MW = function(df){
  pol2_1 <-  subset(df, dataset_ID == "977")
  pol2_2 <-  subset(df, dataset_ID == "20042")
  pol2_3 <-  subset(df,dataset_ID == "20046")
  pol2_4 <-  subset(df,dataset_ID == "45331")
  pol2_5 <-  subset(df,dataset_ID == "45345")
  pol2_6 <-  subset(df,dataset_ID == "45347")
  pol2_7 <-  subset(df,dataset_ID == "45349")
  pol2_all <- rbind(pol2_1,
                    pol2_2,
                    pol2_3,
                    pol2_4,
                    pol2_5,
                    pol2_6,
                    pol2_7)
}

#CallSites_MM
CallSites_MM = function(df){
  pol47_1 <-  subset(df, dataset_ID == "19906")
  pol47_2 <-  subset(df, dataset_ID == "19909")
  pol47_3 <-  subset(df,dataset_ID == "19913")
  pol47_4 <-  subset(df,dataset_ID == "20018")
  pol47_5 <-  subset(df, dataset_ID == "21790")
  pol47_6 <-  subset(df, dataset_ID == "45351")
  pol47_7 <-  subset(df,dataset_ID == "45698")
  pol47_8 <-  subset(df,dataset_ID == "45701")
  pol47_all <- rbind(pol47_1,
                     pol47_2,
                     pol47_3,
                     pol47_4,
                     pol47_5,
                     pol47_6,
                     pol47_7,
                     pol47_8)
}

#CallSites_M
CallSites_M = function(df){
  pol47_1 <-  subset(df, dataset_ID == "19906")
  pol47_2 <-  subset(df, dataset_ID == "19909")
  pol47_3 <-  subset(df,dataset_ID == "19913")
  pol47_4 <-  subset(df,dataset_ID == "20018")
  pol47_5 <-  subset(df, dataset_ID == "21790")
  pol47_6 <-  subset(df, dataset_ID == "45351")
  pol47_7 <-  subset(df,dataset_ID == "45698")
  pol47_8 <-  subset(df,dataset_ID == "45701")
  pol2_1 <-  subset(df, dataset_ID == "977")
  pol2_2 <-  subset(df, dataset_ID == "20042")
  pol2_3 <-  subset(df,dataset_ID == "20046")
  pol2_4 <-  subset(df,dataset_ID == "45331")
  pol2_5 <-  subset(df,dataset_ID == "45345")
  pol2_6 <-  subset(df,dataset_ID == "45347")
  pol2_7 <-  subset(df,dataset_ID == "45349")
  pol47_all <- rbind(pol47_1,
                     pol47_2,
                     pol47_3,
                     pol47_4,
                     pol47_5,
                     pol47_6,
                     pol47_7,
                     pol47_8,
                     pol2_1,
                     pol2_2,
                     pol2_3,
                     pol2_4,
                     pol2_5,
                     pol2_6,
                     pol2_7)
}

#CallSites_SE
CallSites_SE = function(df){
  pol911_1 <-  subset(df, dataset_ID == "4092")
  pol911_2 <-  subset(df, dataset_ID == "4133")
  pol911_3 <-  subset(df,dataset_ID == "4156")
  pol911_4 <-  subset(df,dataset_ID == "4168")
  pol911_5 <-  subset(df, dataset_ID == "4259")
  pol911_6 <-  subset(df, dataset_ID == "4393")
  pol911_7 <-  subset(df,dataset_ID == "4420")
  pol911_8 <-  subset(df,dataset_ID == "4472")
  pol911_9 <-  subset(df, dataset_ID == "4539")
  pol911_10 <-  subset(df, dataset_ID == "4543")
  pol911_11 <-  subset(df,dataset_ID == "4017")
  pol911_12 <-  subset(df,dataset_ID == "3928")
  pol911_all <- rbind(pol911_1,
                      pol911_2,
                      pol911_3,
                      pol911_4,
                      pol911_5,
                      pol911_6,
                      pol911_7,
                      pol911_8,
                      pol911_9,
                      pol911_10,
                      pol911_11,
                      pol911_12)
}

#CallSites_SM
CallSites_SM = function(df){
  pol36_1 <-  subset(df, dataset_ID == "12")
  pol36_2 <-  subset(df, dataset_ID == "1438")
  pol36_3 <-  subset(df,dataset_ID == "4403")
  pol36_4 <-  subset(df,dataset_ID == "45329")
  pol36_all <- rbind(pol36_1,
                     pol36_2,
                     pol36_3,
                     pol36_4)
}

#CallSites_S
CallSites_S = function(df){
  pol911_1 <-  subset(df, dataset_ID == "4092")
  pol911_2 <-  subset(df, dataset_ID == "4133")
  pol911_3 <-  subset(df,dataset_ID == "4156")
  pol911_4 <-  subset(df,dataset_ID == "4168")
  pol911_5 <-  subset(df, dataset_ID == "4259")
  pol911_6 <-  subset(df, dataset_ID == "4393")
  pol911_7 <-  subset(df,dataset_ID == "4420")
  pol911_8 <-  subset(df,dataset_ID == "4472")
  pol911_9 <-  subset(df, dataset_ID == "4539")
  pol911_10 <-  subset(df, dataset_ID == "4543")
  pol911_11 <-  subset(df,dataset_ID == "4017")
  pol911_12 <-  subset(df,dataset_ID == "3928")
  pol36_1 <-  subset(df, dataset_ID == "12")
  pol36_2 <-  subset(df, dataset_ID == "1438")
  pol36_3 <-  subset(df,dataset_ID == "4403")
  pol36_4 <-  subset(df,dataset_ID == "45329")
  pol911_all <- rbind(pol911_1,
                      pol911_2,
                      pol911_3,
                      pol911_4,
                      pol911_5,
                      pol911_6,
                      pol911_7,
                      pol911_8,
                      pol911_9,
                      pol911_10,
                      pol911_11,
                      pol911_12,
                      pol36_1,
                      pol36_2,
                      pol36_3,
                      pol36_4)
}

#make_interval_pol
make_interval_pol = function(dataset, interval, earliest_date){
  dataset_interval <- dataset
  dataset_interval$lower_ends = floor(dataset_interval$meantimes/interval)*interval
  dataset_interval <- subset(dataset_interval, select = -c(dataset_ID))
  dataset_interval <- dataset_interval %>%
    group_by(lower_ends) %>%
    dplyr::summarise(
      meantimes = mean(meantimes),
      across(2:ncol(dataset_interval)-1,na.rm=TRUE,sum))
  dataset_interval <- dataset_interval[-c(103:nrow(dataset_interval)),]
}


#mcp on LCC
mcp_LCC = function(df){
  mcp_data = df
  model = list(LCC~age+1, 1~age+1)  # two intercept-only segments?
  model_null = list(LCC~age+1)
  fit_mcp = mcp(model, data = mcp_data, par_x = "age")
  fit_mcp_null = mcp(model_null, data = mcp_data, par_x = "age")
  summary(fit_mcp)
  plot(fit_mcp)
}

mcp2_LCC = function(df, runs){
  mcp_data = df
  model = list(LCC~age+1, 1~age+1, 1~age+1)  # three intercept-only segments
  model_null = list(LCC~age+1)
  fit_mcp = mcp(model, data = mcp_data, par_x = "age", adapt = runs)
  fit_mcp_null = mcp(model_null, data = mcp_data, par_x = "age")
  summary(fit_mcp)
  plot(fit_mcp)
}

mcp2_LCC = function(df, runs){
  mcp_data = df
  model = list(LCC~1, ~0+age, ~1+age)  # three intercept-only segments
  model_null = list(LCC~age+1)
  fit_mcp = mcp(model, data = mcp_data, par_x = "age", adapt = runs)
  fit_mcp_null = mcp(model_null, data = mcp_data, par_x = "age")
  summary(fit_mcp)
  plot(fit_mcp)
}

mcp2_LCC = function(df, runs){
  mcp_data = df
  model = list(LCC~1, ~1, ~1)  # three intercept-only segments
  #model_null = list(LCC~age+1)
  fit_mcp = mcp(model, data = mcp_data, par_x = "age", adapt = runs)
  #fit_mcp_null = mcp(model_null, data = mcp_data, par_x = "age")
  summary(fit_mcp)
  plot(fit_mcp)
}

mcp2_LCC_newmodel = function(df, runs){
  mcp_data = df
  model = list(LCC~1, ~0+age, ~0+age)
  #model_null = list(LCC~age+1)
  fit_mcp = mcp(model, data = mcp_data, par_x = "age", adapt = runs)
  #fit_mcp_null = mcp(model_null, data = mcp_data, par_x = "age")
  summary(fit_mcp, digits=0)
  plot(fit_mcp)
}


#fit_mcp$loo = loo(fit_mcp)
#fit_mcp_null$loo = loo(fit_mcp_null)
#loo::loo_compare(fit_mcp$loo, fit_mcp_null$loo)

#make increments
makeIncrements = function(df){
  df_inc <- df[2:(nrow(df)),2:ncol(df)]-df[1:(nrow(df)-1),2:ncol(df)]
  df_inc$lower_ends = df$lower_ends[2:nrow(df)]
  return(df_inc)
}

#make sqrt
sqrt_sums = function(df){
  df_sqrt <- sqrt(df[2:ncol(df)])
  df_sqrt$age <- df$age
  df_sqrt <- df_sqrt[,c(8,1,3,2,4,5,6,7)]
  return(df_sqrt)
}

#make increments
makeIncrements_singleLCC = function(df){
  df[1:(nrow(df)),2:ncol(df)]-df[1:(nrow(df)-1),2:ncol(df)]
}

#ecp_sums
ecp_sums = function(groupdf, df){
  big=groupdf
  big_sorted = big[order(big$age),]
  ecp_divisive_for_site = e.divisive(big_sorted, k = NULL, min.size = 5)
  point1 <- big$age[ecp_divisive_for_site$estimates[2]]
  point2 <- big$age[ecp_divisive_for_site$estimates[3]]
  point3 <- big$age[ecp_divisive_for_site$estimates[4]]
  data.frame(df, point1, point2, point3)
}

#ecp_inc
#ecp_inc = function(groupdf, df){
#  big=groupdf
#  big_sorted = big[order(big$lower_ends),]
#  big_restricted = big_sorted
#  big_restricted_nonas = big_restricted[,colSums(is.na(big_restricted)) < nrow(big_restricted)]
#  big_noids = subset(big_restricted_nonas,select=-c(lower_ends))
#  big_scaled = scale(big_noids,scale = FALSE)
#  ecp_divisive_for_site = e.divisive(big_scaled, k = NULL, min.size = 5)
#  point1 <- big$lower_ends[ecp_divisive_for_site$estimates[2]]
#  point2 <- big$lower_ends[ecp_divisive_for_site$estimates[3]]
#  point3 <- big$lower_ends[ecp_divisive_for_site$estimates[4]]
#  data.frame(df, point1, point2, point3)
#}

#plot_LCC
plot_LCCecp = function(LCC_df, ecp_df, titleinquotes){
  ggplot(LCC_df, aes(x = age)) + 
    scale_colour_manual(values=c(col1="darkgreen",col2="green2", col3="darkslategray3", col4="violet", col5="tan3", col6="red2", col7="blue", changepoint="black") ,
                        labels=c("coniferous_woodland","deciduous_woodland","wet_woodland","pasture/meadow","wet_meadow","arable","heath", "changepoint")) +
    geom_line(aes(y = conN,colour="col1")) + 
    geom_line(aes(y = decN,colour="col2")) + 
    geom_line(aes(y = wetwN,colour="col3")) +
    geom_line(aes(y = wetmN,colour="col4")) + 
    geom_line(aes(y = pasN,colour="col5")) + 
    geom_line(aes(y = araN,colour="col6")) +
    geom_line(aes(y = heaN,colour="col7")) + 
    geom_vline(aes(xintercept = ecp_df$point1, colour = "changepoint")) +
    geom_vline(aes(xintercept = ecp_df$point2, colour = "changepoint")) +
    geom_vline(aes(xintercept = ecp_df$point3, colour = "changepoint")) +
    ggtitle(titleinquotes) +
    labs(x = "Years BP",y = "counts",colour = "Legend")
}

#count taxa per interval
CountTaxaPerInterval = function(df, interval_length){
  dfcopy = coniferous_woodland
  dfcopy$lower_ends = floor(dfcopy$meantimes/interval_length)*interval_length
  dfcopy =subset(dfcopy, select = -c(dataset_ID, meantimes))
  dfcopy <- dfcopy %>% group_by(lower_ends) %>% summarise(
    across(2:ncol(dfcopy)-1,na.rm=TRUE,sum))
  dfcopy2 = subset(dfcopy, select = -c(lower_ends))
  dfcopy2[is.na(dfcopy2)] <- 0
  dfcopy2[dfcopy2 != 0] <- 1
  dfcopy2 = rowSums(dfcopy2)
  outdf = data.frame(dfcopy[1], NRtaxa = dfcopy2)
  return(outdf)
}

#count sites per interval
countSitesPerInterval = function(df,interval_length){
  dfcopy = df
  dfcopy$lower_ends = floor(dfcopy$meantimes/interval_length)*interval_length
  outdf = data.frame(age=unique(dfcopy$lower_ends),NRsites=unique(dfcopy$lower_ends))
  for(i in 1:nrow(outdf)){
    df_restricted = dfcopy[dfcopy$lower_ends==outdf[i,1],]
    outdf[i,2] = length(unique(df_restricted$dataset_ID))
  }
  outdf = outdf[order(outdf[,1]),]
  return(outdf)
}
