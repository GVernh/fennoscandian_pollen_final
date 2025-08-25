### ecp multivariate change point test ###

# the dataframe needed must be loaded!

library(ecp)
library(ggplot2)


# ecp-function
ecp_divisive_for_site = function(dataset_ID){
  big=bigdf_genera[bigdf_genera$dataset_ID==dataset_ID,]
  big_sorted = big[order(big$meantimes),]
  big_restricted = big_sorted
  big_restricted_nonas = big_restricted[,colSums(is.na(big_restricted)) < nrow(big_restricted)]
  big_noids = subset(big_restricted_nonas,select=-c(meantimes,dataset_ID))
  big_scaled = scale(big_noids,scale = FALSE)
  ecp_divisive_for_site = e.divisive(big_scaled, k = NULL)
  print(big$meantimes[ecp_divisive_for_site$estimates[2]])
  print(big$meantimes[ecp_divisive_for_site$estimates[3]])
  print(big$meantimes[ecp_divisive_for_site$estimates[4]])
}


# plot-function
plot_ecp_site = function(SiteNR, genus1, genus2, genus3){ #the genera names have to be written in quotation marks!
  big = bigdf_genera[bigdf_genera$dataset_ID==SiteNR,]
  big = big[order(big$meantimes),]
  ecp = ecp_divisive_for_site(SiteNR)
  df_for_plotting = data.frame(meantimes = big$meantimes,c1 = big[[genus1]],c2 = big[[genus2]], c3 = big[[genus3]])
  ggplot(df_for_plotting, aes(x = meantimes)) + 
    scale_colour_manual(values=c(col1="green",col2="blue",col3="orange",changepoint="black"),labels=c(genus1,genus2,genus3,"changepoint")) +
    geom_line(aes(y = c1,colour="col1")) + 
    geom_line(aes(y = c2,colour="col2")) +
    geom_line(aes(y = c3,colour="col3")) +
    geom_vline(aes(xintercept = big$meantimes[ecp$estimates[2]], colour = "changepoint")) +
    geom_vline(aes(xintercept = big$meantimes[ecp$estimates[3]], colour = "changepoint")) +
    geom_vline(aes(xintercept = big$meantimes[ecp$estimates[4]], colour = "changepoint")) +
    ggtitle(SiteNR) +
    labs(x = "Year BP",y = "Count Nr",colour = "Legend")
}
