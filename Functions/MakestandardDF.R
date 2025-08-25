makeStandardDF = function(All_Age_Depth_Curves, pollen_all, sitenum)
{
  counts <- pollen_all[[sitenum]]$counts
  meantimes = apply(All_Age_Depth_Curves[[sitenum]]$thetaPredict,2,mean)
  df <- data.frame(meantimes, counts)
  df$dataset_ID = sitenum
  makeStandardDF = df
}
