longlatDF = function(pollen_all, sitenum)
{
  data.frame(site=sitenum, long=pollen_all[[sitenum]]$dataset$site.data$long, lat=pollen_all[[sitenum]]$dataset$site.data$lat)
}