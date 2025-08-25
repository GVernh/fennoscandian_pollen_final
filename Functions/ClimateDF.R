climateDF = function(climate, sitenum)
{
  data.frame(site=sitenum, long=climate[[sitenum]]$geo_longitude,
             lat=climate[[sitenum]]$geo_latitude, location=climate[[sitenum]]$geo_gcmdLocation, dataSet=climate[[sitenum]]$dataSetName)
}
