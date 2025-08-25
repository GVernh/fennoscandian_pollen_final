#make_spd
arc_spd = function(arc, youngBP){
  arc.caldates <- rcarbon::calibrate(x=arc$Age,errors=arc$Error,calCurves='intcal20') #calibrates the dates
  arc.spd <- rcarbon::spd(arc.caldates,timeRange=c(9700,youngBP)) #aggregates (sums) calibrated dates
  arc.bins <- rcarbon::binPrep(sites=arc$SiteName,ages=arc$Age,h=100) #binning in 100 years
  arc.spd.bins <- rcarbon::spd(arc.caldates,bins=arc.bins,timeRange=c(9700,youngBP))
  #arc.bins.med=binMed(x = arc.caldates,bins=arc.bins)
  spd <- interval_spd(arc.spd.bins)
  return(spd)
}