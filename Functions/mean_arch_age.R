mean.arch.age = function(calibrated.arch.data, site) {
  ages = calibrated.arch.data[[site]]$ageGrid
  densities = calibrated.arch.data[[site]]$densities
  weighted_mean = stats::weighted.mean(ages, densities)
  mean.arch.age = data.frame(LabID=site, weighted_mean = weighted_mean)
}