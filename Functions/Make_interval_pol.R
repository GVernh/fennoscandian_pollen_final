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