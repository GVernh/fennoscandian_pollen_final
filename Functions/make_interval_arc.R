make_interval_arc = function(dataset, interval){
  dataset$lower_ends = floor(dataset$weighted_mean/interval)*interval
  dataset$find <- c(1)
  dataset <- dataset %>%
    dplyr::group_by(lower_ends) %>%
    dplyr::summarise(
      mean_age = mean(weighted_mean),
      nr_finds = sum(find, na.rm = TRUE))
  dataset <- data.frame(lower_ends = seq(-100, dataset$lower_ends[nrow(dataset)], by = 100)) %>%
    dplyr::full_join(dataset, by = "lower_ends") %>%
    dplyr::mutate(NRfinds_new = na.approx(nr_finds, na.rm=FALSE))
  dataset$NRfinds <- round(dataset$NRfinds_new, digits=0)
  dataset <- dataset[-4]
  dataset <- dataset[-c(103:nrow(dataset)),]
  return(dataset)
}