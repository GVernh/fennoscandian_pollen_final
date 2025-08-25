compute_relative_abundance = function(raw_pollen){
  (raw_pollen/rowSums(raw_pollen, na.rm = T))
}