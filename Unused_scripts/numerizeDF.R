numerizeDF = function(df){
  for(i in 1:ncol(df)){
    df[,i] = as.numeric(df[,i])
  }
  df
}