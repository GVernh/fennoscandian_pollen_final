sqrt_sums = function(df){
  df_sqrt <- sqrt(df[2:ncol(df)])
  df_sqrt$age <- df$age
  df_sqrt <- df_sqrt[,c(8,1,3,2,4,5,6,7)]
  return(df_sqrt)
}
