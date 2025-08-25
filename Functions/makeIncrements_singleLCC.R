makeIncrements_singleLCC = function(df){
  df[1:(nrow(df)),2:ncol(df)]-df[1:(nrow(df)-1),2:ncol(df)]
}