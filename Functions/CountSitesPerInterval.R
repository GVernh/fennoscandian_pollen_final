countSitesPerInterval = function(df,interval_length){
  dfcopy = df
  dfcopy$lower_ends = floor(dfcopy$meantimes/interval_length)*interval_length
  outdf = data.frame(age=unique(dfcopy$lower_ends),NRsites=unique(dfcopy$lower_ends))
  for(i in 1:nrow(outdf)){
    df_restricted = dfcopy[dfcopy$lower_ends==outdf[i,1],]
    outdf[i,2] = length(unique(df_restricted$dataset_ID))
  }
  outdf = outdf[order(outdf[,1]),]
  return(outdf)
}
