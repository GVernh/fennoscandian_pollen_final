plot_arc = function(df){
  ggplot(df, aes(x = lower_ends)) + 
    scale_colour_manual(values=c(col1="orange"),labels=c("arc. finds")) +
    geom_line(aes(y = NRfinds,colour="col1")) + 
    labs(x = "Year BP",y = "Nr of finds",colour = "Legend")
}