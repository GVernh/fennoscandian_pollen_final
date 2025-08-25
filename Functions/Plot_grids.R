plot_grids = function(arc, pol, clim){
  ggplot2::ggplot(data = world) +
    geom_sf(fill = "antiquewhite") +
    geom_point(data = arc, aes(x = long, y = lat), size = 4, 
               shape = 21, fill = "darkred") +
    geom_point(data = pol, aes(x = long, y = lat), size = 4, 
               shape = 21, fill = "darkgreen") +
    geom_point(data = clim, aes(x = long, y = lat), size = 4, 
               shape = 21, fill = "darkblue") +
    coord_sf(xlim = c(4, 42), ylim = c(55, 72), expand = FALSE) +
    ggtitle("Fennoscandian datapoints") +
    labs(x = "Longitude",y = "Latitude",fill = "Legend") +
    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.2),
          panel.background = element_rect(fill = "aliceblue"))
}