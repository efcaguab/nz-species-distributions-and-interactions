plot_worldmap <- function(world_land, int_metadata){
  require(ggplot2)
  
  p <- ggplot(world_land) +
    geom_sf(size = 0.5, fill = "antiquewhite", colour = "antiquewhite4") +
    geom_point(data = int_metadata,
               aes(x = lon, y = lat),
               shape = 21,
               fill = "white") +
    coord_sf(expand = FALSE) +
    # pub_theme() +
    theme(panel.grid = element_line(linetype = "dashed", colour = gray(0.5), size = 0.25), 
          axis.title = element_blank(), 
          panel.background = element_rect(fill = "aliceblue"), 
          plot.title = element_text(size = 7, face = "bold", margin = margin(b = 0)),
          plot.subtitle = element_text(size = 7)) +
    labs(title = "Location of interaction networks")
  
  p
}
