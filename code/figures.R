plot_worldmap <- function(world_land, int_metadata){
  require(ggplot2)
  
  p <- ggplot(world_land) +
    geom_sf(fill = cgm()$pal_rb3[2], colour = "antiquewhite4", 
            lwd = 0.25) +
    geom_point(data = int_metadata,
               aes(x = lon, y = lat),
               shape = 21,
               fill = "white", 
               size = 1, 
               color = cgm()$pal_el_green[9]) +
    coord_sf(expand = FALSE) +
    pub_theme() +
    theme(panel.grid = element_line(linetype = "dashed", colour = gray(0.5), size = 0.25), 
          axis.title = element_blank()) +
    labs(title = "Geographic location of pollination communities")
  
  p
}
