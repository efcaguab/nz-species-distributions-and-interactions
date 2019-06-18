plot_worldmap <- function(world_land, int_metadata, clean_interactions){
  require(ggplot2)
  
  valid_locations <- unique(clean_interactions$loc_id)
  
  location_data <- int_metadata %>%
    dplyr::filter(loc_id %in% valid_locations)
  
  p <- ggplot(world_land) +
    geom_sf(fill = cgm()$pal_rb3[2], colour = "antiquewhite4", 
            lwd = 0.25) +
    geom_point(data = location_data,
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
