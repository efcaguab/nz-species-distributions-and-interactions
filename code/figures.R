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

# plot distribution of number of species per number of locations
plot_species_location_distribution <- function(clean_interactions, checked_sp_names, species_ids){
  
  suppressPackageStartupMessages({
    require(ggplot2)
    require(ggforce)
  })
  
  pal <- cgm()$pal_el_green[c(4,7)]

  distribution_data <- clean_interactions %>%
    tidyr::gather("guild", "sp_id", pla_id, ani_id) %>%
    dplyr::group_by(guild, sp_id) %>%
    dplyr::summarise(n_locations = dplyr::n_distinct(loc_id)) %>%
    dplyr::group_by(guild, n_locations) %>%
    dplyr::summarise(n_species = dplyr::n_distinct(sp_id), 
                     sp_id = paste(sp_id, collapse = "."))
  
  sp_id_to_highlight <- distribution_data %>%
    dplyr::group_by(guild) %>%
    dplyr::filter(n_species == 1, 
                  n_locations == max(n_locations)
                  ) %$%
    sp_id
  
  species_to_highlight <- species_ids %>%
    dplyr::filter(sp_id %in% sp_id_to_highlight)
  
  distribution_data_highlight <- distribution_data %>%
    dplyr::inner_join(species_to_highlight, by = "sp_id") %>%
    dplyr::inner_join(checked_sp_names, by = "sp_name") %>%
    dplyr::filter(gnr_score >= 0.98)
  
   p <- distribution_data %>%
    dplyr::left_join(distribution_data_highlight, 
                     by = c("guild", "n_locations", "n_species", "sp_id")) %>%
    ggplot(aes(x = n_locations, y = n_species, fill = guild)) +
    geom_line(aes(colour = guild), linetype = 3, size = 0.5) +
    geom_point(shape = 21, size = 1) +
    # geom_text_repel(data = distribution_data_highlight, aes(label = sp_name)) +
    geom_mark_circle(aes(label = sp_name, filter = !is.na(sp_name), group = sp_name), 
                     expand = unit(0.1, "mm"), 
                     alpha = 0.1,
                     colour = NA, 
                     label.fontsize = 7, 
                     label.buffer = unit(5, "mm"),
                     label.minwidth = unit(10, "mm"),
                     label.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
                     label.fontface = "italic", 
                     con.size = 0.25, 
                     con.cap = unit(1, "mm"), 
                     con.type = "straight") +
    scale_fill_manual(values = pal, aesthetics = c("fill", "colour"), 
                      labels = c("animals", "plants")) +
    scale_y_log10(labels = scales::number_format(big.mark = ",")) +
    scale_x_log10(breaks = c(1,2, 4, 8, 16, 32)) +
    pub_theme() +
    theme(legend.position = c(0.95,0.95), 
          legend.justification = c(1,1), 
          legend.title = element_blank()) +
    labs(x = "number of locations", 
         y = "numver of species",
         title = "distribution of species per locations")
    
  # ggsave("plot.pdf", p,  width = unit(width("single"), "in"), height = unit(2, "in"))
   p
}
