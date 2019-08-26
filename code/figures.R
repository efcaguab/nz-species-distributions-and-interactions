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
plot_species_location_distribution <- function(dist_species_multiple_locations_data){
  
  suppressPackageStartupMessages({
    require(ggplot2)
    require(ggforce)
  })
  
  pal <- cgm()$pal_el_green[c(4,7)]
  
   p <- dist_species_multiple_locations_data %>%
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
         y = "frequency (# species)",
         title = "number of species at multiple locations", 
         subtitle = "frequency distribution")
    
  # ggsave("plot.pdf", p,  width = unit(width("single"), "in"), height = unit(2.2, "in"))
   p
}


get_dist_species_multiple_locations_data <- function(clean_interactions, checked_sp_names, species_ids){
  
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
  
  distribution_data %>%
    dplyr::left_join(distribution_data_highlight, 
                     by = c("guild", "n_locations", "n_species", "sp_id")) 
  
}

plot_sensitivity_analysis <- function(error_subsamples, min_suitability_error, 
                                      min_occurrences_factor, 
                                      suitability_subsamples){
  suppressPackageStartupMessages({
    library(ggplot2)
  })
  
  thresholds <- min_occurrences_factor %>% 
    as.data.frame() %>% 
    tidyr::gather(key = "niche_space")
  
  pal <- cgm()$pal_el_green[c(4,7)]
  
  n_nets <- suitability_subsamples$n_net_occurrences[1]
  
  e <- error_subsamples %>%
    dplyr::group_by(niche_space) %>%
    dplyr::mutate(error = mae)
  
  p <- e %>%
    ggplot(aes(x = n_occ, y = error, fill = niche_space)) +
    geom_point(data = dplyr::sample_n(e, 1000),
               shape = 21, alpha = 1, size = 1) +
    # geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    geom_smooth(aes(colour = niche_space), method = "gam" ,
                method.args = list(family = "binomial"),
                formula = y ~ s(x), se = T,
                size = 0.5) +
    geom_hline(yintercept = min_suitability_error, size = 0.25, linetype = 2) +
    geom_vline(data = thresholds, 
               aes(xintercept = value*n_nets, colour = niche_space), size = 0.5, linetype = 2) +
    # scale_x_continuous(limits = c(2,35)) +
    scale_x_log10() +
    scale_fill_manual(values = pal, aesthetics = c("fill", "colour"), 
                      labels = c(" env. space based on all spp. occurrences", 
                                 " env. space based on each spp. occurrences")) +
    pub_theme() +
    theme(legend.position = c(0.95,0.95), 
          legend.justification = c(1,1), 
          legend.title = element_blank()) +
    labs(x = "number of GBIF occurrences", 
         y = "mean absolute error",
         title = "error of environmental suitability of communities", 
         subtitle = "for a species present in two plant-pollinator communities ")
  
  # ggsave("plot.pdf", p,  width = unit(width("single"), "in"), height = unit(2.2, "in"))
  
  p
}

plot_all_conditional_effect <- function(this_model){
  
  suppressPackageStartupMessages({
    require(brms)
    require(tidybayes)
    require(ggplot2)
  })
  
  
  median_trials <- this_model$data %>%
    # dplyr::distinct(org_id, loc_id, .keep_all = F) %>% nrow
    dplyr::distinct(guild, n_opposite_guild) %>%
    dplyr::group_by(guild) %>% 
    dplyr::summarise(n_opposite_guild = median(n_opposite_guild))
  
  grinell_niche_size_plot <- median_trials %>%
    tidyr::crossing(scaled_grinell_niche_size = seq(-2, 2, length.out = 10), 
                    scaled_suitability = 0, 
                    scaled_log_n_partners_global = 0, 
                    scaled_n_possible_partners = 0) %>%
    add_fitted_draws(this_model, re_formula = NA, n = 100) %>% 
    dplyr::mutate(var = scaled_grinell_niche_size) %>%
    plot_conditional_effect_guild() +
    labs(title = "(a) Effect of environmental niche size", 
         x = "environmental niche size (scaled)")
  
  suitability_scale_attr <- get_scale_attributes(this_model$data$scaled_suitability)
  
  suitability_plot <- median_trials %>%
    tidyr::crossing(scaled_grinell_niche_size = 0, 
                    scaled_suitability = seq(unscaled_to_scaled(0, suitability_scale_attr),
                                             unscaled_to_scaled(1, suitability_scale_attr), length.out = 10), 
                    scaled_log_n_partners_global = 0, 
                    scaled_n_possible_partners = 0) %>%
    add_fitted_draws(this_model, re_formula = NA, n = 100) %>%
    dplyr::mutate(suitability = scaled_to_unscaled(scaled_suitability, suitability_scale_attr), 
                  var = suitability) %>%
    plot_conditional_effect_guild() +
    labs(title = "(a) Environmental suitability", 
         x = "environmental suitability")
  
  generality_scale_attr <- get_scale_attributes(this_model$data$scaled_log_n_partners_global)
  
  generality_plot <- median_trials %>%
    tidyr::crossing(scaled_grinell_niche_size = 0, 
                    scaled_suitability = 0, 
                    scaled_log_n_partners_global = seq(unscaled_to_scaled(log(1), generality_scale_attr),
                                                       unscaled_to_scaled(log(50), generality_scale_attr), 
                                                       length.out = 20), 
                    scaled_n_possible_partners = 0) %>%
    add_fitted_draws(this_model, re_formula = NA, n = 100) %>%
    dplyr::mutate(log_n_partners_global = scaled_to_unscaled(scaled_log_n_partners_global, generality_scale_attr), 
                  n_partners_global = exp(log_n_partners_global),
                  var = n_partners_global) %>%
    dplyr::filter(guild == "ani_id") %>%
    plot_conditional_effect_guild() +
    labs(title = "(c) Generality", 
         x = "environmental suitability")
  
  possible_scale_attr <- get_scale_attributes(this_model$data$scaled_n_possible_partners)
  
  possible_plot <- median_trials %>%
    tidyr::crossing(scaled_grinell_niche_size = 0, 
                    scaled_suitability = 0, 
                    scaled_log_n_partners_global = 0, 
                    scaled_n_possible_partners = seq(unscaled_to_scaled(1, possible_scale_attr),
                                                     2, 
                                                     length.out = 20)) %>%
    add_fitted_draws(this_model, re_formula = NA, n = 100) %>%
    dplyr::mutate(n_possible_partners = scaled_to_unscaled(scaled_n_possible_partners, possible_scale_attr), 
                  var = n_possible_partners ) %>%
    dplyr::filter(guild == "ani_id") %>%
    plot_conditional_effect_guild() +
    labs(title = "(d) Possible number of partners", 
         x = "# possible interactions")
  
  cowplot::plot_grid(suitability_plot, 
                     grinell_niche_size_plot,
                     generality_plot,
                     possible_plot,
                     ncol = 1, 
                     align = "hv", axis = "lt", 
                     rel_widths = c(2,1))
}
