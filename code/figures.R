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
                                      suitability_subsamples, 
                                      chosen_niche_space = "all_species"){
  suppressPackageStartupMessages({
    library(ggplot2)
  })

  thresholds <- min_occurrences_factor %>%
    as.data.frame() %>%
    tidyr::gather(key = "niche_space") %>%
    dplyr::filter(niche_space == chosen_niche_space)

  pal <- cgm()$pal_el_green[c(4,7)]

  n_nets <- suitability_subsamples$n_net_occurrences[1]

  e <- error_subsamples %>%
    dplyr::group_by(niche_space) %>%
    dplyr::mutate(error = mae) %>%
    dplyr::filter(niche_space == "all_species")

  p <- e %>%
    ggplot(aes(x = n_occ, y = error)) +
    geom_point(data = dplyr::sample_n(e, 1000),
               shape = 21, alpha = 1, size = 1, colour = "grey30", stroke = 0.25) +
    # geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    geom_smooth(method = "gam" ,
                method.args = list(family = "binomial"),
                formula = y ~ s(x), se = F,
                size = 0.5, colour = "black") +
    geom_hline(yintercept = min_suitability_error, size = 0.25, linetype = 2) +
    geom_vline(data = thresholds,
               aes(xintercept = value*n_nets), size = 0.25, linetype = 2) +
    geom_text(data = thresholds,
              aes(label = paste("~",  value, "occurrences\nper community"),
                  x = value*n_nets),
              y = max(e$error), angle = 90, hjust = 1, vjust = 1.2,
              size = 2.3, lineheight = 0.8) +
    annotate("text", label = "10% error", x = max(e$n_occ), y = 0.1, hjust = 1,
             vjust = -1, size = 2.3) +
    # scale_x_continuous(limits = c(2,35)) +
    scale_x_log10() +
    # scale_fill_manual(values = pal, aesthetics = c("fill", "colour"),
                      # labels = c(" env. space based on all spp. occurrences",
                                 # " env. space based on each spp. occurrences")) +
    pub_theme() +
    theme(legend.position = "none",
          legend.justification = c(1,1),
          legend.title = element_blank()) +
    labs(x = "number of GBIF occurrences",
         y = "mean absolute error",
         title = "error of bioclimatic suitability",
         subtitle = "for a species present in two plant-pollinator communities ")

  # ggsave("plot.pdf", p,  width = unit(width("single"), "in"), height = unit(2.2, "in"))

  p
}

plot_all_conditional_effect <- function(cond_draws, mean_parameter_values){

  suppressPackageStartupMessages({
    require(ggplot2)
  })

  pal <- cgm()$pal_el_green[c(8,7)]

  # grinell_niche_size_plot <- cond_draws$grinell_niche_size %>%
  #   plot_conditional_effect_guild(pal, mean_parameter_values$niche_size) +
  #   labs(title = "(a) environmental niche size",
  #        x = "environmental niche size (scaled)")

  suitability_plot <- cond_draws$suitability %>%
    # Adding 1-x because we want to plot stress not suitability
    dplyr::mutate(var = 1 - var) %>%
    plot_conditional_effect_guild(pal, 1 - mean_parameter_values$suitability) +
    labs(title = "(a) environmental suitability",
         x = "environmental suitability")

  # generality_plot <- cond_draws$generality %>%
  #   plot_conditional_effect_guild(pal, mean_parameter_values$generality, TRUE) +
  #   labs(title = "(c) generality",
  #        x = "# partners across communities")

  possible_plot <- cond_draws$possible %>%
    plot_conditional_effect_guild(pal, mean_parameter_values$possible) +
    labs(title = "(b) possible number of interactions",
         x = "# possible interactions")

  p <- cowplot::plot_grid(#grinell_niche_size_plot,
                          suitability_plot,
                          # generality_plot,
                          possible_plot,
                          ncol = 1,
                          align = "hv", axis = "lt")
  p
  # ggsave("plot.pdf", p,  width = unit(width("single"), "in"), height = unit(2.2*3, "in"))

}

plot_conditional_effect_guild <- function(data, pal, mean_val, log_transformed = FALSE){

  if(log_transformed) mean_val <- exp(mean_val)

   data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(guild = translate_guild(guild, "effect")) %>%
    ggplot(aes(x = var, y = .value, group = interaction(.draw, guild), colour = guild)) +
    geom_vline(xintercept = mean_val, size = 0.25, linetype = 2) +
    geom_line(alpha = 0.15, size = 0.25) +
    geom_line(aes(group = guild), stat = "summary",fun.y = "mean", size = 1) +
    facet_wrap(~guild) +
    scale_fill_manual(values = pal, aesthetics = c("fill", "colour"),
                      labels = c(" env. space based on all spp. occurrences",
                                 " env. space based on each spp. occurrences")) +
    pub_theme() +
    coord_cartesian(expand = F) +
    theme(legend.position = "none") +
    labs(y = "# interactions")
}

plot_ranf <- function(random_species_draws, random_correlation_posterior, random_sp_names, random_slope_intercepts){

  suppressPackageStartupMessages({
    require(ggplot2)
    require(tidybayes)
    require(ggforce)
  })

  pal <- cgm()$pal_el_green[c(8,6)]

  conditional_effects_plot <- random_species_draws %>%
    dplyr::ungroup() %>%
    dplyr::mutate(guild = translate_guild(guild)) %>%
    dplyr::left_join(random_sp_names, by = "org_id") %>%
    ggplot(aes(x = var, y = .value, colour = guild)) +
    geom_line(aes(group = org_id, alpha = highlight, size = highlight), stat = "summary",fun.y = "mean") +
    geom_mark_circle(aes(group = org_id,
                         filter = highlight & mark,
                          label = sp_name),
                     expand = unit(0, "mm"),
                     alpha = 0.1,
                      # concavity = 0,
                      colour = "transparent",
                      fill = "grey",
                      label.fontsize = 7,
                      label.buffer = unit(5, "mm"),
                      label.minwidth = unit(10, "mm"),
                      label.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
                      label.fontface = "italic",
                      con.size = 0.25,
                      con.cap = unit(0.1, "mm"),
                      con.type = "straight") +
    facet_wrap(~guild, ncol = 1) +
    scale_alpha_manual(values = c(0.5, 1)) +
    scale_size_manual(values = c(0.25, 0.5)) +
    scale_fill_manual(values = pal, aesthetics = c("fill", "colour"),
                      labels = c(" env. space based on all spp. occurrences",
                                 " env. space based on each spp. occurrences")) +
    pub_theme() +
    coord_cartesian(expand = F) +
    theme(legend.position = "none") +
    labs(y = "# interactions",
         x = "environmental suitability",
         title = "(a) effect of suitability on individual species")

  mean_correlation <- random_correlation_posterior %>%
    dplyr::summarise_all(mean) %$%
    correlation

  correlation_plot <- random_correlation_posterior %>%
    ggplot(aes(x = correlation)) +
    geom_density(fill = cgm()$pal_el_green[1], colour = NA) +
    stat_density(geom = "line", colour = cgm()$pal_el_green[9], size = 0.25) +
    geom_vline(xintercept = mean_correlation, size = 0.25, linetype = 2) +
    coord_cartesian(expand = FALSE) +
    pub_theme() +
    theme(panel.border = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x.bottom = element_line(),
          plot.title = element_text(margin = margin(b = 4))) +
    labs(title = "(c) correlation coefficient")

  slope_intercept_plot <- random_slope_intercepts %>%
    dplyr::left_join(random_sp_names, by = "org_id") %>%
    dplyr::mutate(mark = !is.na(sp_name)) %>%
    ggplot(mapping = aes(colour = guild,
                         #fill = guild,
                         x = Intercept_Estimate,
                         y = scaled_suitability_Estimate)) +
    geom_hline(yintercept = 0, linetype = 2, size = 0.25) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25) +
    geom_point(shape = 21, stroke = 0.25, size = 1) +
    #     geom_mark_circle(aes(group = org_id,
    #                          filter = mark,
    #                           label = sp_name),
    #                      expand = unit(0, "mm"),
    #                      alpha = 0.1,
    #     concavity = 0,
  #                       colour = "transparent",
  #                       fill = "grey",
  #                       label.fontsize = 7,
  #                       label.buffer = unit(5, "mm"),
  #                       label.minwidth = unit(10, "mm"),
  #                       label.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
  #                       label.fontface = "italic",
  #                       con.size = 0.25,
  #                       con.cap = unit(0.1, "mm"),
  #                       con.type = "straight") +
    #     geom_errorbar(mapping = aes(ymin = scaled_suitability_Q25,
    #                                 ymax = scaled_suitability_Q75),
    #                   size = 0.15, alpha = 0.25) +
    #     geom_errorbarh(mapping = aes(xmin = Intercept_Q25,
    #                              xmax = Intercept_Q75),
    #                size = 0.15, alpha = 0.25) +
    scale_fill_manual(values = pal, aesthetics = c("fill", "colour")) +
    coord_cartesian(expand = TRUE) +
    pub_theme() +
    theme(legend.position = "none") +
    labs(x = "species' intercept",
         y = "species' suitability slope",
         title = "(b) species' specific intercept and suitability slope",
         subtitle = "all values in parameter space")

  plot_left <- conditional_effects_plot

  plot_right <- cowplot::plot_grid(slope_intercept_plot,
                                   correlation_plot,
                                   ncol = 1,
                                   rel_heights = c(3, 1),
                                   axis = "lr",
                                   align = "v")

  p <- cowplot::plot_grid(plot_left,
                          plot_right,
                          ncol = 2)
  #  ggsave("plot.pdf", p, width = unit(width("double"), "in"), height = unit(2.2*2, "in"))

  p
}

plot_suitability <- function(suitability_distribution){

  suppressPackageStartupMessages({
    require(ggplot2)
  })

  p <- suitability_distribution %>%
    ggplot(aes(x = median_suitability, y = loc_id)) +
    geom_point(size = 0.5, stroke = 0.25, colour = cgm()$pal_el_green[9]) +
    geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), size = 0.25,
                   height = 0, alpha = 0.5, colour = cgm()$pal_el_green[9]) +
    scale_x_continuous(expand = c(0,0)) +
    pub_theme() +
    theme(#axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(margin = margin(b = 4)),
          axis.line.x = element_line()) +
    labs(title = "median suitability per community",
         y = "ecological communities",
         x = "habitat suitability")

    #     ggsave("plot.pdf", width = unit(width("single"), "in"), height = unit(2.2*1.5, "in"))

  p
}


