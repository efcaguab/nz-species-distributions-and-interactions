
get_scale_attributes <- function(x){
  list(scale = attr(x, 'scaled:scale'), 
       center = attr(x, 'scaled:center'))
}

unscaled_to_scaled <- function(x, scale_attr){
  (x - scale_attr$center) / scale_attr$scale
}

scaled_to_unscaled <- function(x, scale_attr){
  x * scale_attr$scale + scale_attr$center
}



tinker <- function(){
  # model_data <- standata(this_model)
  # conditions <- data.frame(guild = c("pla_id", "ani_id"))
  
  # me <- marginal_effects(this_model, "scaled_grinell_niche_size", conditions = median_trials)
  
  this_model %>%
    spread_draws(b_scaled_suitability, 
                 # b_guildpla_id, 
                 b_scaled_grinell_niche_size, 
                 b_scaled_log_n_partners_global, 
                 b_scaled_n_possible_partners) %>% 
    tidyr::gather("variable", "value", dplyr::starts_with("b_")) %>%
    ggplot(aes(x = value, y = variable)) +
    geom_vline(xintercept = 0, size = 0.25, linetype = 2) +
    stat_intervalh() +
    # stat_pointintervalh(size = 1) +
    # geom_vline(xintercept = 0) +
    # facet_grid(~ variable, scales = "free") +
    scale_color_brewer() +
    pub_theme() +
    theme(legend.position = "none") +
    labs(title = "(b) Effect sizes")
  
  effect <- this_model %>%
    spread_draws(b_scaled_grinell_niche_size) %>% 
    tidyr::gather("variable", "value", dplyr::starts_with("b_")) %>%
    ggplot(aes(x = value)) +
    stat_density(geom = "line") +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25) +
    facet_wrap(~ variable, scales = "free") +
    pub_theme() +
    theme(panel.border = element_blank(), 
          axis.line.x = element_line(), 
          axis.ticks.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.title.y = element_blank()) +
    labs(title = "(b) Effect of environmental niche size")
}
