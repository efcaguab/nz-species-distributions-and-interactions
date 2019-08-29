
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

get_parameter_posterior_summaries <- function(this_model){
  brms::posterior_summary(this_model) %>% 
    as.data.frame() %>%
    tibble::rownames_to_column(var = "parameter") 
}

get_median_number_trials <- function(model){
  model$data %>%
    dplyr::distinct(guild, n_opposite_guild) %>%
    dplyr::group_by(guild) %>% 
    dplyr::summarise(n_opposite_guild = median(n_opposite_guild))
}

draw_conditional_fits <- function(this_model, median_trials){
  
  suppressPackageStartupMessages({
    require(brms)
    require(tidybayes)
  })
  
  grinell_niche_size <- median_trials %>%
    tidyr::crossing(scaled_grinell_niche_size = seq(-2, 2, length.out = 10), 
                    scaled_suitability = 0, 
                    scaled_log_n_partners_global = 0, 
                    scaled_n_possible_partners = 0) %>%
    add_fitted_draws(this_model, re_formula = NA, n = 100) %>% 
    dplyr::mutate(var = scaled_grinell_niche_size) 
  
  suitability_scale_attr <- get_scale_attributes(this_model$data$scaled_suitability)
  
  suitability <- median_trials %>%
    tidyr::crossing(scaled_grinell_niche_size = 0, 
                    scaled_suitability = seq(unscaled_to_scaled(0, suitability_scale_attr),
                                             unscaled_to_scaled(1, suitability_scale_attr), length.out = 10), 
                    scaled_log_n_partners_global = 0, 
                    scaled_n_possible_partners = 0) %>%
    add_fitted_draws(this_model, re_formula = NA, n = 100) %>%
    dplyr::mutate(suitability = scaled_to_unscaled(scaled_suitability, suitability_scale_attr), 
                  var = suitability) 
  
  generality_scale_attr <- get_scale_attributes(this_model$data$scaled_log_n_partners_global)
  
  generality <- median_trials %>%
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
    dplyr::filter(guild == "ani_id") 
  
  possible_scale_attr <- get_scale_attributes(this_model$data$scaled_n_possible_partners)
  
  possible <- median_trials %>%
    tidyr::crossing(scaled_grinell_niche_size = 0, 
                    scaled_suitability = 0, 
                    scaled_log_n_partners_global = 0, 
                    scaled_n_possible_partners = seq(unscaled_to_scaled(1, possible_scale_attr),
                                                     2, 
                                                     length.out = 20)) %>%
    add_fitted_draws(this_model, re_formula = NA, n = 100) %>%
    dplyr::mutate(n_possible_partners = scaled_to_unscaled(scaled_n_possible_partners, possible_scale_attr), 
                  var = n_possible_partners ) %>%
    dplyr::filter(guild == "ani_id") 
  
  list(grinell_niche_size = grinell_niche_size, 
       suitability = suitability, 
       generality = generality, 
       possible = possible)
}

draw_conditional_random_species <- function(this_model, median_trials){
  
  sp_properties <- this_model$data %>%
    dplyr::group_by(org_id) %>%
    dplyr::mutate(n_obs = dplyr::n_distinct(loc_id)) %>%
    dplyr::distinct(guild, org_id, n_obs, scaled_log_n_partners_global,scaled_grinell_niche_size) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_obs >= 6)
  
  slopes <- brms::posterior_samples(this_model) %>%
    dplyr::select(dplyr::contains("r_org_id")) %>%
    dplyr::select(dplyr::contains("scaled_suitability")) %>% 
    dplyr::select(-dplyr::contains("cor")) %>% 
    tidyr::gather(key = "col", value = "value") %>%
    dplyr::mutate(org_id = substr(col, 10, 18)) %>%
    dplyr::inner_join(sp_properties, by = "org_id") %>%
    dplyr::group_by(guild, org_id) %>%
    dplyr::summarise(value = mean(value)) %>%
    dplyr::group_by(guild) 
  
  sp_to_highlight <- slopes %>%
    dplyr::filter(value == max(value) | value == min(value)) %$% org_id
  
  suitability_scale_attr <- get_scale_attributes(this_model$data$scaled_suitability)
  
  draws <- median_trials %>%
    dplyr::inner_join(sp_properties, by = "guild") %>%
    tidyr::crossing(scaled_suitability = seq(unscaled_to_scaled(0, suitability_scale_attr),
                                             unscaled_to_scaled(1, suitability_scale_attr), length.out = 10), 
                    scaled_n_possible_partners = 0) %>%
    tidybayes::add_fitted_draws(this_model, re_formula = ~ (1 + scaled_suitability | org_id), n = 100)
  
  draws %>%
    dplyr::group_by(guild, org_id, scaled_suitability) %>%
    dplyr::summarise(.value = mean(.value)) %>%
    dplyr::mutate(suitability = scaled_to_unscaled(scaled_suitability, suitability_scale_attr), 
                  var = suitability, 
                  highlight = org_id %in% sp_to_highlight) %>%
    dplyr::inner_join(slopes, by = c("guild", "org_id")) %>%
    dplyr::group_by(org_id) %>%
    dplyr::mutate(suitability_percent = dplyr::percent_rank(suitability), 
                  s_left = abs(suitability_percent - 0.1) == min(abs(suitability_percent - 0.1)), 
                  s_right = abs(suitability_percent - 0.9) == min(abs(suitability_percent - 0.9)), 
                  mark = value > 0 & s_right | value < 0 & s_left) 
   
}

get_posterior_random_correlation <- function(baseline_model){
  
  brms::posterior_samples(baseline_model, pars = c("cor_org_id__Intercept__scaled_suitability")) %>% 
    dplyr::mutate(correlation = cor_org_id__Intercept__scaled_suitability)
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
