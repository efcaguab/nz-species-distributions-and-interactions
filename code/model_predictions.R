make_predictions <- function(this_model){
  suppressPackageStartupMessages({
    require(brms)
    require(tidybayes)
  })
  
  model_data <- standata(this_model)
  median_trials <- this_model$data %>% 
    dplyr::distinct(guild, n_opposite_guild) %>%
    dplyr::group_by(guild) %>% 
    dplyr::summarise(trials = median(n_opposite_guild))
  
  conditions <- data.frame(guild = c("pla_id", "ani_id"))
  
  me <- marginal_effects(this_model, "scaled_grinell_niche_size", conditions = median_trials)
 
  grinell_niche_size_data <- expand.grid(scaled_grinell_niche_size = seq(-2, 2, length.out = 10), 
              n_opposite_guild = 60, 
              scaled_suitability = 0, 
              guild = c("ani_id", "pla_id"), 
              scaled_log_n_partners_global = 0, 
              scaled_n_possible_partners = 0) %>%
    add_fitted_draws(this_model, re_formula = NA, n = 100)
  
  suitability_data <- expand.grid(scaled_grinell_niche_size = 0, 
                                  n_opposite_guild = 60, 
                                  scaled_suitability = seq(-2, 2, length.out = 10), 
                                  guild = c("ani_id", "pla_id"), 
                                  scaled_log_n_partners_global = 0, 
                                  scaled_n_possible_partners = 0) %>%
    add_fitted_draws(this_model, re_formula = NA, n = 100)
  
  suitability_data %>% 
    ggplot(aes(x = scaled_suitability, y = .value, group = interaction(.draw, guild), colour = guild)) +
    geom_line(alpha = 0.1, size = 0.25) +
    geom_line(aes(group = guild), stat = "summary",fun.y = "mean", size = 1) +
    pub_theme()
  
  get_variables(this_model) %>% head
  
  this_model %>%
    spread_draws(b_scaled_suitability, b_guildpla_id) %>% 
    tidyr::gather("variable", "value", dplyr::starts_with("b_")) %>%
    ggplot(aes(x = value)) +
    stat_density(geom = "line") +
    geom_vline(xintercept = 0) +
    facet_wrap(~ variable, scales = "free") +
    pub_theme()
}
