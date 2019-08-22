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
  
}
