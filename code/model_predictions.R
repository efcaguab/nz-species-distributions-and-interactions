
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

draw_conditional_fits <- function(this_model, median_trials, scale_attr){

  suppressPackageStartupMessages({
    require(brms)
    require(tidybayes)
  })

  # To play around
  #   drake::loadd(chosen_models)
  #   drake::loadd(median_trials, parameter_scale_attributes)
  #   scale_attr <- parameter_scale_attributes
  #   this_model <- chosen_models$formula_base

  grinell_niche_size <- median_trials %>%
    tidyr::crossing(scaled_grinell_niche_size = seq(-2, 2, length.out = 10),
                    scaled_suitability = 0,
                    scaled_log_n_partners_global = 0,
                    scaled_n_possible_partners = 0) %>%
    add_fitted_draws(this_model, re_formula = NA, n = 100) %>%
    dplyr::mutate(var = scaled_grinell_niche_size)

  suitability <- median_trials %>%
    tidyr::crossing(scaled_grinell_niche_size = 0,
                    scaled_suitability = seq(unscaled_to_scaled(0, scale_attr$suitability),
                                             unscaled_to_scaled(1, scale_attr$suitability), length.out = 10),
                    scaled_log_n_partners_global = 0,
                    scaled_n_possible_partners = 0) %>%
    add_fitted_draws(this_model, re_formula = NA, n = 100) %>%
    dplyr::mutate(suitability = scaled_to_unscaled(scaled_suitability, scale_attr$suitability),
                  var = suitability)

  generality <- median_trials %>%
    tidyr::crossing(scaled_grinell_niche_size = 0,
                    scaled_suitability = 0,
                    scaled_log_n_partners_global = seq(unscaled_to_scaled(log(1), scale_attr$generality),
                                                       2, #unscaled_to_scaled(2, scale_attr$generality),
                                                       length.out = 20),
                    scaled_n_possible_partners = 0) %>%
    add_fitted_draws(this_model, re_formula = NA, n = 100) %>%
    dplyr::mutate(log_n_partners_global = scaled_to_unscaled(scaled_log_n_partners_global, scale_attr$generality),
                  n_partners_global = exp(log_n_partners_global),
                  var = n_partners_global) %>%
    dplyr::filter(guild == "ani_id")

  possible <- median_trials %>%
    tidyr::crossing(scaled_grinell_niche_size = 0,
                    scaled_suitability = 0,
                    scaled_log_n_partners_global = 0,
                    scaled_n_possible_partners = seq(unscaled_to_scaled(1, scale_attr$possible),
                                                     2,
                                                     length.out = 20)) %>%
    add_fitted_draws(this_model, re_formula = NA, n = 100) %>%
    dplyr::mutate(n_possible_partners = scaled_to_unscaled(scaled_n_possible_partners, scale_attr$possible),
                  var = n_possible_partners ) %>%
    dplyr::filter(guild == "ani_id")

  list(grinell_niche_size = grinell_niche_size,
       suitability = suitability,
       generality = generality,
       possible = possible)
}

get_all_pars_scale_attributes <- function(this_model){
  list(
    niche_size = list(scale = 1, center = 0),
    suitability = get_scale_attributes(this_model$data$scaled_suitability),
    generality = get_scale_attributes(this_model$data$scaled_log_n_partners_global),
    possible = get_scale_attributes(this_model$data$scaled_n_possible_partners)
  )
}

draw_conditional_random_species <- function(this_model, median_trials, scale_attr){

  sp_properties <- this_model$data %>%
    dplyr::group_by(org_id) %>%
    dplyr::mutate(n_obs = dplyr::n_distinct(loc_id)) %>%
    dplyr::distinct(guild,
                    org_id,
                    n_obs,
                    scaled_log_n_partners_global,
                    scaled_grinell_niche_size) %>%
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

  draws <- median_trials %>%
    dplyr::inner_join(sp_properties, by = "guild") %>%
    tidyr::crossing(scaled_suitability = seq(unscaled_to_scaled(0, scale_attr$suitability),
                                             unscaled_to_scaled(1, scale_attr$suitability), length.out = 10),
                    scaled_n_possible_partners = 0) %>%
    tidybayes::add_fitted_draws(this_model, re_formula = ~ (1 + scaled_suitability | org_id), n = 100)

  draws %>%
    dplyr::group_by(guild, org_id, scaled_suitability) %>%
    dplyr::summarise(.value = mean(.value)) %>%
    dplyr::mutate(suitability = scaled_to_unscaled(scaled_suitability, scale_attr$suitability),
                  var = suitability,
                  highlight = org_id %in% sp_to_highlight) %>%
    dplyr::inner_join(slopes, by = c("guild", "org_id")) %>%
    dplyr::group_by(org_id) %>%
    dplyr::mutate(suitability_percent = dplyr::percent_rank(suitability),
                  s_left = abs(suitability_percent - 0.1) == min(abs(suitability_percent - 0.1)),
                  s_right = abs(suitability_percent - 0.9) == min(abs(suitability_percent - 0.9)),
                  mark = value > 0 & s_right | value < 0 & s_left)

}

# Get model random slopes and intercepts
get_pos_random_params <- function(baseline_model){

  sp_properties <- baseline_model$data %>%
    dplyr::group_by(org_id) %>%
    dplyr::mutate(n_obs = dplyr::n_distinct(loc_id)) %>%
    dplyr::distinct(guild,
                    org_id,
                    n_obs) %>%
    dplyr::ungroup()

  tall_posterior <- brms::posterior_summary(baseline_model, pars = "^r_org_id",
                          probs = c(0.025, 0.25, 0.75, 0.975)) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "parameter") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(org_id = stringr::str_sub(string = parameter,
                                          start = 10,
                                          end = 18),
                parameter = stringr::str_sub(string = parameter,
                                             start = 20,
                                             end = -2L))

  c("Estimate", "Q25", "Q75") %>%
    purrr::map_dfc(~ spread_post_summary(tall_posterior, .)) %>%
    dplyr::inner_join(sp_properties, by = "org_id")

}

spread_post_summary <- function(d, variable){

  d %>%
    dplyr::select(org_id, parameter, !!rlang::enquo(variable)) %>%
    tidyr::spread(key = parameter, value = !!rlang::enquo(variable)) %>%
    dplyr::rename_if(is.numeric, paste, rlang::as_character(variable) , sep = "_")


}


get_posterior_random_correlation <- function(baseline_model){

    brms::posterior_samples(baseline_model,
                          pars = c("cor_org_id__Intercept__scaled_suitability")) %>%
    dplyr::mutate(correlation = cor_org_id__Intercept__scaled_suitability)
}

get_random_sp_names <- function(random_species_draws, org_ids, gbif_key_groups, gbif_keys,
                                species_ids){
  random_species_draws %>%
    dplyr::filter(highlight) %$%
    org_id %>%
    unique() %>%
    purrr::map(get_sp_name, org_ids, gbif_key_groups, gbif_keys,
               species_ids) %>%
    purrr::map(dplyr::slice, 1) %>%
    purrr::map_df(dplyr::select, org_id, sp_name)
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
