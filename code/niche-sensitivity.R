# select_sensitivity_species <- function(fig_dist_species_multiple_locations_data,
#                                        org_id){
#   # Get species ID from the most common plant and the most common pollinator
#   sp_ids <- fig_dist_species_multiple_locations_data %>%
#     dplyr::filter(!is.na(sp_name)) %$%
#     sp_id
#   
#   
#   
# }

niche_sensitivity <- function(thinned_occurrences, interactions_org, 
                              filled_climate_occ, filled_climate_net,grid_networks, R, 
                              this_org_id, n = 10){
  
  suppressPackageStartupMessages({
    require(data.table)
  })
  # this_org_id <- "org_00842"
  this_occurrences <- thinned_occurrences[org_id == this_org_id]
  this_net_occurrences <- this_occurrences %>%
    dplyr::filter(is.na(countryCode))
  sampling_levels <- logspace(5-nrow(this_net_occurrences), 
                              nrow(this_occurrences) - nrow(this_net_occurrences), n) %>%
    round()
  
  independent <- sampling_levels %>%
    purrr::map(~this_occurrences[sample(.N, .)] ) %>%
    purrr::map(rbind, this_net_occurrences) %>%
    purrr::map_df(~dplyr::mutate(calc_suitability(this_org_id, ., interactions_org, 
                                                  filled_climate_occ, filled_climate_net, 
                                                  grid_networks, R), 
                                 n_occ = nrow(.))) %>%
    plyr::mutate(niche_space = "single_species")
  
  niche_space <- calc_niche_space(filled_climate_occ)
  
  collective <- sampling_levels %>%
    purrr::map(~this_occurrences[sample(.N, .)] ) %>%
    purrr::map(rbind, this_net_occurrences) %>%
    purrr::map_df(~dplyr::mutate(calc_suitability(this_org_id, ., interactions_org, 
                                                  filled_climate_occ, filled_climate_net, 
                                                  grid_networks, R, niche_space), 
                                 n_occ = nrow(.))) %>%
    dplyr::mutate(niche_space = "all_species")
  
  dplyr::bind_rows(independent, collective)
}

logspace <- function(x, y, n) exp(seq(log(x), log(y), length.out = n))

unique_rounded_logspace <- function(x, y, n){
  l <- logspace(x, y, n) %>% round() %>% unique()
  internal_n <- n
  while(length(l) < n){
    internal_n <- internal_n + 1
    l <- logspace(x, y, internal_n) %>% round() %>% unique()
  }
  l
}

mse_subsamples <- function(suitability_subsamples){
  baseline <- 
    suitability_subsamples %>%
    dplyr::filter(n_occ == max(n_occ)) %>%
    dplyr::rename(base_suitability = suitability) %>%
    dplyr::select(-n_occ)
    # split(.$niche_space) %>%

  suitability_subsamples %>%
    ggplot(aes(x = n_occ, y = qlogis(suitability), colour = niche_space, shape = loc_id)) +
    # geom_line() +
    # geom_point() +
    geom_smooth(se = T) +
    scale_x_log10()
  
  dplyr::inner_join(suitability_subsamples, baseline) %>%
    dplyr::group_by(niche_space, n_occ) %>%
    dplyr::summarise(mse = mse(suitability, base_suitability), 
                     mse_logis = mse(qlogis(suitability), qlogis(base_suitability)), 
                     mae = mae(suitability, base_suitability)) %>%
    dplyr::rename(error = mae) %>%
    ggplot(aes(x = n_occ, y = error, colour = niche_space, fill = niche_space)) +
    geom_point() +
    # geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    geom_smooth(method = "gam" , method.args = list(family = "binomial"), 
                formula = y ~ splines::bs(x, 4)) +
    scale_x_log10()
  
}

mse <- function(x, y){
  sum((x - y)^2) / length(x)
}


mae <- function(x, y){
  sum(abs(x - y)) / length(x)
}
