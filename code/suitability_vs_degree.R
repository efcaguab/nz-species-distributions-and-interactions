ints_as_nets <- function(interactions_org){
  interactions_org %>%
    split(.$loc_id) %>%
    purrr::map(dplyr::select, -loc_id) %>%
    purrr::map(igraph::graph_from_data_frame, directed = FALSE) 

}

get_possible_interactions <- function(interactions_org){
  interactions_org %>%
    dplyr::distinct() %>%
    split(.$loc_id) %>%
    purrr::map(tidyr::complete, pla_id, ani_id) %>%
    purrr::map_dfr(dplyr::rename, int = loc_id, .id = "loc_id") %>%
    dplyr::mutate(int = !is.na(int)) 
}

calc_org_degree <- function(possible_interactions, interactions_org){
  
  possible_interactions <-interactions_org %>%
    dplyr::distinct() %>%
    split(.$loc_id) %>%
    purrr::map(tidyr::complete, pla_id, ani_id) %>%
    purrr::map_dfr(dplyr::rename, int = loc_id, .id = "loc_id") %>%
    dplyr::mutate(int = !is.na(int)) 
  
  possible_degree <- possible_interactions %>%
    dplyr::group_by(pla_id, ani_id) %>%
    dplyr::filter(any(int)) %>% 
    tidyr::gather(key = "guild", 
                  value = "org_id", 
                  ani_id, pla_id) %>%
    dplyr::group_by(org_id) %>%
    dplyr::group_by(loc_id, org_id) %>%
    dplyr::summarise(n_possible_partners = dplyr::n(), 
                     n_partners = sum(int)) 
  
  global_degree <- interactions_org %>% 
    dplyr::distinct() %>%
    tidyr::gather(key = "guild", 
                  value = "org_id", 
                  ani_id, pla_id) %>%
    dplyr::group_by(guild, org_id) %>%
    dplyr::summarise(n_partners_global = dplyr::n())
  
  n_species_guild <- interactions_org %>%
    dplyr::distinct() %>%
    dplyr::group_by(loc_id) %>%
    dplyr::summarise_all(dplyr::n_distinct) %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("pla"), dplyr::contains("ani")), 
                     function(x) paste0("n_", x))

  global_degree %>%
    dplyr::inner_join(possible_degree, by = "org_id") %>%
    dplyr::inner_join(n_species_guild, by = "loc_id") %>%
    dplyr::mutate(n_opposite_guild = dplyr::if_else(guild == "ani_id", 
                                                    n_pla_id, n_ani_id), 
                  n_this_guild = dplyr::if_else(guild == "ani_id", 
                                                n_ani_id, n_pla_id)) %>%
    dplyr::select(-n_pla_id, -n_ani_id)
}


build_analysis_frame <- function(org_degree, independent_suitability){
  org_degree %>%
    dplyr::right_join(independent_suitability,
                      by = c("loc_id", "org_id")) 
}

sample_baseline_population <- function(analysis_frame){
  analysis_frame %$%
    list(N = NROW(.), 
         K = n_partners, )
  dat = list(N = NROW(analysis_frame), 
             K = d$total_tools, P = d$population, C = d$contact_high)
  fit10_10 <- sampling(m10_10, data = dat, iter = 1000, chains = 2, cores = 2)
  
}

define_binomial_models <- function(){
  require(brms)
  
  formula_base <- brmsformula(
    n_partners | trials(n_opposite_guild) ~ 
      scaled_suitability * guild +
      (1 + scaled_suitability | org_id) + (1 | loc_id), 
    family = binomial, 
    center = TRUE
  )
 
  define_alternative_models(formula_base)
}

define_poisson_models <- function(){
  require(brms)
  
  formula_base <- brmsformula(
    n_partners ~ 
      scaled_suitability * guild +
      (1 + scaled_suitability | org_id) + (1 | loc_id), 
    family = poisson, 
    center = TRUE
  )
  
  define_alternative_models(formula_base)
}

define_alternative_models <- function(formula_base){
  
  formula_global <- update(
    formula_base, 
    ~ . + scaled_log_n_partners_global
  )
  
  formula_possible <- update(
    formula_base, 
    ~ . + scaled_n_possible_partners
  )
  
  formula_global_possible <- update(
    formula_base, 
    ~ . + scaled_n_partners_global + scaled_log_n_possible_partners
  )
  
  formula_no_suitability <- update(
    formula_global_possible, 
    ~ . - scaled_suitability - scaled_suitability:guild -
      (1 + scaled_suitability | org_id) +
      (1 | org_id)
  )
  
  list(
    formula_base = formula_base, 
    formula_global = formula_global, 
    formula_possible = formula_possible, 
    formula_global_possible = formula_global_possible, 
    formula_no_suitability = formula_no_suitability
  )
  
}

tinker <- function(){
  library(brms)
  mod <- analysis_frame %>% 
    dplyr::group_by(org_id) %>%
    dplyr::mutate(n_obs = dplyr::n()) %>% 
    dplyr::group_by(org_id) %>%
    dplyr::filter(any(n_partners != n_possible_partners)) %>%
    # dplyr::filter(n_obs > 5) %>%
    # glm(n_partners ~scale(suitability)* guild + 
          # guild + 
          # scale(n_obs) +
          # scale(log(n_partners_global)) +
          # scale(n_possible_partners),
                # family = "poisson",
                # data = .)
    brm(cbind(n_partners, n_opposite_guild) ~ 
                   suitability * guild +
                  # scale(log(n_partners_global)) +
                  # scale(n_possible_partners) +
                  (suitability | org_id) + (1 | loc_id),
                family = "binomial",
                data = .)
  broom::glance(mod)$AIC
  summary(mod)
  
  mod %>%
    lme4::ranef() %>%
    as.data.frame() %>% 
    ggplot(aes(y = condval, x = grp)) +
    geom_point() +
    geom_errorbar(aes(ymin = condval - 2 * condsd, 
                       ymax = condval + 2 * condsd, 
                      colour = sign(condval - 2 * condsd) == sign(condval + 2 * condsd)), 
                   width = 0) +
    facet_wrap(~term, ncol = 2, scales = "free") +
    theme(legend.position = "none") +
    coord_flip()
    lm(n_partners ~ suitability + log(n_partners_global) + n_opposite_guild + n_possible_partners, data = .) %>% 
    summary()
}
