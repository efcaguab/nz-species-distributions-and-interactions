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

  possible_interactions <- interactions_org %>%
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
    dplyr::rename_at(dplyr::vars(dplyr::contains("pla"),
                                 dplyr::contains("ani")),
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

# Get a data frame that can be used to fit the models
build_analysis_frame <- function(org_degree,
                                 independent_suitability,
                                 filter_same_partners = FALSE,
                                 min_obs = 0){
  af <- org_degree %>%
    dplyr::right_join(independent_suitability,
                      by = c("loc_id", "org_id")) %>%
    dplyr::group_by(org_id) %>%
    dplyr::mutate(n_obs = dplyr::n(),
                  grinell_niche_size = KUD_percent_90)

  if(filter_same_partners) {
    af <- af %>%
      dplyr::filter(any(n_partners != n_possible_partners))
  }

  af <- af %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_obs > min_obs) %>%
    dplyr::mutate(scaled_suitability = scale(suitability),
                  log_n_partners_global = log(n_partners_global),
                  scaled_log_n_partners_global = scale(log_n_partners_global),
                  scaled_n_possible_partners = scale(n_possible_partners),
                  scaled_grinell_niche_size = scale(grinell_niche_size))

  # add scale attributes to main frame
  attr(af, "scale_attributes") <- lapply(af, attributes)

  af %>%
    dplyr::filter(!is.na(suitability)) %>%
    dplyr::filter(n_obs > 1)
}

# Return a list of forumlas for the binomial models
define_binomial_models <- function(){
  suppressPackageStartupMessages({
    require(brms)
  })

  formula_base <- brmsformula(
    n_partners | trials(n_opposite_guild) ~ 1,
    family = binomial,
    center = TRUE
  ) %>%
  add_baseline_predictors()

  define_alternative_models(formula_base)
}

add_baseline_predictors <- function(formula_base){
  update(
         formula_base,
          ~ . +
          scaled_suitability * guild +
          scaled_grinell_niche_size * guild +
          scaled_n_possible_partners +
          scaled_log_n_partners_global +
          (1 + scaled_suitability | org_id) + (1 | loc_id)

  )
}

# Return a list of formulas for the poisson models
define_poisson_models <- function(){
  suppressPackageStartupMessages({
    require(brms)
  })

  formula_base <- brmsformula(
    n_partners ~ 1,
    family = poisson,
    center = TRUE
  ) %>%
  add_baseline_predictors()

  define_alternative_models(formula_base)
}

# Given a base formula with suitability return a list of alternative formulas
define_alternative_models <- function(formula_base){

  formula_no_generalism <- update(
    formula_base,
    ~ . - scaled_log_n_partners_global
  )

  formula_no_grinell_niche_size <- update(
    formula_base,
    ~ . - scaled_grinell_niche_size - scaled_grinell_niche_size:guild
  )

  formula_no_possible_partners <- update(
    formula_base,
    ~ . - scaled_n_possible_partners
  )

  formula_no_guild <- update(
    formula_base,
    ~ . - guild - scaled_grinell_niche_size:guild - scaled_suitability:guild
  )

  # formula_no_possible_partners_generalism <- update(
  #   formula_base,
  #   ~ . - scaled_n_possible_partners - scaled_log_n_partners_global
  # )

  formula_no_suitability <- update(
    formula_base,
    ~ . - scaled_suitability - scaled_suitability:guild -
      (1 + scaled_suitability | org_id) +
      (1 | org_id)
  )
  #
  #   formula_triple_interaction <- update(
  #     formula_base,
  #     ~ . + scaled_grinell_niche_size:scaled_suitability
  #   )
  #
  #   formula_triple_interaction_no_niche <- update(
  #     formula_base,
  #     ~ . + scaled_grinell_niche_size:scaled_suitability - scaled_grinell_niche_size - scaled_grinell_niche_size:guild
  #   )
  #
  list(
    formula_base = formula_base,
    formula_no_generalism = formula_no_generalism,
    formula_no_grinell_niche_size = formula_no_grinell_niche_size,
    formula_no_possible_partners = formula_no_possible_partners,
    #     formula_triple_interaction_no_niche = formula_triple_interaction_no_niche,
    # formula_no_possible_partners_generalism = formula_no_possible_partners_generalism,
    formula_no_suitability = formula_no_suitability,
    formula_no_guild = formula_no_guild
    #     formula_triple_interaction = formula_triple_interaction
  )

}


fit_model <- function(formulas, analysis_frame, cores = 1L, iter = 4000){
  suppressPackageStartupMessages({
    library(brms)
    library(future)
  })

  plan(multiprocess)

  formulas %>%
    purrr::map(brm,
               data = analysis_frame,
               prior = c(
                 set_prior("normal(0,10)", class = "b"),
                 set_prior("normal(0,10)", class = "Intercept")
                 # set_prior("lkj(2)", class = "cor"),
                 # set_prior("cauchy(0, 2)", class = "sd")
                 ),
               chains = 4,
               iter = iter,
               # warmup = 2000,
               cores = cores,
               save_all_pars = FALSE,
               # future = TRUE,
               control = list(adapt_delta = 0.99,
                              max_treedepth =12))
}

get_chosen_model <- function(models, models_index, chosen_formula_type, chosen_dataset){
  models_index %>%
    dplyr::filter(formula_type == chosen_formula_type,
                  dataset_type == chosen_dataset) %$%
                  {
                    extract2(models, i)
                  }
}

compare_models <- function(chosen_models){
  model_loo <- chosen_models %>%
    purrr::map(brms::loo)

  model_waic <- chosen_models %>%
    purrr::map(brms::waic)

  # model_kfold <- chosen_models %>%
  #   purrr::map(brms::kfold)
  #
  # model_kfold_org <- chosen_models %>%
  #   purrr::map(brms::kfold, group = "org_id", folds = "grouped")
  #
  # model_kfold_loc <- chosen_models %>%
  #   purrr::map(brms::kfold, group = "loc_id", folds = "grouped")

  list(loo = model_loo,
       waic = model_waic#,
       # model_kfold = model_kfold,
       # model_kfold_org = model_kfold_org,
       # model_kfold_loc = model_kfold_loc
       )
}


sample_baseline_population <- function(analysis_frame){
  analysis_frame %$%
    list(N = NROW(.),
         K = n_partners, )
  dat = list(N = NROW(analysis_frame),
             K = d$total_tools, P = d$population, C = d$contact_high)
  fit10_10 <- sampling(m10_10, data = dat, iter = 1000, chains = 2, cores = 2)

}


