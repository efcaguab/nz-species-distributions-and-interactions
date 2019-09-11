# Prepare workspace -------------------------------------------------------

pkgconfig::set_config("strings_in_dots" = "literals")
library(magrittr)
library(drake)

# load functions
f <- lapply(list.files("code", full.names = T), source)
config_h <- yaml::read_yaml(file_in("config.yaml"))

# cache of sp names
prev_sp_name_assessments_path <- "data/sp_name_checks.csv"
# if file with previous assessments doesn't exist create one
if (!file.exists(prev_sp_name_assessments_path)){
  tibble::tibble("queried_sp_name",
                 "sp_name",
                 "itis",
                 "itis_reason",
                 "itis_tsn",
                 "gnr_score",
                 "gnr_source",
                 "ncbi_kingdom") %>%
    readr::write_csv(path = prev_sp_name_assessments_path, col_names = FALSE)
}

# Configuration -----------------------------------------------------------

configuration_plan <- drake_plan(
  config = yaml::read_yaml(file_in("config.yaml")),
  data_download_date = config$raw_data_retrieved,
  minimum_spp_locations = config$minimum_spp_locations,
  itis_address = config$itis_address,
  ecoregions_address = config$ecoregions_address,
  worldclim_address = config$worldclim_address,
  envirem_address = config$envirem_address,
  envirem_topo_address = config$envirem_topo_address,
  biblio_download_date = config$bibliography_retrieved,
  prev_occ_download_keys = config$gbif_download_key,
  n_chunks = config$n_chunks,
  climate_buffer = config$climate_buffer,
  env_space_resolution = config$env_space_resolution,
  n_subsamples = config$n_subsamples,
  min_suitability_error = config$min_suitability_error,
  brm_cores = config$brm_cores,
  n_markov_iter = config$n_markov_iter
)

# Download data ----------------------------------------------------------

# create download dir if not already there
dir.create("data/downloads", showWarnings = FALSE)

get_web_of_life_pollination_networks_plan <- drake_plan(
  # the targed gets reevaluated if the date in the config file is changed
  wol_pol_networks = get_wol_networks(int_type = "plant-pollinator",
                                      download_date = data_download_date),
  wol_zip_file = download_wol_network_zip(
    wol_pol_networks,
    file_out("data/downloads/web-of-life_plant-pollinator.zip"))
)

get_itis_synonym_database <- drake_plan(
  itis_db = get_file(
    itis_address,
    file_out("data/downloads/itis_sqlite.zip"),
    data_download_date)
)

get_ecoregions_database <- drake_plan(
  ecoregions_shapefile = get_file(
    ecoregions_address,
    file_out("data/downloads/terrestrial-ecoregions.zip"),
    data_download_date
  )
)

get_climate_data <- drake_plan(
  worldclim = get_file(
    worldclim_address,
    file_out("data/downloads/wordclim_2-5m.zip"),
    data_download_date
  ),
  envirem = get_file(
    envirem_address,
    file_out("data/downloads/envirem_2-5m.zip"),
    data_download_date
  ),
  envirem_topo = get_file(
    envirem_topo_address,
    file_out("data/downloads/envirem_topo_2-5m.zip"),
    data_download_date
  )
)

get_data_plan <- rbind(
  get_web_of_life_pollination_networks_plan,
  get_itis_synonym_database,
  get_ecoregions_database,
  get_climate_data
)

# Pre-process interaction data --------------------------------------------

pre_process_wol <- drake_plan(
  wol_data_raw = read_wol_data(
    file_in("data/downloads/web-of-life_plant-pollinator.zip")),
  wol_data = pre_process_wol_data(wol_data_raw),
  wol_spp = get_wol_species_list(wol_data),
  wol_int = get_wol_interaction_list(wol_data)
)

merge_interaction_data_plan <- drake_plan(
  spp = merge_spp(wol_spp),
  spp_no_subspecies = downgrade_subspecies(spp, sp_name),
  synonyms_db = get_synonyms_db(file_in("data/downloads/itis_sqlite.zip")),
  checked_sp_names = check_spp_names(spp_no_subspecies,
                                     synonyms_db,
                                     file_in(prev_sp_name_assessments_path)),
  manual_name_corrections = get_manual_name_corrections(
    file_in("data/manual_sp-name_corrections.csv")),
  checked_manual_corrections = check_spp_names(
    manual_name_corrections,
    synonyms_db,
    file_in(prev_sp_name_assessments_path)),
  problematic_networks = detect_problematic_networks(checked_sp_names,
                                                     spp_no_subspecies),
  species_ids = get_final_name_list(spp_no_subspecies, checked_sp_names,
                                    manual_name_corrections,
                                    checked_manual_corrections, spp),
  int = merge_int(wol_int),
  recoded_interactions = recode_interactions(species_ids, int),
  clean_interactions = remove_problematic_networks(recoded_interactions,
                                                   problematic_networks),
  manual_net_locations = readr::read_csv(
    file_in("data/manual_net_locations.csv"),
    col_types = "cdd"),
  int_metadata = merge_metadata(wol_data, manual_net_locations)
)

pre_process_int_plan <- rbind(
  pre_process_wol,
  merge_interaction_data_plan
)

# Download and clean occurrence data ---------------------------------------

ocurrences_dir <- "data/downloads/spp_occurrences"
occ_download_success_file <- file.path(ocurrences_dir, "download_successful")
occ_extraction_success_file <- file.path(ocurrences_dir, "extraction_successful")
# create download dir if not already there
dir.create(ocurrences_dir, showWarnings = FALSE)

prev_gbif_keys_path <- "data/gbif_keys.csv"
gbif_key_fields <- c('queried_sp_name', 'key', 'canonicalName', 'rank')
create_empty_csv_if_unexistent(prev_gbif_keys_path,  gbif_key_fields)

download_ocurrence_data_plan <- drake_plan(
  spp_to_download = select_species_to_download(species_ids,
                                               clean_interactions,
                                               minimum_spp_locations),
  gbif_keys = get_gbif_keys(spp_to_download,
                            rgbif_key_fields,
                            prev_gbif_keys_path),
  gbif_queries = build_gbif_queries(gbif_keys),
  gbif_download_keys = prepare_gbif_downloads(gbif_queries,
                                              prev_occ_download_keys),
  gbif_download_info = get_gbif_download_info(gbif_download_keys),
  occ_download = download_gbif_ocurrences(
    gbif_download_info,
    ocurrences_dir,
    file_out(occ_download_success_file)),
  occ_extraction =
    extract_occurrence_files(ocurrences_dir,
                             file_out(occ_extraction_success_file),
                             occ_download),
  occ_data_fields = c('taxonKey',
                      'speciesKey',
                      # 'scientificName',
                      # 'acceptedScientificName',
                      'decimalLatitude',
                      'decimalLongitude',
                      # 'geodeticDatum',
                      'countryCode',
                      'individualCount',
                      'coordinateUncertaintyInMeters',
                      'year',
                      'basisOfRecord',
                      # 'issue',
                      'datasetKey',
                      'taxonRank'),
  occurrences = read_occurrences(ocurrences_dir, occ_data_fields,
                                occ_extraction),
  n_occurrences = count_occurrences_per_taxon(occurrences),
  dataset_keys = get_occurrences_datasets(occurrences),
  land_data = rnaturalearth::ne_download(type = "land",
                                         category = "physical",
                                         returnclass = "sp",
                                         scale = 10),
  country_data_sf = rnaturalearth::ne_countries(returnclass = "sf", scale = 10),
  flagged_occurrences = clean_occurrences_chunked(occurrences, land_data, country_data_sf, n_chunks),
  cleaned_occurrences =  flagged_occurrences[.sea_manual & .summary],
  gbif_key_groups = get_gbif_key_groups(cleaned_occurrences),
  n_cleaned_occurrences = count_occurrences_per_taxon(cleaned_occurrences),
  n_dirty_occurrences = get_n_dirty_occurrences(n_occurrences, n_cleaned_occurrences)
)

# Niche analysis ---- -----------------------------------------------------

climatic_niche_plan <- drake_plan(
 org_ids = get_organisms_ids(gbif_key_groups, gbif_keys, species_ids, clean_interactions),
 interactions_org = interactions_as_org_id(clean_interactions, org_ids),
 net_occurrences = get_occurrences_from_networks(org_ids, interactions_org,
                                                 gbif_key_groups,
                                                 int_metadata),
 net_plus_gbif_occurrences = merge_gbif_and_network_occurrences(
   cleaned_occurrences,
   net_occurrences
 ),
 thinned_occurrences = thin_occurrences_per_species(net_plus_gbif_occurrences,
                                                    gbif_key_groups,
                                                    org_ids,
                                                    get_climate()),
 n_thinned_occurrences = count_occurrences_per_organism(thinned_occurrences),
 climate_in_occurrences = get_climate_for_occurrences(thinned_occurrences, get_climate()),
 complete_climate_cells = table(complete.cases(climate_in_occurrences)),
 filled_climate_in_occurrences_1 = fill_missing_values(climate_in_occurrences,
                                                       get_climate(),
                                                       n_chunks,
                                                       climate_buffer),
 complete_filled_cells_1 = table(complete.cases(filled_climate_in_occurrences_1)),
 filled_climate_in_occurrences_2 = fill_missing_values(filled_climate_in_occurrences_1,
                                                       get_climate(),
                                                       n_chunks,
                                                       climate_buffer*2),
 complete_filled_cells_2 = table(complete.cases(filled_climate_in_occurrences_2)),
 grid_networks = get_grid_networks(int_metadata, get_climate()),
 climate_in_networks = get_climate_for_occurrences(grid_networks, get_climate()),
 filled_climate_in_networks_1 = fill_missing_values(climate_in_networks,
                                                    get_climate(),
                                                    1,
                                                    climate_buffer),
 filled_climate_in_networks_2 = fill_missing_values(filled_climate_in_networks_1,
                                                    get_climate(),
                                                    1,
                                                    climate_buffer),
 sensitivity_species_ids = dplyr::arrange(n_thinned_occurrences, dplyr::desc(N))[1, "org_id"],
 sensitivity_species_name = get_sp_name(sensitivity_species_ids,
                                        org_ids, gbif_key_groups, gbif_keys,
                                        species_ids),
 suitability_subsamples = niche_sensitivity(thinned_occurrences,
                                            interactions_org,
                                            filled_climate_in_occurrences_2,
                                            filled_climate_in_networks_2, grid_networks,
                                            R = env_space_resolution,
                                            sensitivity_species_ids[1],
                                            n = n_subsamples),
 error_subsamples = calc_error_subsamples(suitability_subsamples),
 min_occurrences_factor = determine_min_occurrences_per_niche_space(error_subsamples, min_suitability_error),
 enough_occurrences = purrr::map(min_occurrences_factor, ~remove_sp_few_occurrences(thinned_occurrences, .)),
 n_enough_occurrences = purrr::map(enough_occurrences, count_occurrences_per_organism),
 independent_suitability = calc_suitability_independently_all(enough_occurrences$single_species,
                                                              interactions_org,
                                                              filled_climate_in_occurrences_2,
                                                              filled_climate_in_networks_2, grid_networks,
                                                              R = env_space_resolution),
 collective_suitability = calc_suitability_collectivelly_all(enough_occurrences$all_species,
                                                             interactions_org,
                                                             filled_climate_in_occurrences_2,
                                                             filled_climate_in_networks_2,
                                                             grid_networks,
                                                             R = env_space_resolution),
 # grinellian_niche_size = measure_pairwise_ebv_distance(),
 ecoregions = read_ecoregions(file_in("data/downloads/terrestrial-ecoregions.zip"))
)

# Suitability vs. generalism  ----------------------------------------------

suitability_vs_generalism_plan <- drake::drake_plan(
  nets = ints_as_nets(interactions_org),
  possible_interactions = get_possible_interactions(interactions_org),
  org_degree = calc_org_degree(possible_interactions, interactions_org),
  datasets = list(independent_suitability = independent_suitability,
                  collective_suitability = collective_suitability),
  analysis_frames = purrr::map(datasets,
                              ~build_analysis_frame(
                                 org_degree, .,
                                 filter_same_partners = FALSE,
                                 min_obs = 2)),
  anal_frame_distinct = purrr::map(datasets,
                              ~build_analysis_frame(
                                 org_degree, .,
                                 filter_same_partners = TRUE,
                                 min_obs = 2)),
  anal_frame_more_data = purrr::map(datasets,
                                    ~ build_analysis_frame(
                                        org_degree,
                                        .,
                                        filter_same_partners = FALSE,
                                        min_obs = 5)),
  all_datasets = list(analysis_frames = analysis_frames,
                      anal_frame_distinct = anal_frame_distinct,
                      anal_frame_more_data = anal_frame_more_data),
  poisson_formulas = define_poisson_models(),
  binomial_formulas = define_binomial_models(),
  #   binomial_constrained_formulas = define_binomial_constrained_models(),
  model_formulas = list(binomial_formulas = binomial_formulas,
                        #                         binomial_constrained_formulas = binomial_constrained_formulas,
                        poisson_formulas = poisson_formulas),
  formulas_and_data = purrr::map(all_datasets,
                                 ~purrr::cross2(model_formulas, .)),
  models_index = generate_model_index(model_formulas, analysis_frames),
  all_fitted_models = purrr::map(formulas_and_data, fit_all_models, n_markov_iter),
  models = all_fitted_models$analysis_frames,
  chosen_models = get_chosen_model(models, models_index,
                                   chosen_formula_type = "binomial_formulas",
                                   chosen_dataset = "collective_suitability"),
  model_ranking = compare_models(chosen_models),
  baseline_model = chosen_models$formula_base,
  bayesian_r2_baseline = brms::bayes_R2(baseline_model),
  parameter_posterior_summary = get_parameter_posterior_summaries(baseline_model),
  median_trials = get_median_number_trials(baseline_model),
  cond_draws = draw_conditional_fits(baseline_model, median_trials, parameter_scale_attributes),
  random_species_draws = draw_conditional_random_species(baseline_model, median_trials, parameter_scale_attributes),
  random_correlation_posterior = get_posterior_random_correlation(baseline_model),
  parameter_scale_attributes = get_all_pars_scale_attributes(baseline_model),
  mean_parameter_values = purrr::map(parameter_scale_attributes, ~scaled_to_unscaled(0, .)),
  random_sp_names = get_random_sp_names(random_species_draws, org_ids, gbif_key_groups, gbif_keys,
                                        species_ids)
)

# Referencing -------------------------------------------------------------

data_references_plan <- drake_plan(
  interaction_references = get_int_ref_dois(int_metadata, "data/int_data_references.csv")
)

# Figures -----------------------------------------------------------------

figures_plan <- drake_plan(
  world_land = rnaturalearth::ne_download(type = "land",
                                          category = "physical",
                                          returnclass = "sf"),
  fig_worldmap = plot_worldmap(world_land, int_metadata, clean_interactions),
  fig_dist_species_multiple_locations_data = get_dist_species_multiple_locations_data(
    clean_interactions, checked_sp_names, species_ids
  ),
  fig_dist_species_multiple_locations = plot_species_location_distribution(
    fig_dist_species_multiple_locations_data
  ),
  fig_sensitivity_analysis = plot_sensitivity_analysis(error_subsamples,
                                                       min_suitability_error,
                                                       min_occurrences_factor,
                                                       suitability_subsamples),
  fig_conditional_effects = plot_all_conditional_effect(cond_draws, mean_parameter_values),
  fig_random_effects = plot_ranf(random_species_draws, random_correlation_posterior, random_sp_names)
)

# Manuscript --------------------------------------------------------------

reporting_plan <- drake_plan(
  bibliography = target(
    command = get_bibliography("https://raw.githubusercontent.com/efcaguab/phd-bibliography/master/interactions%2Bsdm_manuscript.bib",
                               file_out("paper/bibliography.bib"),
                               biblio_download_date)
  ),
  interaction_bibliography = target(
    command = get_bibliography("https://raw.githubusercontent.com/efcaguab/phd-bibliography/master/interactions%2Bsdm_interaction-data-references.bib",
                               file_out("paper/int-bibliography.bib"),
                               biblio_download_date)
  ),
  interaction_citations = bib2df::bib2df(file_in("paper/int-bibliography.bib")),
  abstract = readLines(file_in("./paper/abstract.md")),
  keywords = process_keywords(file_in("./paper/keywords.md")),
  acknowledgements = readLines(file_in("./paper/acknowledgements.md")),
  intro_line_number = get_line_number(file_in("paper/manuscript.Rmd"), "# Introduction"),
  abs_wordcount = count_words(file_in("paper/abstract.md")),
  msc_wordcount = count_words(file_in('paper/manuscript.Rmd'), lines_to_ignore = 1:intro_line_number),
  n_references = count_references(file_in('paper/manuscript.Rmd'), lines_to_ignore = 1:intro_line_number, refs_to_exclude = "@ref"),
  n_displays = count_displays(file_in('paper/manuscript.Rmd'), lines_to_ignore = 1:intro_line_number),
  msc_title = get_yaml_title(file_in('paper/manuscript.Rmd')),
  supp_info = render_pdf(knitr_in('paper/supp-info.Rmd'), file_out('paper/supp-info.pdf'), clean_md = FALSE),
  draft_info = render_pdf(file_in('paper/draft-info.Rmd'), file_out('paper/draft-info.pdf'), clean_md = FALSE),
  manuscript = render_pdf(knitr_in('paper/manuscript.Rmd'), file_out('paper/manuscript.pdf'), clean_md = FALSE),
  cover_letter = knitr::knit2pdf(knitr_in("paper/cover-letter.Rnw"), output = file_out("paper/cover-letter.tex"))
)


paper_plan <- rbind(
  # full_plan,
  configuration_plan,
  get_data_plan,
  pre_process_int_plan,
  download_ocurrence_data_plan,
  climatic_niche_plan,
  suitability_vs_generalism_plan,
  data_references_plan,
  figures_plan,
  reporting_plan
)

# full_config <- drake_config(full_plan)
# make(paper_plan, parallelism = "clustermq", jobs = 4)
# future::plan(future.callr::callr)
# plan_config <- drake_config(paper_plan)
# vis_drake_graph(plan_config, targets_only = T)
make(paper_plan, lock_envir = FALSE)
