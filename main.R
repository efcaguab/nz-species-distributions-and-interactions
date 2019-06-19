# Prepare workspace -------------------------------------------------------

pkgconfig::set_config("strings_in_dots" = "literals")
# pararell environment
options(clustermq.scheduler = "multicore")

library(magrittr)
library(drake)

# load functions
f <- lapply(list.files("code", full.names = T), source)
config_h <- yaml::read_yaml(file_in("config.yaml"))

# cache of sp names
prev_sp_name_assessments_path <- "data/sp_name_checks.csv"
# if file with previous assessments doesn't exist create one
if(!file.exists(prev_sp_name_assessments_path)){
  tibble::tibble("queried_sp_name", "sp_name", "itis", "itis_reason", "itis_tsn", 
         "gnr_score", "gnr_source", "ncbi_kingdom") %>%
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
  biblio_download_date = config$bibliography_retrieved
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
  synonyms_db = get_synonyms_db(file_in("data/downloads/itis_sqlite.zip")), 
  checked_sp_names = check_spp_names(spp, 
                                     synonyms_db, 
                                     file_in(prev_sp_name_assessments_path)), 
  manual_name_corrections = get_manual_name_corrections(
    file_in("data/manual_sp-name_corrections.csv")),
  checked_manual_corrections = check_spp_names(
    manual_name_corrections, 
    synonyms_db, 
    file_in(prev_sp_name_assessments_path)), 
  problematic_networks = detect_problematic_networks(checked_sp_names, spp), 
  species_ids = get_final_name_list(spp, checked_sp_names, 
                                    manual_name_corrections, 
                                    checked_manual_corrections),
  int = merge_int(wol_int),
  recoded_interactions = recode_interactions(species_ids, int),
  clean_interactions = remove_problematic_networks(recoded_interactions, problematic_networks), 
  int_metadata = merge_metadata(wol_data)
)

pre_process_int_plan <- rbind(
  pre_process_wol,
  merge_interaction_data_plan
)

# Download occurrence data ------------------------------------------------

download_ocurrence_data_plan <- drake_plan(
  spp_to_download = select_species_to_download(spp, minimum_spp_locations)
)

# Referencing -------------------------------------------------------------

data_references_plan <- drake_plan(
  interaction_references = get_int_ref_dois(int_metadata), 
  dois_csv = readr::write_csv(interaction_references, 
                              file_out("data/int_data_references.csv"))
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
  )
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
  data_references_plan, 
  figures_plan, 
  reporting_plan
)

# full_config <- drake_config(full_plan)
# make(paper_plan, parallelism = "clustermq", jobs = 4)
make(paper_plan)
