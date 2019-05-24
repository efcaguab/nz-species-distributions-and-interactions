# Prepare workspace -------------------------------------------------------

pkgconfig::set_config("strings_in_dots" = "literals")

library(magrittr)
library(drake)

# load functions
f <- lapply(list.files("code", full.names = T), source)

# Configuration -----------------------------------------------------------

configuration_plan <- drake_plan(
  config = yaml::read_yaml(file_in("config.yaml")), 
  data_download_date = config$raw_data_retrieved,
  minimum_spp_locations = config$minimum_spp_locations
)

# Get interaction data ----------------------------------------------------

get_web_of_life_pollination_networks_plan <- drake_plan(
  # the targed gets reevaluated if the date in the config file is changed
  wol_pol_networks = get_wol_networks(int_type = "plant-pollinator",
                                      download_date = data_download_date), 
  wol_zip_file = download_wol_network_zip(wol_pol_networks, 
                                          file_out("data/web-of-life_plant-pollinator.zip"))
)

get_data_plan <- rbind(
  get_web_of_life_pollination_networks_plan
)

# Pre-process interaction data --------------------------------------------

pre_process_wol <- drake_plan(
  wol_data_raw = read_wol_data(file_in("data/web-of-life_plant-pollinator.zip")),
  wol_data = pre_process_wol_data(wol_data_raw), 
  wol_spp = get_wol_species_list(wol_data), 
  wol_int = get_wol_interaction_list(wol_data)
)

merge_interaction_data_plan <- drake_plan(
  spp = merge_spp(wol_spp), 
  int = merge_int(wol_int)
)

pre_process_int_plan <- rbind(
  pre_process_wol,
  merge_interaction_data_plan
)

# Download occurrence data ------------------------------------------------

download_ocurrence_data_plan <- drake_plan(
  spp_to_download = select_species_to_download(spp, minimum_spp_locations)
)


# exploration------

read_data_plan <- drake_plan(
  networks = read_networks(network_folder = "./data/raw/web-of-life_2018-10-23_042854"),
  metadata = read_metadata(file_in("./data/raw/web-of-life_2018-10-23_042854/references.csv"))
)

exploration_plan <- drake_plan(
  species_list = get_species_list(networks, metadata),
  interaction_list = get_interaction_list(networks, metadata),
  # the thresholds are specificied on the number of locations a species is
  focal_species_options = get_focal_species_options(species_list, interaction_list, categ = "loc"),
  sp_list_extra_info = get_sp_extra_info(focal_species_options, 10),
  backbones = get_sp_backbones(sp_list_extra_info),
  n_occurrences = get_n_ocurrences(backbones),
  maps = fetch_maps(backbones),
  turnover_exploration = my_render(knitr_in("./paper/data-exploration/turnover.Rmd"),
            file_out("./paper/data-exploration/turnover.md"))
)


reporting_plan <- drake_plan(
  bibliography = target(
    command = get_bibliography("https://raw.githubusercontent.com/efcaguab/phd-bibliography/master/interactions%2Bsdm.bib",
                               file_out("paper/bibliography.bib"))
  ),
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

full_plan <- rbind(
  read_data_plan,
  exploration_plan,
  reporting_plan
)

paper_plan <- rbind(
  full_plan,
  configuration_plan,
  get_data_plan,
  pre_process_int_plan,
  download_ocurrence_data_plan
)



# full_config <- drake_config(full_plan)
make(paper_plan)
