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
  wol_zip_file = download_wol_network_zip(
    wol_pol_networks, 
    file_out("data/web-of-life_plant-pollinator.zip"))
)

get_data_plan <- rbind(
  get_web_of_life_pollination_networks_plan
)

# Pre-process interaction data --------------------------------------------

pre_process_wol <- drake_plan(
  wol_data_raw = read_wol_data(
    file_in("data/web-of-life_plant-pollinator.zip")),
  wol_data = pre_process_wol_data(wol_data_raw), 
  wol_spp = get_wol_species_list(wol_data), 
  wol_int = get_wol_interaction_list(wol_data)
)

merge_interaction_data_plan <- drake_plan(
  spp = merge_spp(wol_spp), 
  int = merge_int(wol_int), 
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
  world_land = rnaturalearth::ne_download(type = "land", category = "physical", returnclass = "sf"),
  fig_worldmap = plot_worldmap(world_land, int_metadata)
)


paper_plan <- rbind(
  # full_plan,
  configuration_plan,
  get_data_plan,
  pre_process_int_plan,
  download_ocurrence_data_plan, 
  data_references_plan, 
  figures_plan
)



# full_config <- drake_config(full_plan)
make(paper_plan)
