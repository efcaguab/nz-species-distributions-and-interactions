# Prepare workspace -------------------------------------------------------

if(!packrat:::isPackratModeOn()) packrat::on()
pkgconfig::set_config("drake::strings_in_dots" = "literals")

library(magrittr)

# load functions
f <- lapply(list.files("code", full.names = T), source)


read_data_plan <- drake::drake_plan(
  networks = read_networks(network_folder = "./data/raw/web-of-life_2018-10-23_042854"),
  metadata = read_metadata(drake::file_in("./data/raw/web-of-life_2018-10-23_042854/references.csv"))
)

exploration_plan <- drake::drake_plan(
  species_list = get_species_list(networks, metadata), 
  interaction_list = get_interaction_list(networks, metadata),
  # the thresholds are specificied on the number of locations a species is
  focal_species_options = get_focal_species_options(species_list, interaction_list, categ = "loc"), 
  sp_list_extra_info = get_sp_extra_info(focal_species_options, 10),
  backbones = get_sp_backbones(sp_list_extra_info),
  n_occurrences = get_n_ocurrences(backbones),
  my_render(drake::knitr_in("./paper/data-exploration/turnover.Rmd"), 
            drake::file_out("./paper/data-exploration/turnover.md"))
)

full_plan <- rbind(
  read_data_plan, 
  exploration_plan
)

full_config <- drake::drake_config(full_plan)
drake::make(full_plan)
