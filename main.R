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
  interaction_list = get_interaction_list(networks, metadata)
)

full_plan <- rbind(
  read_data_plan, 
  exploration_plan
)

full_config <- drake::drake_config(full_plan)
drake::make(full_plan)
