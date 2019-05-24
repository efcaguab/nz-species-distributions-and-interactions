# get list of species to which download data from rgbif
select_species_to_download <- function(spp, minimum_spp_locations){
  spp %>% 
    dplyr::count(sp_name, sort = T) %>%  
    dplyr::filter(n >= minimum_spp_locations)
}
