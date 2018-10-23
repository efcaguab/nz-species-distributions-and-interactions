# get species lists by network and locality
get_species_list <- function(networks, metadata){
  
  locality_info <- metadata %>%
    dplyr::select(net_name, loc_name, lat, lon)
  
  plants <- networks %>%
    purrr::map_df(~tibble::data_frame(sp_name = rownames(.)), .id = "net_name") %>%
    dplyr::mutate(guild = "pla")
  pollinators <- networks %>%
    purrr::map_df(~tibble::data_frame(sp_name = colnames(.)), .id = "net_name") %>%
    dplyr::mutate(guild = "pol")
  
  dplyr::bind_rows(plants, pollinators) %>%
    dplyr::mutate(genus = gsub(pattern = "\\s.+", 
                               replacement = "", 
                               x = sp_name)) %>%
    dplyr::inner_join(locality_info, by = "net_name")
}

