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
    dplyr::mutate(genus = get_first_word(sp_name)) %>%
    dplyr::inner_join(locality_info, by = "net_name")
}

# return the first word of the string (which is assumed to be the genus in species name)
get_first_word <- function(x){
  gsub(pattern = "\\s.+", 
       replacement = "", 
       x = x)
}

get_interaction_list <- function(networks, metadata){
  
  locality_info <- metadata %>%
    dplyr::select(net_name, loc_name, lat, lon)
  
  networks %>%
    purrr::map_df(interactions_as_df, .id = "net_name")  %>%
    dplyr::mutate(plant_genus = get_first_word(pla), 
                  pol_genus = get_first_word(pol))  %>%
    dplyr::inner_join(locality_info, by = "net_name")
}

interactions_as_df <- function(x){
  as.data.frame.table(x) %>% 
    dplyr::filter(Freq > 0) %>%
    dplyr::mutate_if(is.factor, as.character) %>% 
    `names<-`(c("pla", "pol", "int_weight"))
}
