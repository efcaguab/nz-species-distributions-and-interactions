thin_occurrences_per_species <- function(cleaned_occurrences){
  suppressPackageStartupMessages({
    library(data.table)
  })
  
  cleaned_occurrences %>%
    dplyr::inner_join(gbif_key_groups, by = "taxonKey") %>%
    dplyr::inner_join(org_ids, by = c("key_id" = "sp_key_id")) %>%
    data.table::as.data.table() %>%
    split(by = "org_id") %>%
    purrr::map(thin_occurrences) %>%
    rbindlist()
}

get_organisms_ids <- function(gbif_key_groups, gbif_keys, species_ids){
  gbif_key_groups %>%
    dplyr::full_join(gbif_keys, by = c("taxonKey"= "key")) %>%
    dplyr::full_join(species_ids, by = c("queried_sp_name" = "sp_name")) %>% 
    dplyr::distinct(sp_id, key_id) %>%
    tidyr::drop_na() %>%
    # make into a graph to detect components
    igraph::graph_from_data_frame(directed = FALSE) %>%
    igraph::components() %>%
    igraph::groups() %>%
    purrr::map_df(~tibble::tibble(sp_key_id = .), .id = "org_id") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(org_id = stringr::str_pad(org_id, 5, pad = "0"), 
                  org_id = paste("org", org_id, sep = "_"))
}

thin_occurrences <- function(this_sp_occurrences){
  # get latitudes and longitudes
  lon_lats <- this_sp_occurrences %>%
    .[, list(decimalLongitude, decimalLatitude)]
  
  this_sp_occurrences %>%
    # figure out where each coordinate is 
    .[ , wc_grid := raster::cellFromXY(
      worldclim_stack, lon_lats)] %>% 
    # just keep one coordinate per grid
    unique(by = c("wc_grid")) 
}

get_worldclim_stack <- function(worldclim_zip){
  worldclim_zip <- "data/downloads/wordclim_2-5m.zip"
  temp_dir <- file.path(tempdir(), "wordclim")
  unzip(worldclim_zip, exdir = temp_dir)
  file_names <- list.files(temp_dir, full.names = T)
  raster::stack(file_names)
}
