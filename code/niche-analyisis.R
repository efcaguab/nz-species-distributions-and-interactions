thin_occurrences_per_species <- function(cleaned_occurrences, gbif_key_groups, org_ids, stacks){
  suppressPackageStartupMessages({
    library(data.table)
  })
  
  cleaned_occurrences %>%
    dplyr::inner_join(gbif_key_groups, by = "taxonKey") %>%
    dplyr::inner_join(org_ids, by = c("key_id" = "sp_key_id")) %>%
    data.table::as.data.table() %>%
    split(by = "org_id") %>%
    purrr::map(thin_occurrences, stacks$worldclim) %>%
    # envirem grid number is the same as wordclim so no need to add it independently
    purrr::map(add_stack_grid, stacks$envirem, "en_grid") %>%
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

thin_occurrences <- function(this_sp_occurrences, worldclim_stack){
  # get latitudes and longitudes
 this_sp_occurrences %>%
    add_stack_grid(worldclim_stack, "wc_grid") %>% 
    # just keep one coordinate per grid
    unique(by = c("wc_grid")) 
}

add_stack_grid <- function(this_sp_occurrences, stack, colname){
  # get latitudes and longitudes
  lon_lats <- this_sp_occurrences %>%
    .[, list(decimalLongitude, decimalLatitude)]
  
  this_sp_occurrences %>%
    # figure out where each coordinate is 
    .[ , (colname) := raster::cellFromXY(
      stack, lon_lats)]
}

remove_sp_few_occurrences <- function(thinned_occurrences, min_occurrences = 5){
  thinned_occurrences[, n := .N, by = org_id][] %>%
    .[n >= 5] %>%
    .[, n := NULL]
}

read_ecoregions <- function(shapefile_path){
  extract_dir <- file.path(tempdir(), "ecoregion")
  unzip(shapefile_path, exdir = extract_dir)
  sf::st_read(extract_dir) %>% 
    dplyr::select(ECO_NAME, WWF_MHTNAM, WWF_REALM2) %>% 
    dplyr::transmute('ecoregion' = ECO_NAME, 'major_habitat_type' = WWF_MHTNAM, 'biogeographic_realm'= WWF_REALM2)
}

get_raster_stack <- function(worldclim_zip){
  # worldclim_zip <- "data/downloads/wordclim_2-5m.zip"
  temp_dir <- tempfile()
  unzip(worldclim_zip, exdir = temp_dir)
  file_names <- list.files(temp_dir, full.names = T)
  raster::stack(file_names)
}

merge_stacks <- function(worldclim_stack, envirem_stack, topo_stack){
  extents <- raster_stacks %>%
    purrr::map(raster::extent) 
  
  smallest_extents_index <- extents %>% 
    purrr::map(as.vector) %>%
    purrr::map(abs) %>% 
    purrr::pmap(cbind) %>%
    purrr::map(as.vector) %>%
    purrr::map(which.min)
  
  smallest_extent <- extents %>%
    purrr::map(as.vector) %>%
    purrr::pmap(cbind) %>%
    purrr::map2_dbl(smallest_extents_index, function(x,y) x[y]) %>%
    raster::extent()
  
  worldclim_stack %>%
    purrr::map(~raster::crop(raster::raster(.)), smallest_extent)
  
  stack_names <-     purrr

  1:raster::nlayers(worldclim_stack) %>%
    purrr::map(~raster::subset(worldclim_stack, .)) %>%
    # purrr::map(raster::raster) %>%
    purrr::map(raster::crop, smallest_extent) %>%
    # purrr::map2(names(worldclim_stack), `names<-`) %>%
    # purrr::map(class)
    raster::stack()
    # raster::crop(raster::raster(raster::subset(worldclim_stack, 1)), smallest_extent)
}

crop_raster_stack <- function(stack, extent){
  
}

get_climate_species <- function(this_occurrences, ecoregions){
  this_occurrences <- good_qual_occurrences[org_id == "org_00001"]
  this_occurrences_sf <- sf::st_as_sf(this_occurrences, coords = c("decimalLongitude" ,  "decimalLatitude"), crs = 4326)
  occurrence_ecoregion_overlap <- sf::st_join(this_occurrences_sf, ecoregions , join = sf::st_intersects)
}



tinker <- function(){
  library(ggplot2)
  thinned_occurrences %>% 
    group_by(wc_grid) %>%
    slice(1) %>%
    ggplot() +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude))
}
