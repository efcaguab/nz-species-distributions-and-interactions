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
    .[n < 100] %>%
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

get_climate <- function(){
  worldclim_stack = get_raster_stack(file_in("data/downloads/wordclim_2-5m.zip"))
  envirem_stack = get_raster_stack(file_in("data/downloads/envirem_2-5m.zip"))
  topo_stack = get_raster_stack(file_in("data/downloads/envirem_topo_2-5m.zip"))
  list(worldclim = worldclim_stack, 
       envirem = envirem_stack, 
       topo = topo_stack)
}

get_climate_for_occurrences <- function(this_occurrences, raster_stacks){
  grids <- this_occurrences$wc_grid %>% 
    unique()
  raster_stacks %>%
    purrr::map(raster::extract, grids) %>%
    purrr::map_dfc(tibble::as_data_frame) %>%
    dplyr::bind_cols(tibble::tibble(wc_grid = grids), .)
}

climatic_pca <- function(climatic_variables){
  climatic_variables %>%
    dplyr::select(-wc_grid) %>%
    ade4::dudi.pca(center = T, scale = T, scannf = F, nf = 2)
}

fill_missing_values <- function(climate_in_occurrences, raster_stacks, n_chunks, buffer, verbose = T){
  
  # subsample for testing
  # climate_in_occurrences <- climate_in_occurrences %>%
    # dplyr::sample_n(1000)
  
  if (verbose) cat("filling topographic values\n")
  topo_averages <- average_climate_buffer(climate_in_occurrences, 
                                          names(raster_stacks$topo), 
                                          raster_stacks$topo, 
                                          n_chunks, 
                                          buffer)
  
  if (verbose) cat("filling worldclim values\n")
  worldclim_averages <- average_climate_buffer(climate_in_occurrences, 
                                               names(raster_stacks$worldclim), 
                                               raster_stacks$worldclim, 
                                               n_chunks, 
                                               buffer)
  
  if (verbose) cat("filling envirem averages\n")
  envirem_averages <- average_climate_buffer(climate_in_occurrences, 
                                             names(raster_stacks$envirem), 
                                             raster_stacks$envirem, 
                                             n_chunks, 
                                             buffer)

  climate_in_occurrences %>%
    fill_averages(topo_averages) %>%
    fill_averages(worldclim_averages) %>%
    fill_averages(envirem_averages)
}  

average_climate_buffer <- function(climate_in_occurrences, pattern, stack, n_chunks, buffer){

  problematic_grids <- climate_in_occurrences %>%
    dplyr::select(tidyselect::one_of(pattern), wc_grid) %>%
    dplyr::filter_all(dplyr::any_vars(is.na(.))) %$%
    wc_grid # %>% 
    # extract(1:10)
  
  # if there are no NA values just don't return anything
  if (length(problematic_grids) == 0) return(NULL)
  
  raster::xyFromCell(stack, problematic_grids) %>%
    raster::extract(stack, ., buffer = buffer,
                    cellnumbers = TRUE, 
                    fun = mean) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(wc_grid = problematic_grids)
}

fill_averages <- function(frame_with_na, averages){
  
  # If there are no averages return the same thing we got
  if (is.null(averages)) return(frame_with_na)
  
  cols <- colnames(averages) %>% head(-1)
  
  for (i in cols) {
    for (j in averages$wc_grid) {
      frame_with_na[frame_with_na$wc_grid == j, i] <- 
        averages[averages$wc_grid == j, i]
    }
  }
  
  return(frame_with_na)
}

  





climatic_pca <- function(climatic_variables){
  climatic_variables %>%
    dplyr::select(-wc_grid) %>%
    ade4::dudi.pca(center = T, scale = T, scannf = F, nf = 2)
}

explore_missing_values <- function(){
  problematic_grids <- climate_in_occurrences %>%
    # dplyr::select(tidyselect::starts_with("current")) %>%
    dplyr::filter_all(dplyr::any_vars(is.na(.))) %$%
    wc_grid
  
  thinned_occurrences[wc_grid %in% problematic_grids] %>%
    ggplot() +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), size = 0.25)
  
  system.time({
    a <- thinned_occurrences[wc_grid %in% problematic_grids] %>%
      .[1:1, c("decimalLongitude", "decimalLatitude")] %>%
      raster::extract(worldclim_stack, ., buffer = 5000, cellnumbers = TRUE, fun = mean)
  })
  
  library(data.table)
  envirem_stack[[1]] %>% plot(col = "black", legend = F)
  
  
  a %>%
    purrr::map(~.[complete.cases(.), ]) %>% View
  
  sample_raster_NA <- function(r, xy){
    apply(X = xy, MARGIN = 1, 
          FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])
    
  }
  
  
  
}

tinker <- function(){
  library(ggplot2)
  thinned_occurrences %>% 
    group_by(wc_grid) %>%
    slice(1) %>%
    ggplot() +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude))
}



tinker <- function(){
  library(ggplot2)
  thinned_occurrences %>% 
    group_by(wc_grid) %>%
    slice(1) %>%
    ggplot() +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude))
}
