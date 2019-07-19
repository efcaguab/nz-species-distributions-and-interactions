# Species ids are a big mess, so this function combines information from the
# GBIF keys and the species IDs from the networks to determine a single organism
# id which identifies organisms at the species level across datasets and
# analysis
get_organisms_ids <- function(gbif_key_groups, gbif_keys, species_ids,
                              clean_interactions){
  
  # Find "components" of species ids and gbif keys that belong to the same group
  orgs_in_gbif <- gbif_key_groups %>%
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
  
  used_org_ids <- orgs_in_gbif %>%
    dplyr::distinct(org_id) %>%
    tidyr::separate(org_id, c(NA, "org_number"), sep = "_") %>%
    dplyr::mutate(org_number = as.numeric(org_number)) %$%
    org_number %>% 
    max
  
  # Add info for organism that were not found in GBIF but deserve some love
  other_orgs <- clean_interactions %>%
    tidyr::gather(key = "guild", 
                  value = "sp_id", 
                  pla_id, ani_id) %>%
    dplyr::filter(! sp_id %in% orgs_in_gbif$sp_key_id) %>%
    dplyr::distinct(sp_id) %>%
    dplyr::mutate(org_id = 1:dplyr::n() + used_org_ids,
                  org_id = stringr::str_pad(org_id, 5, pad = "0"), 
                  org_id = paste("org", org_id, sep = "_")) %>%
    dplyr::rename(sp_key_id = sp_id)
  
  dplyr:::bind_rows(orgs_in_gbif, other_orgs) 
}


# Observation of species in the networks also counts as occurrences that can be
# used for the niche suitability analysis. This function takes the network data
# and reshape it so that it can be appended to GBIF occurrences for further
# analysis. Only species for which data was found in GBIF are included
get_occurrences_from_networks <- function(org_ids, interactions_org, 
                                          gbif_key_groups, int_metadata){
  # Get interaction networks coded by organism id
  interactions_org %>%
    tidyr::gather(key = "guild", 
                  value = "org_id", 
                  ani_id, pla_id) %>%
    dplyr::distinct(loc_id, org_id) %>%
    # Find the GBIF keys for those organisms
    dplyr::inner_join(org_ids, by = "org_id") %>%
    dplyr::filter(stringr::str_detect(sp_key_id, "key")) %>%
    dplyr::rename(key_id = sp_key_id) %>%
    dplyr::inner_join(gbif_key_groups, by = "key_id") %>%
    dplyr::inner_join(int_metadata, by = "loc_id") %>%
    dplyr::select(taxonKey, decimalLatitude = lat, decimalLongitude = lon) %>%
    dplyr::distinct()
}

# Merge GBIF and network occurrences
merge_gbif_and_network_occurrences <- function(cleaned_occurrences, 
                                               net_occurrences) {
  suppressPackageStartupMessages({
    library(data.table)
  })
  rbind(as.data.table(cleaned_occurrences), as.data.table(net_occurrences), 
        fill = TRUE)
}

# Loop over evert species (organism id) and thin the occurrences fpr that species
thin_occurrences_per_species <- function(cleaned_occurrences, gbif_key_groups, 
                                         org_ids, stacks){
  suppressPackageStartupMessages({
    library(data.table)
  })
  
  # Occurrences are coded by taxonKey so need to find the org_ids first
  cleaned_occurrences %>%
    dplyr::inner_join(gbif_key_groups, by = "taxonKey") %>%
    dplyr::inner_join(org_ids, by = c("key_id" = "sp_key_id")) %>%
    data.table::as.data.table() %>%
    split(by = "org_id") %>%
    purrr::map(thin_occurrences, stacks$worldclim) %>%
    # envirem grid number is the same as wordclim so no need to add it
    # independently
    purrr::map(add_stack_grid, stacks$envirem, "en_grid") %>%
    rbindlist()
}

# Given a species and a climatic grid, keep only one of the occurrences in each
# grid
thin_occurrences <- function(this_sp_occurrences, worldclim_stack){
  # get latitudes and longitudes
 this_sp_occurrences %>%
    # Find out the cell number (grid) in which the occurrence is recorded
    add_stack_grid(worldclim_stack, "wc_grid") %>% 
    # just keep one coordinate per grid
    unique(by = c("wc_grid")) 
}

# Given some occurrences find out the cell number (grid) in which the occurrence
# is recorded.
# stack = the Raster stack with the climatic grid info
# colname = the string with the name of the column where the grid number will be
# recorded
add_stack_grid <- function(this_sp_occurrences, stack, colname){
  # get latitudes and longitudes
  lon_lats <- this_sp_occurrences %>%
    .[, list(decimalLongitude, decimalLatitude)]
  
  this_sp_occurrences %>%
    # figure out where each coordinate is 
    .[ , (colname) := raster::cellFromXY(
      stack, lon_lats)]
}

# Remove species (org_id) with less occurrences than min_occurrences
remove_sp_few_occurrences <- function(thinned_occurrences, min_occurrences = 5){
  thinned_occurrences[, n := .N, by = org_id][] %>%
    .[n >= min_occurrences] %>%
    .[, n := NULL]
}

# Function to read ecoregions from a zipped shapefile path and return as Spatial
# Data Frame (I think)
read_ecoregions <- function(shapefile_path){
  extract_dir <- file.path(tempdir(), "ecoregion")
  unzip(shapefile_path, exdir = extract_dir)
  sf::st_read(extract_dir) %>% 
    dplyr::select(ECO_NAME, WWF_MHTNAM, WWF_REALM2) %>% 
    dplyr::transmute('ecoregion' = ECO_NAME, 'major_habitat_type' = WWF_MHTNAM, 
                     'biogeographic_realm'= WWF_REALM2)
}

# Read raster stack from a zip containing multiple rasters
get_raster_stack <- function(worldclim_zip){
  # worldclim_zip <- "data/downloads/wordclim_2-5m.zip"
  temp_dir <- tempfile()
  unzip(worldclim_zip, exdir = temp_dir)
  file_names <- list.files(temp_dir, full.names = T)
  raster::stack(file_names)
}

# Get a list of raster stacks from all three environmental zip files (worldclim,
# envirem and topography)
get_climate <- function(){
  worldclim_stack = get_raster_stack(file_in("data/downloads/wordclim_2-5m.zip"))
  envirem_stack = get_raster_stack(file_in("data/downloads/envirem_2-5m.zip"))
  topo_stack = get_raster_stack(file_in("data/downloads/envirem_topo_2-5m.zip"))
  list(worldclim = worldclim_stack, 
       envirem = envirem_stack, 
       topo = topo_stack)
}

# Given a list of occurrences (with raster grid information) return a frame with
# the climate vector for that occurrence
get_climate_for_occurrences <- function(this_occurrences, raster_stacks){
  # get the grids of the occurrences
  grids <- this_occurrences$wc_grid %>% 
    unique()
  # Extract climate for each of the stacks and put together as a data frame
  raster_stacks %>%
    purrr::map(raster::extract, grids) %>%
    purrr::map_dfc(tibble::as_data_frame) %>%
    dplyr::bind_cols(tibble::tibble(wc_grid = grids), .)
}

# this function fills empty climatic values for every stack independently by
# finding the mean climate for each stack
fill_missing_values <- function(climate_in_occurrences, raster_stacks, n_chunks,
                                buffer, verbose = T){
  
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

  # Replace the NAs with the calculated means
  climate_in_occurrences %>%
    fill_averages(topo_averages) %>%
    # dplyr::filter(wc_grid %in% problematic_grids) %>% View
    fill_averages(worldclim_averages) %>%
    fill_averages(envirem_averages)
}  

# Some cells of the ocurrences have no climatic information, this function finds
# out the mean climate of the cells within a buffer (in meters) arround the
# occurrence for a given stacks. Divides the problem into n_chunks to make it a
# bit less memory intensive
average_climate_buffer <- function(climate_in_occurrences, pattern, stack, n_chunks, buffer){

  # First find out which are the problematic cells
  problematic_grids <- climate_in_occurrences %>%
    dplyr::select(tidyselect::one_of(pattern), wc_grid) %>%
    dplyr::filter_all(dplyr::any_vars(is.na(.))) %$%
    wc_grid # %>% 
    # extract(1:10)
  
  # if there are no NA values just don't return anything
  if (length(problematic_grids) == 0) return(NULL)
  
  # Extract climate from neighbouring cells and format as a data frame
  raster::xyFromCell(stack, problematic_grids) %>%
    raster::extract(stack, ., buffer = buffer,
                    cellnumbers = TRUE, 
                    fun = function(x) mean(x, na.rm = TRUE)) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(wc_grid = problematic_grids)
}

# This function takes the frame with climate for occurrences and fills the
# missing values with the a set of values previously calculated
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


count_occurrences_per_organism <- function(x){
  x[, .N, by = org_id]
}

# Transform networks (which are coded by species ids) so that they are coded as
# organism ids, which is the way species are best identified
interactions_as_org_id <- function(clean_interactions, org_ids){
  clean_interactions %>%
    dplyr::left_join(org_ids, by = c("pla_id" = "sp_key_id")) %>% 
    dplyr::left_join(org_ids, by = c("ani_id" = "sp_key_id")) %>%
    dplyr::select(loc_id, pla_id = org_id.x, ani_id = org_id.y)
}

# Get the climate grid (cells) for each of the network locations
get_grid_networks <- function(int_metadata, stacks){
  suppressPackageStartupMessages({
    require(data.table)
  })
  
  int_metadata %>%
    dplyr::group_by(loc_id) %>% 
    dplyr::summarise(decimalLongitude = mean(lon), 
                     decimalLatitude = mean(lat)) %>%
    dplyr::filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>%
    as.data.table() %>%
    add_stack_grid(stacks$worldclim, "wc_grid") 
}

# Given some locations (a data frame with grid, get the climate for each row)
get_climate_cells <- function(locations, filled_climate){
  locations %>%
    dplyr::select(wc_grid) %>%
    dplyr::left_join(filled_climate, by = "wc_grid")
}

# Calculate suitability of all species using niche spaces calculated using the
# occurrences of each species independently
calc_suitability_independently_all <- function(thinned_occurrences,
                                               interactions_org, 
                                               filled_climate_occ,
                                               filled_climate_net,
                                               grid_networks, R){
  suppressPackageStartupMessages({
    require(data.table)
  })
  
  # For each species calculate the suitability
  thinned_occurrences$org_id %>%
    unique() %>%
    purrr::map_df(calc_suitability, 
                  thinned_occurrences, interactions_org, 
                  filled_climate_occ, filled_climate_net, grid_networks, R)
}

# Calculate suitability of all species using a single niche space for all
# calculated using the occurrences of all species together
calc_suitability_collectivelly_all <- function(thinned_occurrences, 
                                               interactions_org, 
                                               filled_climate_occ,
                                               filled_climate_net,
                                               grid_networks,
                                               R){
  suppressPackageStartupMessages({
    require(data.table)
  })
  
  niche_space <- calc_niche_space(filled_climate_occ)
  
  thinned_occurrences$org_id %>%
    unique() %>%
    purrr::map_df(calc_suitability, 
                  thinned_occurrences, interactions_org, 
                  filled_climate_occ, filled_climate_net, grid_networks, R, 
                  niche_space)
}

# Calculate the suitability of a species at cells given by filled_climate_occ
# based on the occurrences found in thined_occurrences. Uses a kernel density
# estimate with a resolution of R. If no background niche (a dudi pca object) is
# provided it calculate one using the occurrences of the species itself
calc_suitability <- function(
  this_sp, thinned_occurrences, interactions_org, 
  filled_climate_occ, filled_climate_net, grid_networks, R, niche_space = NULL,
  verbose = T){
  
  if(verbose) cat("Calculating suitability for", this_sp, "\n")
  
  # Filter the occurrences that correspond to the species
  this_occurrences <- thinned_occurrences[org_id == this_sp]
  
  # Get the network locations where the species is present
  this_net_locations <- interactions_org %>%
    tidyr::gather("guild", "org_id", pla_id, ani_id) %>%
    dplyr::filter(org_id == this_sp) %$%
    unique(loc_id)
  
  # If the species is not found in the networks (mistery) just return an empty
  # frame
  if(length(this_net_locations) == 0) {
    return(tibble::tibble(org_id = character(), 
                          loc_id = character(), 
                          suitability = double(), 
                          w = double()))
  }
  
  # Get climate of points where species has been seen in GBIF
  sp_locations_climate <- this_occurrences %>%
    get_climate_cells(filled_climate_occ) 
  
  # Get climate of points where suitability will be calculated
  net_locations_climate <- grid_networks %>%
    dplyr::filter(loc_id %in% this_net_locations) %>%
    get_climate_cells(filled_climate_net)
  
  if(is.null(niche_space)){
    niche_space <- calc_niche_space(sp_locations_climate)
  }
  
  # Get niche and smooth it
  sp_niche <- project_locations_climate_to_space(sp_locations_climate, 
                                                 niche_space) %>%
    smooth_realised_niche(R)
  
  # Get network climate in niche space
  net_niche_space <- project_locations_climate_to_space(net_locations_climate, 
                                                        niche_space)$lisup
  
  # Calculate the suitability
  suitabiliy <- raster::cellFromXY(sp_niche$z, net_niche_space) %>%
    raster::extract(sp_niche$z, .) 
  w <- raster::cellFromXY(sp_niche$w, net_niche_space) %>%
    raster::extract(sp_niche$w, .) 
  
  # Put it all in a nice data frame format
  tibble::tibble(org_id = this_sp, 
                 suitability = suitabiliy, 
                 w = w)
}

# Gicen some locations (coded as a grid in the climate thingy) find their projection in niche space 
project_locations_climate_to_space <- function(sp_locations_climate, niche_space){
  sp_locations_climate %>%
    dplyr::filter_all(function(x) !is.na(x)) %>%
    dplyr::select(-wc_grid) %>%
    ade4::suprow(niche_space, .) 
}

smooth_realised_niche <- function(projection, R){
  projection %$%
    ecospat::ecospat.grid.clim.dyn(lisup, lisup, lisup, R) %$%
    list(z = z.uncor, w = w)
}

calc_niche_space <- function(background_climate){
  background_climate %>%
    dplyr::filter_all(function(x) !is.na(x)) %>%
    dplyr::select(-wc_grid) %>%
    ade4::dudi.pca(center = T, scale = T, scannf = F, nf = 2)
}

