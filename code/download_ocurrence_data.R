# get list of species to which download data from rgbif
select_species_to_download <- function(species_ids, clean_interactions, minimum_spp_locations){
  sp_id_to_download <- clean_interactions %>%
    tidyr::gather("guild", "sp_id", pla_id, ani_id) %>%
    dplyr::group_by(sp_id) %>%
    dplyr::summarise(n_loc = dplyr::n_distinct(loc_id)) %>%
    dplyr::filter(n_loc >= minimum_spp_locations) %$%
    sp_id
  
  species_ids %>%
    dplyr::filter(sp_id %in% sp_id_to_download)
}

download_species_ocurrences <- function(spp_to_download, 
                                        data_fields, 
                                        prev_sp_ocurrences_path){
  
  # load previous assessments to filter species before 
  prev_sp_ocurrences <- readr::read_csv(prev_sp_ocurrences_path, 
                                        col_types = "dcddccidiccccc")
  
  # species_level
  spp_to_download %>%
    dplyr::filter(!is.na(sp_name), 
                  !sp_name %in% unique(prev_sp_ocurrences$sp_name)) %>%
    dplyr::distinct(sp_name) %$% 
    sp_name %>%
    extract(1:15) %>%
    purrr::map_df(download_sp_ocurrences_memoised, data_fields, prev_sp_ocurrences_path)
  
}


download_sp_ocurrences_memoised <- function(this_sp_names, 
                                            data_fields,
                                            prev_sp_ocurrences_path){
  
  prev_sp_ocurrences <- readr::read_csv(prev_sp_ocurrences_path, 
                                        col_types = "dcddccidiccccc")
  
  sp_to_query_index <- !this_sp_names %in% unique(prev_sp_ocurrences$sp_name)
  sp_to_query <- this_sp_names[sp_to_query_index]
  
  if (length(sp_to_query) == 0){
    previous_info <- prev_sp_ocurrences %>% 
      filter(sp_name %in% this_sp_names)
    return(previous_info)
  } else {
    new_info <- sp_to_query %>%
      purrr::map_dfr(download_sp_ocurrences, data_fields)
    readr::write_csv(new_info, 
                     path = prev_sp_ocurrences_path, 
                     append = TRUE)
    return(new_info)
  }
}

download_sp_ocurrences <- function(this_sp_name, data_fields, verbose = TRUE){

  format_successful_ocurrences <- function(x){
    dplyr::select(x$data, !!data_fields) %>%
      dplyr::mutate(sp_name = attributes(x)$args$scientificName)
  }
  
  if(verbose){
    cat("Downloading ocurrences for:", glue::glue_collapse(this_sp_name, sep = ", ", last = " and "), "\n")
  }
  
  ocurrences_list <- rgbif::occ_data(scientificName = this_sp_name, 
                                     hasCoordinate = TRUE,
                                     limit = 1000000)
  
  if(!is.null(ocurrences_list$data)){
    ocurrences_df <- format_successful_ocurrences(ocurrences_list)
  } else {
    ocurrences_df <- tibble::tibble(
      key = NA_real_, 
      scientificName = NA_character_, 
      decimalLatitude = NA_real_, 
      decimalLongitude = NA_real_,
      geodeticDatum = NA_character_, 
      countryCode = NA_character_, 
      individualCount = NA_integer_, 
      coordinateUncertaintyInMeters = NA_real_, 
      year = NA_integer_, 
      basisOfRecord = NA_character_, 
      issues = NA_character_, 
      datasetKey = NA_character_, 
      taxonRank = NA_character_,
      sp_name = attributes(ocurrences_list)$args$scientificName)
  }
  "dcddccidiccccc"
  ocurrences_df
  
}

