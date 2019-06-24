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
                                        prev_sp_ocurrences_path, 
                                        ocurrences_dir){
  
  # load previous assessments to filter species before 
  prev_sp_ocurrences <- readr::read_csv(prev_sp_ocurrences_path, 
                                        col_types = "ccc")
  
  # species_level
  spp_to_download %>%
    dplyr::filter(!is.na(sp_name), 
                  !sp_name %in% unique(prev_sp_ocurrences$sp_name)) %>%
    dplyr::distinct(sp_name) %$% 
    sp_name %>%
    # extract(15:20) %>%
    furrr::future_map_dfr(download_sp_ocurrences_memoised, 
                          data_fields,
                          prev_sp_ocurrences_path, 
                          ocurrences_dir, 
                          .progress = TRUE)
}


download_sp_ocurrences_memoised <- function(this_sp_names, 
                                            data_fields,
                                            prev_sp_ocurrences_path, 
                                            ocurrences_dir){
  
  require(magrittr)
  
  prev_sp_ocurrences <- readr::read_csv(prev_sp_ocurrences_path, 
                                        col_types = "ccc")
  
  sp_to_query_index <- !this_sp_names %in% unique(prev_sp_ocurrences$sp_name)
  sp_to_query <- this_sp_names[sp_to_query_index]
  
  if (length(sp_to_query) == 0) {
    previous_info <- prev_sp_ocurrences %>%
      filter(sp_name %in% this_sp_names)
    return(previous_info)
  } else {
    new_ocurrences <- sp_to_query %>%
      purrr::map_dfr(download_sp_ocurrences, data_fields)
    readr::write_csv(new_ocurrences, 
                     path = file.path(ocurrences_dir, 
                                      paste0(this_sp_names, ".csv")))
    
    n_ocurrences <- dplyr::filter(new_ocurrences, !is.na(key)) %>% nrow()
    new_info <- tibble::tibble(sp_name = this_sp_names, 
                               n_ocurrences = n_ocurrences, 
                               date_time = Sys.time())
    lck <- filelock::lock(paste0(prev_sp_ocurrences_path, ".lck"))
    readr::write_csv(new_info, 
                     path = prev_sp_ocurrences_path, 
                     append = T)
    filelock::unlock(lck)
    return(new_info)
  }
}

download_sp_ocurrences <- function(this_sp_name, data_fields, verbose = TRUE){

  # An epmpty dataframe with the desired output structure
  empty_ocurrences_df <- tibble::tibble(
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
    sp_name = this_sp_name)
  
  format_successful_ocurrences <- function(x, empty_ocurrences_df){
    # Sometimes the data downloaded doesn't include all expected columns so need
    # to merge it with an empty one to make sure all the required ones are there
    dplyr::bind_rows(empty_ocurrences_df, x$data) %>%
      dplyr::select(!!data_fields) %>%
      dplyr::mutate(sp_name = attributes(x)$args$scientificName) %>%
      dplyr::slice(-1)
  }
  
  # Get number of ocurrences in advance
  n_ocurrences <- purrr::possibly(~rgbif::occ_search(scientificName = ., 
                                                    hasCoordinate = TRUE, 
                                                    # hasGeospatialIssue = FALSE, 
                                                    limit = 1, 
                                                    return = "meta")$count, 
                                  0) %>%
    purrr::invoke(this_sp_name)
  
  if (verbose) {
    cat("Downloading", 
        format(n_ocurrences, big.mark = ","),
        "ocurrences of", 
        glue::glue_collapse(this_sp_name, sep = ", ", last = " and "), "\n")
  }
  
  ocurrences_list <- definitely(rgbif::occ_data, n_tries = 10, sleep = 1/10, 
                                scientificName = this_sp_name, 
                                hasCoordinate = TRUE,
                                limit = 1000000)
  
  if (!is.null(ocurrences_list$data)) {
    ocurrences_df <- format_successful_ocurrences(ocurrences_list,
                                                  empty_ocurrences_df)
  } else {
    ocurrences_df <- empty_ocurrences_df
  }

    ocurrences_df
}

create_empty_csv_if_unexistent <- function(path, fields){
  if(!file.exists(path)){
    tibble::tibble %>%
      purrr::lift_dv() %>%
      purrr::invoke(c(fields)) %>%
      readr::write_csv(path = path, col_names = FALSE)
  }
}

get_gbif_keys <- function(spp_to_download, 
                          rgbif_key_fields, 
                          prev_gbif_keys_path){
  # load previous assessments to filter species before 
  prev_sp_keys <- readr::read_csv(prev_gbif_keys_path, 
                                  col_types = "cicc")
  
  # species_level
  spp_to_download %>%
    dplyr::filter(!is.na(sp_name), 
                  !sp_name %in% unique(prev_sp_keys$queried_sp_name)) %>%
    dplyr::distinct(sp_name) %$% 
    sp_name %>%
    purrr::map_dfr(get_gbif_key_memoised, 
                   prev_gbif_keys_path)
}

get_gbif_key_memoised <- function(this_sp_name,  
                                  prev_gbif_keys_path, 
                                  verbose = TRUE){
  suppressPackageStartupMessages(require(magrittr))
  
  prev_sp_keys <- readr::read_csv(prev_gbif_keys_path, 
                                        col_types = "cicc")
  
  previous_info <- prev_sp_keys %>% 
    dplyr::filter(queried_sp_name == this_sp_name)
  
  # if there is no previous info
  if(nrow(previous_info) == 0){
    new_info <- get_gbif_key(this_sp_name) %>%
      tibble::add_column(queried_sp_name = this_sp_name, .before = 1)
    readr::write_csv(new_info, 
                     path = prev_gbif_keys_path, 
                     append = TRUE)
    return(new_info)
  } else {
    return(previous_info)
  }
  
}

get_gbif_key <- function(this_sp_name, verbose = TRUE){
  
  if (verbose == TRUE){
    cat("Looking up GBIF keys for", this_sp_name, "\n")
  }
  
  keys <- rgbif::name_suggest(this_sp_name, limit = 1000)
  if(nrow(keys) == 0){
    tibble::tibble(key = NA_integer_, 
                   canonicalName = NA_character_, 
                   rank = NA_character_)
  } else {
    keys
  }
}


build_gbif_queries <- function(gbif_keys){
  keys <- gbif_keys %>%
    dplyr::filter(! rank %in% c("GENUS", "UNRANKED")) %>%
    dplyr::mutate(same_name = tolower(queried_sp_name) == tolower(canonicalName), 
                  rank_queried = stringr::str_count(queried_sp_name, " "), 
                  rank_canonical = stringr::str_count(canonicalName, " "), 
                  same_rank = rank_queried == rank_canonical) %>% 
    dplyr::group_by(queried_sp_name) %>%
    dplyr::mutate(queried_found = any(tolower(queried_sp_name) == tolower(canonicalName)), 
                  only_subspecies = all(rank_canonical > rank_queried)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(necessary_name = (queried_found & same_name) | (!queried_found), 
                  include = same_rank & necessary_name | only_subspecies) %>% View
    dplyr::filter(include) %$% 
    key
  
  initial_query <- construct_query(keys)
  initial_query_length <- check_query_length(initial_query)
  if(initial_query_length > 12000){
    n_chunks <- initial_query_length %>%
      divide_by(12000) %>%
      ceiling() %>%
      add(1)
    
    keys %>%
      split(cut(seq_along(.), n_chunks)) %>%
      purrr::map(construct_query) %>%
      magrittr::set_names(NULL)
  } else {
    list(initial_query)
  }
}

construct_query <- function(keys){
  taxon_query <- keys %>%
    paste0(collapse = ",") %>%
    paste0('taxonKey = ', .)
  
  rgbif::occ_download_prep(taxon_query, 
                    'hasCoordinate = true',
                    'hasGeospatialIssue = false', 
                    user = 'efcaguab', 
                    pwd = 'SkC*8Dmh', 
                    email = 'efcaguab@gmail.com')
}

check_query_length <- function(x){
  x %$%
    request %>%
    rgbif:::check_inputs() %>%
    as.character() %>%
    nchar()
}
