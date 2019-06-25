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
                  include = same_rank & necessary_name | only_subspecies) %>% 
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
                    'hasGeospatialIssue = false')
}

check_query_length <- function(x){
  x %$%
    request %>%
    rgbif:::check_inputs() %>%
    as.character() %>%
    nchar()
}

download_gbif_ocurrences <- function(gbif_queries, download_path, verbose = TRUE){
  # If things fail cancel downloads so another one can be started without waiting
  
  downloaded_files <- list.files(download_path, pattern = "zip")
  n_downloaded_files <- downloaded_files %>% length()
  
  if(n_downloaded_files > 0){
    if (n_downloaded_files != length(gbif_queries)) {
      warning("Found zip files in the spp_occurrences folder but its number doesn't match the number of requests")
    }
    message("New occurrences won't be downloaded from GBIF. To get new ocurrences delete these zip files. ")
    download_details <- rgbif::occ_download_list()
  } else {
    on.exit(rgbif::occ_download_cancel_staged())
    
    if (verbose){
      cat("Requesting datasets to GBIF\n")
    }
    downloads <- rgbif::occ_download_queue(.list = gbif_queries)
    
    if (verbose){
      cat("Downloading datasets from GBIF\n")
    }
    
    download_details <- rgbif::occ_download_list()
    
    these_downloads <- download_details$results %>%
      dplyr::filter(key %in% as.character(downloads)) %>%
      dplyr::mutate(filename = basename(downloadLink))
    
    these_downloads %>%
      split(.$key) %>%
      purrr::map(~ definitely(func = download.file, n_tries = 10, sleep = 1/10, 
                              url = .$downloadLink,
                              destfile = file.path(download_path, .$filename)))
    
    downloaded_files <- list.files(download_path, pattern = "zip")
  }
  
  download_details$results %>%
    dplyr::mutate(file_name = basename(downloadLink)) %>%
    dplyr::filter(file_name %in% downloaded_files)
}
