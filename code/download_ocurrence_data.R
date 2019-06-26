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

prepare_gbif_downloads <- function(gbif_queries, prev_occ_download_keys){
  if (is.null(prev_occ_download_keys)) {
    message("No download keys found in config.yaml file. *New* datasets will be
            requested from GBIF")
    # If things fail cancel downloads so another one can be started without waiting
    on.exit(rgbif::occ_download_cancel_staged())
    download_keys <- rgbif::occ_download_queue(.list = gbif_queries)
    config <- yaml::read_yaml("config.yaml")
    config$gbif_download_key <- as.character(download_keys)
    yaml::write_yaml(config, "config.yaml")
    return(config$gbif_download_key)
  } else {
    message("Download keys found in config.yaml file. *No* datasets will be
            requested from GBIF")
    return(prev_occ_download_keys)
  }
}

get_gbif_download_info <- function(gbif_download_keys){
  
  download_details <- rgbif::occ_download_list()
  download_details$results %>%
    dplyr::filter(key %in% gbif_download_keys) %>%
    dplyr::mutate(filename = basename(downloadLink))
}

download_gbif_ocurrences <- function(gbif_download_info, download_path, success_file){
  
  downloaded_files <- list.files(download_path, pattern = "zip")
  
  if (all(gbif_download_info$filename %in% downloaded_files)) {
    message("All files have been already downloaded, no extra download necessary")
    write(Sys.time(), success_file)
  } else {
    if (length(downloaded_files) == 0) {
      message("Downloading all files")
      keys_to_download <- gbif_download_info
    } else {
      message("Downloading only some files")
      keys_to_download <- gbif_download_info %>%
        dplyr::filter(!filename %in% downloaded_files) 
    }
    keys_to_download %>%
      split(.$key) %>%
      purrr::map(~ definitely(func = download.file, n_tries = 10, sleep = 1/10, 
                              url = .$downloadLink,
                              destfile = file.path(download_path, .$filename)))
    write(Sys.time(), success_file)
  }
  # these_downloads
}
