# Get status of species and genus names, wether they are accepted or not if not wether they have a synonym
get_synonyms_db <- function(db_zip){
  
  suppressPackageStartupMessages({
    require(DBI)
    require(dplyr)
    })

  # db_zip <- "data/downloads/itis_sqlite.zip"
  
  # Establish connection to the ITIS datavase
  dir <- tempdir()
  unzip_files <- unzip(db_zip, exdir = dir, overwrite = TRUE)
  db_path <- unzip_files[stringr::str_detect(unzip_files, "ITIS")]
  db_con <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Needed tables from the database
  dbListTables(db_con)
  taxonomic_units <- tbl(db_con, "taxonomic_units") %>%
    filter(kingdom_id %in% c(3,5)) %>% # only keep plants and animals
    select(tsn, complete_name, n_usage, rank_id, unaccept_reason) # only keep useful columns
  synonym_links <- tbl(db_con, "synonym_links")
  
  tables <- list(taxonomic_units = collect(taxonomic_units), 
                 synonym_links = collect(synonym_links))
  
  dbDisconnect(db_con)
  tables
}

# Assess a given species name. This function takes as argument the name of a
# species and checks in the synonym database and the GNR one to see if its a
# valid one
assess_sp_name <- function(this_sp_name, synonyms_db, verbose = TRUE){
  suppressPackageStartupMessages({
    require(dplyr)
  })
  if(verbose){
    cat("Assessing species name:", this_sp_name, "\n")
  }
  
  # look for synonyms in the database
  itis_results <- check_itis(this_sp_name, synonyms_db)
  # confirm names in GNR
  gnr_results <- itis_results %>%
    split(.$sp_name) %>%
    purrr::map_df(~check_gnr(.$sp_name))
  # look again for synonyms of stuff that was missspelled
  itis_round_two <- gnr_results %>%
    filter(!sp_name %in% itis_results$sp_name) %>%
    split(.$sp_name) %>%
    purrr::map_df(~check_itis(.$sp_name, synonyms_db))
  # integrate things together
  
  bind_rows(itis_results, itis_round_two) %>% 
      full_join(gnr_results, by = "sp_name")  %>% 
      # Check the kingdom of the GNR result should be plant or animal
      rowwise() %>%
      mutate(ncbi_kingdom = definitely(get_possible_kingdoms, 10, 1/10, sp_name))
  }

# Function to try a function multiple times until it succeeds, especially useful
# for http requests that might fail randomly
definitely <- function(func, n_tries = 10, sleep = 1/10, ...){
  
  possibly_func = purrr::possibly(func, otherwise = NULL)
  
  result = NULL
  try_number = 1
  
  while(is.null(result) && try_number <= n_tries){
    # message("Try number: ", try_number)
    try_number = try_number + 1
    result = possibly_func(...)
    Sys.sleep(sleep)
  }
  
  return(result)
}

# Get the kindoms a species name (or genus might belong to)
get_possible_kingdoms <- function(this_sp_name){
  
  uids <- this_sp_name %>%
    taxize::get_uid_(db = "ncbi", messages = F) %>%
    purrr::map_df(function(x){x}) %>%
    # because it could match random shit eg. Anthrax could max Bacilus anthracis anthrax
    filter(scientificname == this_sp_name)
  
  # It it was not found in NCIB return NA
  if(nrow(uids) == 0){
    return(NA_character_)
  } else {
    uids %>%
      split(.$uid) %>%
      purrr::map_chr(~get_kingdom_of_uid(.$uid)) %>%
      paste(collapse = "-")
  }
}

# Get the kingdom of a NCBI UID taxon
get_kingdom_of_uid <- function(uid){
  kingdom <- taxize::classification(uid, db = "ncbi")[[1]] %>%
    filter(rank == "kingdom") %$%
    name
  
  if(length(kingdom) == 0){
    return("NoKingdomAvailable")
  } else {
    return(kingdom)
  }
}

# Check wether a tentative species name (string is in the itis database) if it 
# is check for synonyms, returns a data frame
check_itis <- function(sp_name, synonyms_db){
  
  # find species in itis database
  itis_matches <- synonyms_db$taxonomic_units %>%
    # filter(stringr::str_detect(complete_name, sp_name)) %>%
    filter(complete_name == sp_name) %>%
    left_join(synonyms_db$synonym_links, by = "tsn") 
  
  if(nrow(itis_matches) > 0) {
    # reformat to desired data frame structure
    itis_df <- rename_itis_df(itis_matches)
    
    # if there are accepted synonyms append them
    if(any(!is.na(itis_matches$tsn_accepted))) {
      accepted_synonyms <- synonyms_db$taxonomic_units %>%
        filter(tsn %in% itis_matches$tsn_accepted) %>%
        rename_itis_df()
      
      itis_df <- bind_rows(itis_df, accepted_synonyms)
    } 
  } else {
    # if its not found in itis return a df with desired structure anyways
    itis_df <- tibble(sp_name = sp_name, 
                      itis = NA_character_, 
                      itis_reason = NA_character_, 
                      itis_tsn = NA_integer_)
  }
  itis_df
}

# rename the structure of itis df columns with the desired onw
rename_itis_df <- function(df){
  df %>%
    select(sp_name = complete_name, 
           itis = n_usage, 
           itis_reason = unaccept_reason, 
           itis_tsn = tsn) 
}

# Check wether a species name is available in the global names resolver system
check_gnr <- function(sp_name){
  
  empty_gnr <- function(sp_name){
    tibble(sp_name = sp_name, 
           gnr_score = NA, 
           gnr_source = NA)
  }
  
  gnr_match <- taxize::gnr_resolve(sp_name, 
                                   best_match_only = "TRUE", 
                                   canonical = TRUE) 
  
  if(nrow(gnr_match) > 0){
    gnr_df <- tibble(sp_name = gnr_match$matched_name2, 
                     gnr_score = gnr_match$score, 
                     gnr_source = gnr_match$data_source_title)
    
    if(gnr_match$user_supplied_name != gnr_match$matched_name2) {
      gnr_df <- empty_gnr(sp_name) %>%
        bind_rows(gnr_df)
    }
  } else {
    gnr_df <- empty_gnr(sp_name)
  } 
  gnr_df
}

# With the list of species names assess them all. Uses a "cache" of names that
# have been previoulsy assessed
check_spp_names <- function(spp, synonyms_db, prev_sp_name_assessments_path){
  suppressPackageStartupMessages({
    require(dplyr)
  })
  
  # load previous assessments to filter species before 
  prev_sp_name_assessments <- readr::read_csv(prev_sp_name_assessments_path, 
                                              col_types = "ccccidcc")
  
  # species_level
  spp %>%
    filter(!sp_unidentified, 
           !sp_name %in% prev_sp_name_assessments$queried_sp_name) %>%
    distinct(sp_name) %$% 
    sp_name %>%
    # extract(1:15) %>%
    purrr::map_df(assess_sp_name_memoised, prev_sp_name_assessments_path, synonyms_db)
  
  # Once is done just read the assessment file from disk and calculate helpful stats
  readr::read_csv(prev_sp_name_assessments_path, col_types = "ccccidcc") %>%
    filter(queried_sp_name %in% unique(spp$sp_name)) %>%
    # remove variant and subspecies abbreviations from the retrieved species
    # names, it is clear that they are one when there is 3 words or more in there
    mutate(sp_name = stringr::str_replace_all(sp_name,"var. ", ""),
           sp_name = stringr::str_replace_all(sp_name,"ssp. ", ""),
           sp_name_in_gnr = !is.na(gnr_score), 
           sp_name_itis_invalid = itis %in% c("invalid", "not accepted"), 
           # good species name has been found and its not invalid
           good_sp_name = sp_name_in_gnr & ! sp_name_itis_invalid, 
           good_queried_sp_name = good_sp_name & sp_name == queried_sp_name, 
           sp_name_rank = get_name_rank(sp_name), 
           queried_sp_name_rank = get_name_rank(queried_sp_name)) %>%
    group_by(queried_sp_name) %>%
    mutate(corrected_typo = (any(gnr_score == 0.750) & 
                               all(sp_name_rank == queried_sp_name_rank)) | 
             (any(gnr_score >= 0.988) & 
                queried_sp_name != sp_name & all(is.na(itis_reason))),
           corrected_typo = tidyr::replace_na(corrected_typo, FALSE),
           corrected_typo = any(corrected_typo), 
           good_queried_sp_name = any(good_queried_sp_name), 
           n_spellings = n_distinct(sp_name), 
           any_sp_itis_invalid = any(sp_name_itis_invalid), 
           alt_sp_name_found = n_spellings > 1 & any_sp_itis_invalid) %>%
    ungroup()
  
}

# Memoised version of assess_sp_name that checks a data frame with species names
# before checking with online APIS (as it's much faster)
assess_sp_name_memoised <- function(this_sp_name, 
                                    prev_sp_name_assessments_path, 
                                    synonyms_db){
  
  prev_sp_name_assessments <- readr::read_csv(prev_sp_name_assessments_path, 
                                              col_types = "ccccidcc")
  
  previous_info <- prev_sp_name_assessments %>% 
    filter(queried_sp_name == this_sp_name)
  
  # if there is no previous info
  if(nrow(previous_info) == 0){
    new_info <- assess_sp_name(this_sp_name, synonyms_db) %>%
      tibble::add_column(queried_sp_name = this_sp_name, .before = 1)
    readr::write_csv(new_info, 
                     path = prev_sp_name_assessments_path, 
                     append = TRUE)
    return(new_info)
  } else {
    return(previous_info)
  }
  
}
# Phacelia secunda
# Phacelia secunda

# assess_sp_name_memoised <- memoise::memoise(assess_sp_name, )



get_name_rank <- function(x){
  sp_name_n_spaces <- stringr::str_count(x, " ")
  case_when(
    sp_name_n_spaces == 0 ~ "genus", 
    sp_name_n_spaces == 1 ~ "species", 
    TRUE ~ "subspecies"
  )
}
