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
  
  # # Frame to be used to merge when having accepted names
  # taxonomic_unit_names <- taxonomic_units %>%
  #   select(tsn, complete_name) %>%
  #   rename(tsn_accepted = tsn, complete_name_accepted = complete_name)
  # 
  # # Species level
  # spp_names <- spp %>%
  #   filter(!sp_unidentified) %>%
  #   distinct(sp_name) %$% sp_name
  # by_species <- taxonomic_units %>%
  #   filter(complete_name %in% spp_names) %>%
  #   select(tsn, complete_name, n_usage, rank_id, unaccept_reason) %>%
  #   left_join(synonym_links, by = "tsn") %>%
  #   left_join(taxonomic_unit_names, by = "tsn_accepted") %>%
  #   collect()
  # 
  # # By genus
  # genus_names <- spp %>%
  #   filter(!gen_unidentified, 
  #          sp_unidentified) %>% 
  #   distinct(genus) %$% genus
  # by_genus <- taxonomic_units %>% 
  #   filter(complete_name %in% genus_names) %>%
  #   select(tsn, complete_name, n_usage, rank_id, unaccept_reason) %>%
  #   left_join(synonym_links, by = "tsn") %>%
  #   left_join(taxonomic_unit_names, by = "tsn_accepted") %>%
  #   collect() 
  
  dbDisconnect(db_con)
  tables
}

assess_sp_name <- function(this_sp_name, synonyms_db){
  suppressPackageStartupMessages({
    require(dplyr)
  })
  message(this_sp_name)
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
    full_join(gnr_results, by = "sp_name") %>% print()
  
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
# Phacelia secunda
# Phacelia secunda

# assess_sp_name_m <- memoise::memoise(assess_sp_name)
