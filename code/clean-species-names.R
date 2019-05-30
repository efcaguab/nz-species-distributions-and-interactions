# Get status of species and genus names, wether they are accepted or not if not wether they have a synonym
get_synonyms <- function(db_zip, spp){
  require(DBI)
  require(dplyr)

  # db_zip <- "data/downloads/itis_sqlite.zip"
  
  # Establish connection to the ITIS datavase
  dir <- tempdir()
  unzip_files <- unzip(db_zip, exdir = dir, overwrite = TRUE)
  db_path <- unzip_files[stringr::str_detect(unzip_files, "ITIS")]
  db_con <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Needed tables from the database
  dbListTables(db_con)
  taxonomic_units <- tbl(db_con, "taxonomic_units")
  synonym_links <- tbl(db_con, "synonym_links")
  
  # Frame to be used to merge when having accepted names
  taxonomic_unit_names <- taxonomic_units %>%
    select(tsn, complete_name) %>%
    rename(tsn_accepted = tsn, complete_name_accepted = complete_name)
  
  # Species level
  spp_names <- spp %>%
    filter(!sp_unidentified) %>%
    distinct(sp_name) %$% sp_name
  by_species <- taxonomic_units %>%
    filter(complete_name %in% spp_names) %>%
    select(tsn, complete_name, n_usage, rank_id, unaccept_reason) %>%
    left_join(synonym_links, by = "tsn") %>%
    left_join(taxonomic_unit_names, by = "tsn_accepted") %>%
    collect()
  
  # By genus
  genus_names <- spp %>%
    filter(!gen_unidentified, 
           sp_unidentified) %>% 
    distinct(genus) %$% genus
  by_genus <- taxonomic_units %>% 
    filter(complete_name %in% genus_names) %>%
    select(tsn, complete_name, n_usage, rank_id, unaccept_reason) %>%
    left_join(synonym_links, by = "tsn") %>%
    left_join(taxonomic_unit_names, by = "tsn_accepted") %>%
    collect() 
  
  dbDisconnect(db_con)
  list(by_species, by_genus)
}
