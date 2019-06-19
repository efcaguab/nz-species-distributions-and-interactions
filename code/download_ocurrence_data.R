# get list of species to which download data from rgbif
select_species_to_download <- function(spp, minimum_spp_locations){
  spp %>% 
    dplyr::count(sp_name, sort = T) %>%  
    dplyr::filter(n >= minimum_spp_locations)
}

download_sp_ocurrences_memoised <- function(this_sp_names, 
                                             prev_sp_ocurrences_path){
  
  prev_sp_ocurrences <- readr::read_csv(prev_sp_ocurrences_path)
  
}

download_sp_ocurrences <- function(this_sp_names){
  this_sp_names <- c("Stachys albi", "Stachys alba")
  rgbif::name_lookup(this_sp_names[2], rank = "species")$data
  rgbif::name_lookup('Phacelia secunda', rank = "species")$data %>% View
  
  rgbif::name_suggest(q=this_sp_names[2], rank='species')
  rgbif::name_suggest(q='Phacelia secunda', rank='species')
  data_fields <- c('key', 'scientificName', 'decimalLatitude', 'decimalLongitude', 'geodeticDatum', 'countryCode', 'country', 'individualCount', 'coordinateUncertaintyInMeters', 'year', 'basisOfRecord')
  
  system.time({
    c <- rgbif::occ_data(scientificName = c('Phacelia secunda'), 
                           hasCoordinate = TRUE,
                           limit = 1000000) 
  })
  
  system.time({
    c('Phacelia secunda', 'Phacelia secunda') %>%
      purrr::map(~rgbif::occ_search(scientificName = ., 
                                    hasCoordinate = TRUE,
                                    limit = 1000000) )
  })
 
  
  system.time({
    rgbif::occ_search(scientificName = 'Stachys albicaulis', 
                                    hasCoordinate = TRUE,
                                    limit = 1000000) 
  })
  
  a <- rgbif::occ_download('taxonKey=7316195', 'hasCoordinate = true') 
  rgbif::occ_download_meta(a)
  f <- rgbif::occ_download_get(a, overwrite = T)
  f %>% rgbif::occ_download_import()
  f %>% rgbif::gbif_citation()
}


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
