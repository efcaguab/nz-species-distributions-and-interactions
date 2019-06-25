clean_occurrences <- function(dirty_occurrence_df, land_data, country_data_sf){
  # dirty_occurrence_df <- readr::read_csv("data/downloads/spp_ocurrences/Adia cinerella.csv")
  occurrence_df <- dirty_occurrence_df %>%
    # dplyr::mutate(sp_id = "a") %>% # only for testing
    dplyr::mutate(decimallongitude = as.numeric(as.character(decimalLongitude)), 
                  decimallatitude = as.numeric(as.character(decimalLatitude)), 
                  coordinateUncertaintyInMeters  = 
                    as.numeric(as.character(coordinateUncertaintyInMeters)), 
                  year  = as.numeric(as.character(year)), 
                  country_code_iso3c = 
                    countrycode::countrycode(countryCode, 
                                             origin =  'iso2c', 
                                             destination = 'iso3c')) %>%
    dplyr::filter(!is.na(decimalLongitude),
                  !is.na(decimalLatitude),
                  !is.na(countryCode),
                  coordinateUncertaintyInMeters/1000 <= 100 |
                    is.na(coordinateUncertaintyInMeters),
                  basisOfRecord == "HUMAN_OBSERVATION" |
                    basisOfRecord == "OBSERVATION" |
                    basisOfRecord == "PRESERVED_SPECIMEN",
                  individualCount > 0 | is.na(individualCount),
                  individualCount < 99 | is.na(individualCount),
                  year > 1945)
    
  cleaned_occurrences <- occurrence_df %>%
    dplyr::mutate(.sea_manual = CoordinateCleaner::cc_sea(., ref = land_data,
                                                   scale = 10, 
                                                   value = "flagged", 
                                                   verbose = FALSE)) %>%
    
    CoordinateCleaner::clean_coordinates(tests = c("capitals",
                                                   "centroids", 
                                                   "equal", 
                                                   "gbif", 
                                                   "institutions", 
                                                   "zeros"),
                                         species = "sp_name", 
                                         countries = "country_code_iso3c",
                                         verbose = FALSE)
  # library(ggplot2)
  # cleaned_occurrences %>%
  #   ggplot(aes(x = decimallongitude, y = decimallatitude)) +
  #   geom_map(map = map_data('world'),
  #            data = map_data('world'),
  #            aes(map_id = region, x = long, y = lat, group = group),
  #            fill = "white") +
  #   geom_point(aes(colour = .sea, shape = .otl))
}

extract_occurrence_files <- function(path){
  files <- list.files(path, pattern = "zip", full.names = TRUE, recursive = FALSE)
  keys <- basename(files) %>% tools::file_path_sans_ext()
  
  purrr::map2(files, keys, 
              ~unzip(.x, files = "occurrence.txt", exdir = file.path(path, .y), 
                     unzip = "unzip"))
  
  
}

x <- function(){
  drake::loadd(occ_downloads_info)
  system.time({
    small <- rgbif::occ_download_import(key = occ_downloads_info$key[2], 
                                        path =  "data/downloads/spp_occurrences")
  })
  
}
