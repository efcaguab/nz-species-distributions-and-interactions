clean_occurrences <- function(dirty_occurrence_df){
  dirty_occurrence_df <- readr::read_csv("data/downloads/spp_ocurrences/Adia cinerella.csv")
  dirty_occurrence_df %>%
    dplyr::mutate(decimallongitude = as.numeric(as.character(decimalLongitude)), 
                  decimallatitude = as.numeric(as.character(decimalLatitude)), 
                  coordinateUncertaintyInMeters  = as.numeric(as.character(coordinateUncertaintyInMeters)), 
                  year  = as.numeric(as.character(year)), 
                  country_code_iso3c = countrycode::countrycode(countryCode, origin =  'iso2c', destination = 'iso3c')) %>%
    dplyr::filter(!is.na(decimalLongitude),
                  !is.na(decimalLatitude),
                  !is.na(countryCode)) %>% 
    dplyr::mutate(valid_coordinate = 
                    CoordinateCleaner::cc_val(x = ., 
                                              # lon = "decimalLongitude",
                                              # lat = "decimalLatitude",
                                              value = "flagged"))
}
