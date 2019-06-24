clean_occurrences <- function(dirty_occurrence_df){
  dirty_occurrence_df <- readr::read_csv("data/downloads/spp_ocurrences/Adia cinerella.csv")
  occurrence_df <- dirty_occurrence_df %>%
    dplyr::mutate(sp_id = "a") %>% # only for testing
    dplyr::mutate(decimallongitude = as.numeric(as.character(decimalLongitude)), 
                  decimallatitude = as.numeric(as.character(decimalLatitude)), 
                  coordinateUncertaintyInMeters  = as.numeric(as.character(coordinateUncertaintyInMeters)), 
                  year  = as.numeric(as.character(year)), 
                  country_code_iso3c = countrycode::countrycode(countryCode, origin =  'iso2c', destination = 'iso3c')) %>%
    dplyr::filter(!is.na(decimalLongitude),
                  !is.na(decimalLatitude),
                  !is.na(countryCode))# %>% 
    # dplyr::mutate(valid_coordinate = 
                    # CoordinateCleaner::cc_val(x = ., 
                                              # lon = "decimalLongitude",
                                              # lat = "decimalLatitude",
                                              # value = "flagged"))
  
  cleaned_occurrences <- CoordinateCleaner::clean_coordinates(occurrence_df, 
                                       species = "sp_id", 
                                       countries = "country_code_iso3c",
                                       seas_ref = land_data, 
                                       seas_scale = 10,
                                       verbose = FALSE) 
  
  library(ggplot2)
  cleaned_occurrences %>%
    ggplot(aes(x = decimallongitude, y = decimallatitude)) +
    geom_map(map = map_data('world'),
             data = map_data('world'),
             aes(map_id = region, x = long, y = lat, group = group),
             fill = "white") +
    geom_point(aes(colour = .sea, shape = .otl))
}
