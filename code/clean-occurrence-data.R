clean_occurrences_chunked <- function(dirty_occurrences, land_data, country_data_sf){
  suppressPackageStartupMessages({
    require(CoordinateCleaner)
    require(data.table)
  })
  
  n_occurrences <- nrow(dirty_occurrences)
  dirty_occurrences <- dirty_occurrences[, cuts := cut(1:n_occurrences, future::availableCores())]
  split(dirty_occurrences, by = "cuts") %>%
    purrr::map(clean_occurrences, land_data, country_data_sf
    ) %>%
    rbindlist()
}

clean_occurrences <- function(dirty_occurrences, land_data, country_data_sf, verbose = TRUE){
  # dirty_occurrence_df <- readr::read_csv("data/downloads/spp_ocurrences/Adia cinerella.csv")
  suppressPackageStartupMessages({
    require(CoordinateCleaner)
    require(data.table)
  })
  
  if (verbose) {
     cat("cleaning occurrence chunk\n")
  }
   
  dirty_ocurrences <- na.omit(dirty_occurrences, 
                              cols = c("decimalLongitude", 
                                       "decimalLatitude", 
                                       "countryCode")) 
  
  dirty_occurrences <- dirty_occurrences[
    (coordinateUncertaintyInMeters/1000 <= 100 |
       is.na(coordinateUncertaintyInMeters)) &
      (basisOfRecord == "HUMAN_OBSERVATION" |
         basisOfRecord == "OBSERVATION" |
         basisOfRecord == "PRESERVED_SPECIMEN") &
      (individualCount > 0 | individualCount < 99 | is.na(individualCount)) &
      (year > 1945)]
  
  cleaned_occurrences <- dirty_occurrences %>%
    dplyr::mutate(.sea_manual = cc_sea(., ref = land_data,
                                       lon = "decimalLongitude", 
                                       lat = "decimalLatitude",
                                       scale = 10, 
                                       value = "flagged", 
                                       verbose = FALSE)) %>%
    clean_coordinates(tests = c("capitals",
                                "centroids", 
                                "equal", 
                                "gbif", 
                                "institutions", 
                                "zeros"),
                      lon = "decimalLongitude", 
                      lat = "decimalLatitude",
                      species = "taxonKey",
                      countries = "countryCode",
                      verbose = FALSE)
  cleaned_occurrences <- as.data.table(cleaned_occurrences)
  cleaned_occurrences[, !c(".val", 
                          ".equ", 
                           ".zer", 
                           ".cap", 
                           ".cen", 
                           ".gbf", 
                           ".inst", 
                           "individualCount", 
                           "coordinateUncertaintyInMeters", 
                           "year", 
                           "basisOfRecord", 
                           "datasetKey")]
  # library(ggplot2)
  # cleaned_occurrences %>%
  #   ggplot(aes(x = decimallongitude, y = decimallatitude)) +
  #   geom_map(map = map_data('world'),
  #            data = map_data('world'),
  #            aes(map_id = region, x = long, y = lat, group = group),
  #            fill = "white") +
  #   geom_point(aes(colour = .sea, shape = .otl))
}

extract_occurrence_files <- function(path, success_file, file_trigger){
  files <- list.files(path, 
                      pattern = "zip",
                      full.names = TRUE,
                      recursive = FALSE)
  
  keys <- basename(files) %>% 
    tools::file_path_sans_ext()
  
  purrr::map2(files, keys, 
              ~unzip(.x, files = "occurrence.txt", exdir = file.path(path, .y), 
                     unzip = "unzip"))
  
  write(as.character(Sys.time()), success_file)
}

read_occurrences <- function(path, occ_data_fields, file_trigger, verbose = TRUE){
  
  suppressPackageStartupMessages({
    require(data.table)
    require(countrycode)
  })
  
  files <- list.files(path, full.names = T, recursive = T, 
                      pattern = "occurrence.txt")
  
  if (verbose) cat("Reading files\n")
  occurrences <- files %>%
    purrr::map(data.table::fread, 
               sep = "\t", 
               select  = occ_data_fields, 
               quote = "") %>%
    data.table::rbindlist()
  
  
  to_num <- function(x){
    x %>%
      as.character() %>%
      as.numeric()
  }
  
  if (verbose) cat("pre-processing occurrences\n")
  occurrences[, ':='(decimalLatitude = to_num(decimalLatitude),
                     decimalLongitude = to_num(decimalLongitude), 
                     coordinateUncertaintyInMeters = to_num(coordinateUncertaintyInMeters), 
                     year = to_num(year), 
                     countryCode = countrycode(countryCode, 
                                               origin = "iso2c", 
                                               destination = "iso3c"))]
  
}

count_occurrences_per_taxon <- function(occurrences){
  occurrences[, .N, by = taxonKey]
}

get_occurrences_datasets <- function(occurrences){
  unique(occurrences, by = "datasetKey")[, datasetKey]
}

