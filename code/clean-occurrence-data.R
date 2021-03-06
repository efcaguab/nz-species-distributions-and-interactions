# clean all occcurences downloaded. To do that chunk them in pieces and call clean_occurrences
clean_occurrences_chunked <- function(dirty_occurrences, land_data, country_data_sf, n_chunks){
  suppressPackageStartupMessages({
    require(CoordinateCleaner)
    require(data.table)
  })

  n_occurrences <- nrow(dirty_occurrences)
  dirty_occurrences <- dirty_occurrences[, cuts := cut(1:n_occurrences, n_chunks)]
  split(dirty_occurrences, by = "cuts") %>%
    purrr::map(clean_occurrences, land_data, country_data_sf
    ) %>%
    rbindlist()
}

# Remove occurrences with weird or incorrect coordinates
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
                                       "countryCode",
                                       "individualCount"))

  # There are lots of NA values that we're keeping because. Most records have no uncertainty or counts
  less_dirty_occurrences <- dirty_occurrences %>%
    .[coordinateUncertaintyInMeters/1000 <= 100 |
      is.na(coordinateUncertaintyInMeters)] %>%
    .[basisOfRecord == "HUMAN_OBSERVATION" |
      basisOfRecord == "OBSERVATION" |
      basisOfRecord == "PRESERVED_SPECIMEN"] %>%
    .[individualCount > 0 | is.na(individualCount)] %>%
    .[year > 1945]

  cleaned_occurrences <- less_dirty_occurrences %>%
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

# Extract occurrence files that were just downloaded. Path is where zip files
# are success file is an output to exit when successful and file trigger is just
# a dummy input to maintain the dependcy from the download
extract_occurrence_files <- function(path, success_file, file_trigger){
  files <- list.files(path,
                      pattern = "zip",
                      full.names = TRUE,
                      recursive = FALSE)

  if(length(files) > 0){
    keys <- basename(files) %>%
      tools::file_path_sans_ext()

    purrr::map2(files, keys,
                ~unzip(.x, files = "occurrence.txt", exdir = file.path(path, .y),
                       unzip = "unzip"))

    write(as.character(Sys.time()), success_file)
  }
}

# Read the occurrences from the extracted files
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

# Group species by species key. This is needed for putting occurrences together
get_gbif_key_groups <- function(occurrences){
  suppressPackageStartupMessages({
    library(data.table)
  })

  occurrences[, .(taxonKey, speciesKey)] %>%
    unique(by = c("taxonKey", "speciesKey")) %>%
    # make into a graph to detect components
    igraph::graph_from_data_frame(directed = FALSE) %>%
    igraph::components() %>%
    igraph::groups() %>%
    purrr::map_df(~tibble::tibble(taxonKey = .), .id = "key_id") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(key_id = stringr::str_pad(key_id, 5, pad = "0"),
                  key_id = paste("key", key_id, sep = "_"),
                  taxonKey = as.integer(taxonKey))
}

test <- function(){
  drake::loadd(gbif_keys)
  library(dplyr)
  library(data.table)

  groups <- get_gbif_key_groups(occurrences)

  inf_occurrences  <- dplyr::left_join(occurrences, groups)
  inf_gbif_keys <- dplyr::left_join(gbif_keys, groups, by = c("key" = "taxonKey"))

  inf_occurrences %>%
    dplyr::full_join(inf_gbif_keys, by = c("key_id")) %>%
    dplyr::group_by(key_id) %>%
    dplyr::mutate(unknown_queried_sp_name = all(is.na(queried_sp_name))) %>%
    dplyr::filter(unknown_queried_sp_name) %>%
    dplyr::select(taxonKey, scientificName, queried_sp_name)


  gbif_keys %>%
    dplyr::filter(stringr::str_detect(canonicalName, "Coereba"))
	  summary()
}

get_n_dirty_occurrences <- function(n_occurrences, n_cleaned_occurrences){
  n_occurrences %>%
    dplyr::mutate(clean = "no") %>%
    dplyr::bind_rows(dplyr::mutate(n_cleaned_occurrences, clean = "yes")) %>%
    tidyr::spread(clean, N) %>%
    dplyr::mutate(yes = dplyr::if_else(is.na(yes), 0L, yes),
                  loss = no - yes) %>%
    dplyr::transmute(taxonKey, N = loss) %>%
    dplyr::filter(N > 0)
}
