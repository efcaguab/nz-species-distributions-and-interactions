# merge species list from multiple interaction data sources
merge_spp <- function(wol_spp){
  suppressPackageStartupMessages({
    require(dplyr)
  })
  
  wol_spp %>%
    mutate(original_name = sp_name, 
           sp_unidentified = stringr::str_detect(sp_name, "M_PL"), 
           gen_unidentified = genus %in% c("Unientified",
                                           "Undefined", 
                                           "Unidentified"),
           # remove unwanted abbreviations in cannonical names
           sp_name = remove_abbreviations(sp_name), 
           is_subspecies = get_name_rank(sp_name), 
           is_subspecies = is_subspecies == "subspecies" & !sp_unidentified) %>%
    group_by(loc_id) %>%
    mutate(node_id = paste0(loc_id, "-", 
                            guild, "_",
                            stringr::str_pad(1:n(), width = 4, pad = "0"))) %>%
    ungroup()
}

# merge interactions from multiple interaction data sources
merge_int <- function(wol_int){
  wol_int
# remove uncertainity abbreviations, varieties stuff and simplify crosses
remove_abbreviations <- function(x){
  x %>%
    stringr::str_replace(stringr::fixed(" var."), "") %>%
    stringr::str_replace(stringr::fixed(" aff."), "") %>%
    stringr::str_replace(stringr::fixed(" cf."), "") %>%
    stringr::str_replace(" x .+", "")
}

merge_metadata <- function(wol_data, manual_net_locations){
  wol_data$metadata %>%
    # if there is a manual coordinate available, replace it
    dplyr::left_join(manual_net_locations, by = "net_name") %>% 
    dplyr::mutate(lat = dplyr::if_else(is.na(lat.y), lat.x, lat.y), 
                  lon = dplyr::if_else(is.na(lon.y), lon.x, lon.y)) %>% 
    dplyr::select(-tidyselect::contains(".x"), -tidyselect::contains(".y"))
}

downgrade_subspecies <- function(spp, col_name){
  col_name <- rlang::enquo(col_name)
  spp %>%
    dplyr::mutate(!!col_name := dplyr::if_else(
      condition = is_subspecies, 
      true = stringr::word(!!col_name, 1, 2), 
      false = !!col_name, 
      missing = !!col_name
    ))
}
