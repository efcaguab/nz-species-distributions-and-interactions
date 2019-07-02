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
           sp_name = stringr::str_replace(sp_name, stringr::fixed(" var."), ""), 
           sp_name = stringr::str_replace(sp_name, stringr::fixed(" aff."), ""),
           sp_name = stringr::str_replace(sp_name, stringr::fixed(" cf."), ""), 
           is_subspecies = get_name_rank(sp_name), 
           is_subspecies = is_subspecies == "subspecies" & !sp_unidentified, 
           # simplify crosses
           sp_name = stringr::str_replace(sp_name, " x .+", "")) %>%
    group_by(loc_id) %>%
    mutate(node_id = paste0(loc_id, "-", 
                            guild, "_",
                            stringr::str_pad(1:n(), width = 4, pad = "0"))) %>%
    ungroup()
}

# merge interactions from multiple interaction data sources
merge_int <- function(wol_int){
  wol_int
}

merge_metadata <- function(wol_data){
  wol_data$metadata
}

downgrade_subspecies <- function(spp){
  spp %>%
    dplyr::mutate(sp_name = dplyr::if_else(
      condition = is_subspecies, 
      true = stringr::word(sp_name, 1, 2), 
      false = sp_name, 
      missing = sp_name
    ))
}