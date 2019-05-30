# merge species list from multiple interaction data sources
merge_spp <- function(wol_spp){
  wol_spp %>%
    dplyr::mutate(sp_unidentified = stringr::str_detect(sp_name, "M_PL"), 
                  gen_unidentified = genus %in% c("Unientified", "Undefined", "Unidentified"))
}

# merge interactions from multiple interaction data sources
merge_int <- function(wol_int){
  wol_int
}

merge_metadata <- function(wol_data){
  wol_data$metadata
}
