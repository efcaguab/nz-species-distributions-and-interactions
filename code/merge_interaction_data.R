# merge species list from multiple interaction data sources
merge_spp <- function(wol_spp){
  require(dplyr)
  wol_spp %>%
    mutate(sp_unidentified = stringr::str_detect(sp_name, "M_PL"), 
           gen_unidentified = genus %in% c("Unientified", "Undefined", "Unidentified")) %>%
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
