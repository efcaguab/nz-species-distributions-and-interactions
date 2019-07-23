ints_as_nets <- function(interactions_org){
  interactions_org %>%
    split(.$loc_id) %>%
    purrr::map(dplyr::select, -loc_id) %>%
    purrr::map(igraph::graph_from_data_frame, directed = FALSE) 

}

calc_org_degree <- function(nets){
  degree_per_net <- nets %>%
    purrr::map(igraph::degree) %>%
    purrr::map_dfr(~tibble::tibble(org_id = names(.), k = .), .id = "loc_id")
  
  global_degree <- purrr::lift_dl(igraph::union)(nets) %>%
    igraph::degree() %>%
    tibble::tibble(org_id = names(.), k_global = .)
  
  net_size <- nets %>%
    purrr::map(igraph::V) %>%
    purrr::map(length) %>%
    purrr::imap_dfr(~tibble(loc_id = .y, n = .x))
  
  degree_per_net %>%
    dplyr::full_join(global_degree, by = "org_id") %>%
    dplyr::full_join(net_size, by = "loc_id") %>% 
    dplyr::mutate(normalised_k = k/n, 
                  proportion_k = k/k_global)
}

get_possible_interactions <- function(interactions_org){
  interactions_org %>%
    dplyr::distinct() %>%
    split(.$loc_id) %>%
    purrr::map(tidyr::complete, pla_id, ani_id) %>%
    purrr::map_dfr(dplyr::rename, int = loc_id, .id = "loc_id") %>%
    dplyr::mutate(int = !is.na(int)) 
}

get_realised_to_possible <- function(possible_interactions){
  
  possible_interactions <-interactions_org %>%
    dplyr::distinct() %>%
    split(.$loc_id) %>%
    purrr::map(tidyr::complete, pla_id, ani_id) %>%
    purrr::map_dfr(dplyr::rename, int = loc_id, .id = "loc_id") %>%
    dplyr::mutate(int = !is.na(int)) 
  
  possible_degree <- possible_interactions %>%
    dplyr::group_by(pla_id, ani_id) %>%
    dplyr::filter(any(int)) %>% 
    tidyr::gather(key = "guild", 
                  value = "org_id", 
                  ani_id, pla_id) %>%
    dplyr::group_by(org_id) %>%
    dplyr::group_by(loc_id, org_id) %>%
    dplyr::summarise(k_possible = dplyr::n(), 
                     k = sum(int)) 
  
  global_degree <- interactions_org %>% 
    dplyr::distinct() %>%
    tidyr::gather(key = "guild", 
                  value = "org_id", 
                  ani_id, pla_id) %>%
    dplyr::group_by(org_id) %>%
    dplyr::summarise(k_global = dplyr::n())
}
