# get species lists by network and locality
get_species_list <- function(networks, metadata){
  
  locality_info <- metadata %>%
    dplyr::select(net_name, loc_name, lat, lon)
  
  plants <- networks %>%
    purrr::map_df(~tibble::data_frame(sp_name = rownames(.)), .id = "net_name") %>%
    dplyr::mutate(guild = "pla")
  pollinators <- networks %>%
    purrr::map_df(~tibble::data_frame(sp_name = colnames(.)), .id = "net_name") %>%
    dplyr::mutate(guild = "pol")
  
  dplyr::bind_rows(plants, pollinators) %>%
    dplyr::mutate(genus = get_first_word(sp_name)) %>%
    dplyr::inner_join(locality_info, by = "net_name")
}

# return the first word of the string (which is assumed to be the genus in species name)
get_first_word <- function(x){
  gsub(pattern = "\\s.+", 
       replacement = "", 
       x = x)
}

get_interaction_list <- function(networks, metadata){
  
  locality_info <- metadata %>%
    dplyr::select(net_name, loc_name, lat, lon)
  
  networks %>%
    purrr::map_df(interactions_as_df, .id = "net_name")  %>%
    dplyr::mutate(pla_genus = get_first_word(pla_name), 
                  pol_genus = get_first_word(pol_name))  %>%
    dplyr::inner_join(locality_info, by = "net_name")
}

interactions_as_df <- function(x){
  as.data.frame.table(x) %>% 
    dplyr::filter(Freq > 0) %>%
    dplyr::mutate_if(is.factor, as.character) %>% 
    `names<-`(c("pla_name", "pol_name", "int_weight"))
}

# get frequency of group (eg sp_name or genus)
get_sp_freq_by_group <- function(species_list, group){
  group_var <- dplyr::enquo(group)
  species_list %>%
    dplyr::group_by(!!group_var, guild) %>%
    dplyr::summarise(n_net = dplyr::n_distinct(net_name), 
                     n_loc = dplyr::n_distinct(loc_name)) %>%
    dplyr::group_by(guild) %>%
    dplyr::mutate(n_net_rank = dplyr::row_number(dplyr::desc(n_net)),
                  n_loc_rank = dplyr::row_number(dplyr::desc(n_loc)))
}

# get frequency of interactions by group (eg pla_name & pol_name or pla_genus $pol_genus)
get_int_freq_by_group <- function(interaction_list, pla_group, pol_group){
  pla_group_var <- dplyr::enquo(pla_group)
  pol_group_var <- dplyr::enquo(pol_group)
  
  interaction_list %>%
    dplyr::group_by(!!pla_group_var, !!pol_group_var) %>%
    dplyr::summarise(n_net = dplyr::n_distinct(net_name), 
                     n_loc = dplyr::n_distinct(loc_name)) %>%
    dplyr::group_by() %>%
    dplyr::mutate(n_net_rank = dplyr::row_number(dplyr::desc(n_net)),
                  n_loc_rank = dplyr::row_number(dplyr::desc(n_loc)))
}

#x is a dataframe
humanize <- function(x){
  if ("guild" %in% names(x)) {
    x %<>%
      dplyr::mutate(guild = dplyr::case_when(
        guild == "pla" ~ "plants", 
        TRUE ~ "pollinators"
      ))
  }
  if ("focal" %in% names(x)) {
    x %<>%
      dplyr::mutate(focal = dplyr::if_else(focal, 
                                           "selected", 
                                           "not-selected"))
  }
  x
}

get_focal_species_options <- function(species_list, interaction_list, categ = "loc") {
  filter_var <- paste0("n_", categ)
  
  sp_rank <- species_list %>%
    remove_unknown_species() %>% 
    get_sp_freq_by_group(sp_name) 
  
  sp_values <- sp_rank %>%
  {x <- .; unique(x[filter_var])} %>%
    extract2(1)
  
  int_rank <- interaction_list %>%
    remove_unknown_species() %>% 
    get_int_freq_by_group(pla_name, pol_name)
  
  int_values <- int_rank %>%
  {x <- .; unique(x[filter_var])} %>%
    extract2(1)
  
  list(sp_threshold = sp_values, int_threshold = int_values) %>%
    purrr::cross() %>%
    purrr::map_df(purrr::lift(get_focal_species), 
                  sp_rank = sp_rank, int_rank = int_rank, filter_var = filter_var)
}

# get focal
get_focal_species <- function(sp_threshold, int_threshold, sp_rank, int_rank, filter_var){
  
  sp <- sp_rank %>%
    dplyr::group_by() %>%
    {d <- . ; d[d[filter_var] >= sp_threshold, ]} %>% 
    dplyr::select(guild, sp_name)
  
  int <- int_rank %>%
    dplyr::group_by() %>%
    {d <- . ; d[d[filter_var] >= int_threshold, ]} %>% 
    dplyr::select(pla_name, pol_name) %>%
    gather_pla_pol() %>% 
    dplyr::distinct()
  
  dplyr::intersect(sp, int) %>%
    dplyr::mutate(sp_threshold = sp_threshold, 
                  int_threshold = int_threshold, 
                  scale = filter_var) 
}

# gather pla_name and pol_name into guild and sp_name
gather_pla_pol <- function(x){
  x %>%
    tidyr::gather("guild", "sp_name", pla_name, pol_name) %>%
    dplyr::mutate(guild = substr(guild, 1,3))
}