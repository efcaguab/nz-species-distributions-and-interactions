# function to read all netwokrks in a folder 
read_networks <- function(network_folder){
  # get network names
  net_names <- list.files(network_folder, full.names = F) %>%
    `[`(grepl("M_PL", .)) %>%
    tools::file_path_sans_ext()
  
  list.files(network_folder, full.names = T) %>%
    `[`(grepl("M_PL", .)) %>%
    purrr::map(readr::read_csv) %>%
    # remove mistakes 
    purrr::map(~dplyr::filter(., X1 != 'Abundance"')) %>%
    purrr::map(int_df_to_matrix) %>%
    `names<-`(net_names)
}  

# function to get an intearction data frame to a matrix with proper column and row names
int_df_to_matrix <- function(x){
  column_names <- names(x)[-1]
  row_names <- as.character(x$X1)
  as.matrix(x[, -1]) %>%
    `rownames<-`(row_names) %>%
    `colnames<-`(column_names)
}

# read metadata
read_metadata <- function(metadata_file){
  readr::read_csv(metadata_file, 
                  col_names = c("net_name", 
                                "n_spp", 
                                "n_int",
                                "c", 
                                "int_type", 
                                "data_type", 
                                "reference", 
                                "loc_name",
                                "lat", 
                                "lon"))
}

# x is a dataframe
remove_unknown_species <- function(x){
  x %>% 
    dplyr::filter_if(is.character, 
                     dplyr::all_vars(!grepl(pattern = "undefined", x = ., ignore.case = TRUE))) %>% 
    dplyr::filter_if(is.character, 
                     dplyr::all_vars(!grepl(pattern = "unidentified", x = ., ignore.case = TRUE)))
}
