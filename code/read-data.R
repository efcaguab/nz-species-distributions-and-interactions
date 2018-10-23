# function to read all netwokrks in a folder 
read_networks <- function(network_folder){
  # get network names
  net_names <- list.files(network_folder, full.names = F) %>%
    `[`(grepl("M_PL", .)) %>%
    tools::file_path_sans_ext()
  
  list.files(network_folder, full.names = T) %>%
    `[`(grepl("M_PL", .)) %>%
    purrr::map(read.csv) %>%
    purrr::map(int_df_to_matrix) %>%
    `names<-`(net_names)
}  

# function to get an intearction data frame to a matrix with proper column and row names
int_df_to_matrix <- function(x){
  column_names <- names(x)[-1]
  row_names <- as.character(x$X)
  as.matrix(x[, -1]) %>%
    `rownames<-`(row_names) %>%
    `colnames<-`(column_names)
}
