# get list of web of life networks for a particular interaction type. 
# Available types include
# mutualistic
# "3" (plant-ant)
# "5" (plant-pollinator)
# "6" (plant-seed disperser)
# "11" (anemone-fish)
# antagonistic
# "8" (host-parasite)
# "10" (plant-herbivore)
# "7" (food webs)
get_wol_networks <- function(int_type = "plant-pollinator", download_date = NULL){
  type_id <- dplyr::case_when(
    int_type == "plant-pollinator" ~ 5L)
  
  "http://www.web-of-life.es/networkslist.php?type=" %>%
    paste0(type_id, "&data=All") %>%
    readLines() %>%
    glue::glue_collapse() %>%
    rjson::fromJSON()
}

# downloads the a zip file containing the network data and the metadata of the
# networks specified in the zip file
download_wol_network_zip <- function(nets, out_file, species_name = TRUE){
  
  # Variables used to build the query url
  format <- "csv"
  speciesName <- FALSE
  if (species_name) speciesName <- "yes"
  net_list <- purrr::map(nets, function(x) x$networkName) %>%
    unlist() %>% 
    paste(collapse = ",")
  
  paste0("http://www.web-of-life.es/", 
         "map_download_fast2.php?format=", 
         format, 
         "&networks=", 
         net_list, 
         "&species=", 
         speciesName, 
         "&type=&data=&speciesrange=", 
         "&interactionsrange=&searchbox=&checked=") %>%
    download.file(out_file)
}


# get a zip file and return all the networks inside it with their metadata
read_wol_data <- function(zip_file){
  dir <- tempdir()
  unzip(zip_file, exdir = dir)
  list(
    netwokrs = read_wol_networks(dir), 
    metadata = read_wol_metadata(file.path(dir, "references.csv"))
  )
}


# function to read all web of life netwokrks in a folder 
read_wol_networks <- function(network_folder){
  # get network names
  net_names <- list.files(network_folder, 
                          full.names = F, 
                          pattern = "M_PL") %>%
    tools::file_path_sans_ext()
  
  suppressMessages({
    suppressWarnings({
      list.files(network_folder, full.names = T, pattern = "M_PL") %>%
        purrr::map(readr::read_csv) %>%
        # remove mistakes 
        purrr::map(~dplyr::filter(., X1 != 'Abundance"')) %>%
        purrr::map(int_df_to_matrix) %>%
        `names<-`(net_names)
    })
  })
}  

# function to get an intearction data frame to a matrix with proper column and row names
int_df_to_matrix <- function(x){
  column_names <- names(x)[-1]
  row_names <- as.character(x$X1)
  as.matrix(x[, -1]) %>%
    `rownames<-`(row_names) %>%
    `colnames<-`(column_names)
}

# read wol metadata
read_wol_metadata <- function(metadata_file){
  suppressMessages({
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
  })
}

