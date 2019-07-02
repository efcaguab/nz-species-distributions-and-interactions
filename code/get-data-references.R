# Get references of data sources
get_int_ref_dois <- function(int_metadata, out_file){
  suppressPackageStartupMessages({
    require(dplyr)
  })
  # If there no previous file storing that info download it
  if (!file.exists(out_file)) {
    refs <- int_metadata %>%
      dplyr::mutate(published = is_reference_published(reference)) %>%
      dplyr::filter(published) %$%
      reference %>%
      unique() 
    
    dois <- refs %>% 
      purrr::map_df(~ rcrossref::cr_works(query = ., 
                                          limit = 1, 
                                          sort = "score", 
                                          select = "DOI")$data)
    
    interaction_references <- dois %>%
      mutate(reference = refs) %>%
      rename(likely_doi = doi)
    
    readr::write_csv(interaction_references, out_file)
  }
  readr::read_csv(out_file, col_types = "cc")
}

# determine if a reference (string) is likely published
is_reference_published <- function(reference){
  reference %>%
    stringr::str_detect("unpubl", negate = TRUE)
}
