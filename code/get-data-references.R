get_int_ref_dois <- function(int_metadata){
  require(dplyr)
  
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
  
  dois %>%
    mutate(reference = refs) %>%
    rename(likely_doi = doi)
  
}

# determine if a reference (string) is likely published
is_reference_published <- function(reference){
  reference %>%
    stringr::str_detect("unpubl", negate = TRUE)
}
