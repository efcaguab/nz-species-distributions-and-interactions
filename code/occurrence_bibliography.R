# Save a bib file for a list of dois from GBIF
get_occurrence_refs <- function(dois){
  
  occurrences_bib <- RefManageR::GetBibEntryWithDOI(dois)
  
  # update GBIF name cause its ugly
  change_author_familyname <- function(x, new_name){
    x$author$family <- new_name
    x
  }
  
  update_ref_key <- function(x){
    names(x) <- basename(names(x)) %>%
      paste0("gbif_", .)
    x
  }
  
  update_bib_type <- function(x){
    x$bibtype <- "Electronic"
    x
  }
  
  occurrences_bib %>%
    purrr::map(change_author_familyname,
               new_name = "GBIF.org") %>%
    purrr::map(update_ref_key) %>%
    purrr::map(update_bib_type)
  
}

write_bib <- function(references, bib_file){
  
  # start fresh if the file already exists
  if (file.exists(bib_file))
    file.remove(bib_file)
  
  references %>%
    purrr::walk(RefManageR::WriteBib, 
                file = bib_file, 
                append = TRUE)
}

