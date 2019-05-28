
# Function writen by Scott Chamberlain https://github.com/ropensci/taxize/issues/533
### For each input, pick one name, either synonym if found, or same name
### that use gave if no synonyms found
#' @export
#' @rdname synonyms
#' @examples
#' synonyms_gather(x)
#' synonyms_gather(x, FALSE)
synonyms_gather <- function(x, simplify = TRUE) {
  res <- list()
  for (i in seq_along(x)) {
    switch(
      attr(x, "db"),
      itis = {
        res[[ names(x)[i] ]] <- if ('acc_name' %in% names(x[[i]])) {
          unique(x[[i]]$acc_name)
        } else {
          unique(names(x)[i])
        }
      },
      col = {
        res[[ names(x)[i] ]] <-
          if (!is.null(x[[i]])) {
            
            unique(x[[i]]$acc_name)
          } else {
            unique(names(x)[i])
          }
      }
    )
  }
  return(res)
}


# Create A file for the synonyms if it doesn't exist already
get_synonym_dic <- function(file_path){
  if(!file.exists(file_path)){
    readr::write_csv(tibble::tibble("original_sp_name", "accepted_sp_name"), file_path, col_names = F)
  } 
  readr::read_csv(file_path)
}

get_synonyms <- function(stored_synonym_dict, this_spp){
  
  sp_in_dict <- this_spp$sp_name %in% stored_synonym_dict$original_sp_name
  
  if(all(sp_in_dict)) {
    return(
      tibble::tibble(original_sp_name = character(), 
                     accepted_sp_name = character()))
  } 
  
  this_spp$sp_name[!sp_in_dict] %>%
    taxize::synonyms(db = "itis", verbose = FALSE) %>%
    synonyms_gather() %>%{
      tibble::tibble(original_sp_name = names(.), accepted_sp_name = unlist(.)) 
    }
}
