# merge species list from multiple interaction data sources
merge_spp <- function(wol_spp){
  wol_spp
}

# merge interactions from multiple interaction data sources
merge_int <- function(wol_int){
  wol_int
}

merge_metadata <- function(wol_data){
  wol_data$metadata
}

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
