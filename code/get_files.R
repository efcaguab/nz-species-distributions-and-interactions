
# DOwnload itis synonym database. As a zip file
get_file <- function(itis_address, dest_file, download_date = NULL){
  message("donwloading data on", download_date)
  download.file(itis_address, dest_file)
}
