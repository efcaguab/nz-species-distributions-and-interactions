
# Download itis synonym database. As a zip file
get_file <- function(itis_address, dest_file, download_date = NULL){
  if(!file.exists(dest_file)){
    message("donwloading data on ", download_date)
    download.file(itis_address, dest_file)
  } else {
    message("file already found")
    return(0)
  }
  
}
