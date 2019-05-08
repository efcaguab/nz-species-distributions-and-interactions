# function to render markdown files in a drake friendly way
my_render <- function(file_in, file_out){
  rmarkdown::render(input = file_in)
}
