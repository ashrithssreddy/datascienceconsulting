# This function generates glimpse view of the data.frame passed into it
# You are stringly advides to pass output_name to function, else it will create text file

glimpse_generation <- function(dataset, output_name = gsub(x = paste0("glimpse_",Sys.time(),".txt"),pattern = " |:|-",replacement = "_")){
  sink(output_name, type=c("output"))  # Ensures the output is written to 01_glimpse.txt going forward
  dplyr::glimpse(dataset)              # Generates glimpse file
  sink()                               # Ensures the output is written to console going forward
}
