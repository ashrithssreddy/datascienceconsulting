#' Glimpse generation
#'
#' Glimpse Function implementation from tibble.
#' Typically you'll receive multiple files before collating them into a single ADS
#' This function generates glimpse view of the data.frame passed into it
#' You are strongly advided to pass output_name to function, else it will create text file with time stamp
#'
#' @param dataset data.frame passed to tibble's glimpse
#' @param output_name names of final output file
#'
#' @return
#' No objects are returned
#' Ouptut will be a text file written to working directory
#'
#' @export
glimpse_generation <- function(dataset, output_name = gsub(x = paste0("glimpse_",Sys.time(),".txt"),pattern = " |:|-",replacement = "_")){
  sink(output_name, type=c("output"))  # Ensures the output is written to 01_glimpse.txt going forward
  dplyr::glimpse(dataset)              # Generates glimpse file
  sink()                               # Ensures the output is written to console going forward
}
