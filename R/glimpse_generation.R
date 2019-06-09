rm(list=ls());cat("\014")

glimpse_generation = function(dataset, final_output_name = gsub(x = paste0("glimpse_",Sys.time(),".txt"),pattern = " |:|-",replacement = "_")){
  sink(final_output_name, type=c("output"))  # Ensures the output is written to 01_glimpse.txt going forward
  dplyr::glimpse(dataset)                   # Generates glimpse file
  sink()                                    # Ensures the output is written to console going forward
}