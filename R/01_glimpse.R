#' Generate glimpse of dataset
#'
#' Many a times, you will receive multiple datasets to understand and having a glimpse of it stored in test files might come handy while coding.
#' This function generates glimpse of a data.frame using tibble::glimpse, and write to a text file.
#' Using same file name for different datasets will append the outputs to a same file.
#'
#' @param dataset A data.frame
#' @param output_filename Name of the output text file (should end in ".txt")
#' Strongly advised to pass this parameter, else the function's default is glimpse_timestamp.txt
#' @export
#' @examples
#'
#' glimpse_to_file(mtcars, "glimpse_mtcars.txt")
#' glimpse_to_file(iris,   "glimpse_iris.txt"  )

glimpse_to_file <- function(dataset, output_filename = gsub(x = paste0("glimpse_",Sys.time(),".txt"),pattern = " |:|-",replacement = "_")){
  sink(output_filename, type=c("output"),append = T)  # Ensures the glimpse output is written to output_filename text file
  tibble::glimpse(dataset)                            # Generates and writes the output of tibble::glimpse() to output_filename text file
  cat("\n");cat("\n")                                 # 2 blank lines at the end of output
  sink()                                              # Unmounts output_filename text file

  invisible()                                         # To return nothing
}
