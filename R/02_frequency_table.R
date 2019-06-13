frequency_table = function(dataset, output_filename = "frequency_table.xlsx", columns_to_exclude = c(), maximum_entries = 2^20, format_width = T, Sl_No_required = T, Frequency_required = T, Percentage_required = T, Cumulative_Percentage_required = F, String_Length_required = T, Sl_No_to_last = F){

  dataset = dataset[, !colnames(dataset) %in% columns_to_exclude] # Exclude columns of no interest
  dataset = dataset %>% dplyr::mutate_if(is.factor, as.character) # Change factor columns to character
  data.table::setDT(dataset)                                      # Changing class of dataset to data.table
  workbook = openxlsx::createWorkbook(output_filename)            # Creating and mounting an empty excel file

  for(i in 1:ncol(dataset)){                                      # Loop over each column
    sheet_name = names(dataset)[i]                                # Each column will be stored in a sheet of excel
    # Excel sheet name can have a maximum of 31 characters. If a column-name has more than 31 characters, below line will pick first 15 and last 16 characters.
    sheet_name = ifelse(nchar(sheet_name)>31, paste0(substr(sheet_name,1,15), substr(sheet_name,nchar(sheet_name)-15, nchar(sheet_name))), sheet_name)

    openxlsx::addWorksheet(workbook, sheet_name)                  # Add a new empty sheet for this particular column

    frequency_table = dataset[, i, with = FALSE]                  # Retain current column
    frequency_table = as.data.frame(frequency_table[,.(Frequency=.N), by = c(names(dataset)[i])]) # Aggregate to get frequency

    frequency_table = frequency_table %>%
      dplyr::arrange(desc(Frequency)) %>%                              # Sort by decreasing order of frequency
      dplyr::mutate(Percentage = 100*Frequency/sum(Frequency)) %>%     # Create new column, frequency to percentage
      dplyr::mutate(Sl_No = 1:nrow(.)) %>%                             # Create new column, serial number
      dplyr::mutate(Cumulative_Percentage = cumsum(Percentage)) %>%    # Create new column, Cumulative Percentage
      dplyr::mutate(String_Length = nchar(.[[names(dataset)[i]]])) %>% # Create new column, string length = number of characters in the value
      dplyr::select(Sl_No,everything(),String_Length)                  # Reorder columns

    # Excel supports a maximum of 2^20 rows (1048576). Therefore, Any entries below that in frequency table will not be written
    frequency_table = frequency_table[1:min(nrow(frequency_table),maximum_entries), ]

    if(format_width == T){
      openxlsx::setColWidths(workbook, sheet = i, cols = 1:5, widths = "auto")
    }
    if(Sl_No_to_last == T){
      frequency_table = frequency_table %>% dplyr::select(-Sl_No,everything(),Sl_No)
    }
    if(Sl_No_required == F){
      frequency_table = frequency_table %>% dplyr::select(-Sl_No)
    }
    if(Frequency_required == F){
      frequency_table = frequency_table %>% dplyr::select(-Frequency)
    }
    if(Percentage_required == F){
      frequency_table = frequency_table %>% dplyr::select(-Percentage)
    }
    if(Cumulative_Percentage_required == F){
      frequency_table = frequency_table %>% dplyr::select(-Cumulative_Percentage)
    }
    if(String_Length_required == F){
      frequency_table = frequency_table %>% dplyr::select(-String_Length)
    }

    openxlsx::writeData(workbook, i, frequency_table)                  # Write frequency table to workbook at ith position

    invisible(gc())                                                    # Silent garbage collection
    message(paste0(names(dataset)[i]," - Completed"))                  # For status printing
  }
  openxlsx::saveWorkbook(workbook, file = output_filename)             # Write final excel and unmount it from R
  rm(dataset,i,frequency_table,workbook,sheet_name)                    # Clean up the intermediate variables (Habit!)
  invisible(gc())                                                      # Silent garbage collection

  invisible()                                                          # To return nothing
}

if(F){
  # library("data.table")
  # cat("\014")
  # frequency_table(mtcars)
  # frequency_table(iris)
  # detach("package:data.table", unload = TRUE)
}
