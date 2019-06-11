frequency_table = function(dataset, output_filename = "frequency_table.xlsx"){
  dataset = dataset %>% mutate_if(is.factor, as.character)
  setDT(dataset)
  wb = openxlsx::createWorkbook(output_filename)     # Creating and mounting an empty excel
  for(i in 1:ncol(dataset)){                                # Loop over each column
    print(paste0(names(dataset)[i]," - completed"))
    invisible(gc())

    sheetname = names(dataset)[i]
    openxlsx::addWorksheet(wb, ifelse(nchar(sheetname)>31, paste0(substr(sheetname,1,15),substr(sheetname,nchar(sheetname)-15,nchar(sheetname)))  , sheetname)) # Add a new sheet

    one_columns = dataset[,i,with = FALSE]                  # Retain column of interest
    one_columns = one_columns[,.(Count=.N),by = c(names(dataset)[i])] %>% as.data.frame() # Aggregate to get frequency
    one_columns$Percentage = 100*one_columns$Count/sum(one_columns$Count)
    one_columns = one_columns %>%
      arrange(desc(Count)) %>%
      mutate(sl_no = 1:nrow(.), string_length = nchar(.[[names(dataset)[i]]])) %>%
      select(sl_no,everything(),string_length) #

    openxlsx::writeData(wb, i, one_columns)                 # Write frequency table to excel
  }
  openxlsx::saveWorkbook(wb, file = output_filename) # Write and unmount final excel
  rm(dataset,i,one_columns,wb,sheetname)
  invisible(gc())
}
