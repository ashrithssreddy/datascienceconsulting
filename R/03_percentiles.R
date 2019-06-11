percentiles = function(dataset){
  dataset = dataset %>% select_if(is.numeric)
  percentiles = c(0,seq(0.000,0.010,by=0.001),seq(0,1,0.01),25,50,75,90:99,seq(99,100,0.01),seq(99.99,100,by=0.001),100) %>% unique %>% sort

  for(column_name in names(dataset)){
    cat(column_name)
    invisible(gc())

    data_vector = dataset[[column_name]]
    percentiles_one_column = sapply(percentiles ,FUN = function(x){quantile(data_vector,x/100,na.rm = TRUE) %>% unname}) %>%
      as.data.frame() %>%
      setNames("value") %>%
      cbind(percentiles) %>%
      select(2,1)

    xlsx::write.xlsx(percentiles_one_column,"Percentiles_Of_Numeric_Fields.xlsx",sheetName = column_name,append = TRUE)
  }
  invisible()
}
