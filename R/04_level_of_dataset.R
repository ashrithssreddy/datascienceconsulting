level_of_data = function(dataset){
  setDF(dataset)

  generate_column_combinations = function(dataset,n){
    utils::combn(names(dataset),n) %>%
      t %>%
      data.frame() %>%
      setNames(paste0("V",1:length(.))) %>%
      as.matrix()
  }

  check_for_level = function(dataset , column_combinations){
    for(j in 1:nrow(column_combinations)){
      concatenated_combination = dataset %>%
        select(column_combinations[j,]) %>%
        tidyr::unite(concatenated,sep = ";;;") %>%
        pull(concatenated)

      residual = length(concatenated_combination) -  length(unique(concatenated_combination))

      cat(paste(column_combinations[j,],collapse = " x "),"----","residual:",format(residual,big.mark=",",scientific=F),"\n")
      cat(paste(column_combinations[j,],collapse = " x "),"----","residual:",format(residual,big.mark=","),"\n")

      if(residual == 0){
        message(paste(paste(column_combinations[j,],collapse = " x "),"IS A LEVEL"))
      }else{
        print(paste(paste(column_combinations[j,],collapse = " x "),"IS NOT A LEVEL"))
      }
      cat("\n")
      rm(concatenated_combination,residual);invisible(gc());
    }
  }

  for(i in 1:ncol(dataset)){
    column_combinations = generate_column_combinations(dataset,i)
    check_for_level(dataset,column_combinations)
  }

  invisible()
}
