

imputation_function <- function(omicsData_use, thresholds = NULL){
  
  ## Just so we don't have to touch make_filters
  if(is.null(thresholds)){
    thresholds <- list(
      keep = missingHandleSliderValsFilter()$md_keep,
      impute = missingHandleSliderValsFilter()$md_impute,
      convert = missingHandleSliderValsFilter()$md_convert,
      remove = missingHandleSliderValsFilter()$md_remove
    )
  }
  
  out <- if(!inherits(omicsData_use, "pepData")){
    edata_nathresh_transform(as.slData(omicsData_use), thresholds)
    
  } else {
    
    ## Get handling of protein-level data
    transform_df <- get_transform_df(as.slData(pepQCData$objQCPro), thresholds)
    
    ## Determine peptide handling based on protein handling
    combined_handling <- left_join(
      omicsData_use$e_meta[c(get_edata_cname(omicsData_use), get_emeta_cname(omicsData_use))],
      transform_df
    )
    
    transform_df2 <- combined_handling[!(colnames(combined_handling)%in% get_emeta_cname(omicsData_use))]
    
    ## Only apply imutation -- convert and remove applied on protein level
    transform_df2$Handling != "Estimate" <- "Keep"
    
    ## Degenerate peptides catch
    degen_catch <- duplicated(transform_df2[[get_edata_cname(omicsData_use)]])
    transform_df2 <- transform_df2[!degen_catch,]
    
    ## Impute as needed
    edata_nathresh_transform(as.slData(omicsData_use), transform_df2)
    
  }
  
  ## Conserve all attributes
  transfer <- attributes(out)
  transfer$class <- attr(omicsData_use, "class")
  extra_attr <- attributes(omicsData_use)[!(names(attributes(omicsData_use)) %in% names(transfer))]
  transfer <- c(transfer, extra_attr)
  
  attributes(omicsData_use) <- transfer
  omicsData_use$e_data <- out$e_data
  omicsData_use$f_data <- out$f_data
  omicsData_use$e_meta <- out$e_meta
  
  omicsData_use

  
}