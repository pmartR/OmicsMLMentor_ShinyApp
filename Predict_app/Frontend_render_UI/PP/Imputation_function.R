imputation_function <- function(omicsData_use, thresholds){
  
  out <- if(!inherits(omicsData_use, "pepData")){
    edata_nathresh_transform(as.slData(omicsData_use), thresholds)
    
  } else {
    
    ## Get handling of protein-level data
    ## which means we first need to roll up the data
    # pepQCData$objQCPro <- protein_quant(edata_transform(omicsData$objQC, "log2"),
    #                                     method = input$qc_which_rollup,
    #                                     qrollup_thresh = input$qc_qrollup_thresh / 100,
    #                                     single_pep = single_pep,
    #                                     single_observation = single_observation,
    #                                     combine_fn = input$qc_which_combine_fn,
    #                                     parallel = TRUE
    # )
    rollup_method = attr(omicsData$model$pp_omics,"pro_quant_info")$method

    objQCPro <- pmartR::protein_quant(omicsData_use, method = rollup_method)
    transform_df <- get_transform_df(as.slData(objQCPro), thresholds)
    
    ## Determine peptide handling based on protein handling
    combined_handling <- left_join(
      omicsData_use$e_meta[c(pmartR::get_edata_cname(omicsData_use), pmartR::get_emeta_cname(omicsData_use))],
      transform_df
    )
    
    transform_df2 <- combined_handling[!(colnames(combined_handling)%in% pmartR::get_emeta_cname(omicsData_use))]
    
    ## Only apply imutation -- convert and remove applied on protein level
    transform_df2$Handling[transform_df2$Handling != "Estimate"] <- "Keep"
    
    
    ## Degenerate peptides catch
    degen_catch <- duplicated(transform_df2[[pmartR::get_edata_cname(omicsData_use)]])
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
