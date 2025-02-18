
unsup_combine <- function(new_omics, backfill_info, model_omics){

    #### create a fused fdata ####
    # new_omics <- omicsNewdata
    # model_omics <- omicsData$model$pp_omics
    
    if(!is.null(new_omics$f_data) && !is.null(model_omics$f_data)){
      
      ## Prioritize naming convention from new data
      fdata_cname <- get_fdata_cname(new_omics)
      f1 <- new_omics$f_data
      f2 <- model_omics$f_data
      
      f1$Source <- "New data"
      f2$Source <- "Model data"
      
      new_fdata <- full_join(f1, f2)
      
      ## Should knock out any nas
      if(get_fdata_cname(new_omics) != get_fdata_cname(model_omics)){

        new_fdata[[get_fdata_cname(new_omics)]] <- c(
          new_fdata[[get_fdata_cname(new_omics)]],
          f2[[get_fdata_cname(model_omics)]]
          )
      }
      
    } else if (!is.null(new_omics$f_data)){
      
      fdata_cname <- get_fdata_cname(new_omics)
      
      f1 <- new_omics$f_data
      f1$Source <- "New data"
      
      f2 <- data.frame(
        temp = colnames(model_omics$e_data)[colnames(model_omics$e_data) != get_edata_cname(model_omics)],
        Source = "Model data"
      )
      colnames(f2) <- c(fdata_cname, "Source")
      
      new_fdata <- full_join(f1, f2)
      
    } else if (!is.null(model_omics$f_data)){
      
      fdata_cname <- get_fdata_cname(model_omics)
      
      f1 <- data.frame(
        temp = colnames(new_omics$e_data)[colnames(new_omics$e_data) != get_edata_cname(new_omics)],
        Source = "New data"
      )
      colnames(f1) <-  c(fdata_cname, "Source")
      
      f2 <- model_omics$f_data
      f2$Source <- "Model data"
      
      new_fdata <- full_join(f1, f2)
      
    } else {
      
      fdata_cname <- "SampleID"
      
      f1 <- data.frame(
        temp = colnames(new_omics$e_data)[colnames(new_omics$e_data) != get_edata_cname(new_omics)],
        Source = "New data"
      )
      colnames(f1) <- fdata_cname
      
      f2 <- data.frame(
        temp = colnames(model_omics$e_data)[colnames(model_omics$e_data) != get_edata_cname(model_omics)],
        Source = "Model data"
      )
      colnames(f2) <- fdata_cname
      
      new_fdata <- full_join(f1, f2)
      
    }
    
    #### Make sure NAs are filled
    
    df1 <- new_fdata[new_fdata$Source == "New data",]
    df2 <- new_fdata[new_fdata$Source == "Model data",]
    
    df1[is.na(df1)] <- "New data"
    df2[is.na(df2)] <- "Model data"
    
    new_fdata <- rbind(df1, df2)
    
    #### create a fused edata ####
    
    ## Should be the same # of rows at this point in the same order
    new_edata <- cbind(backfill_info, model_omics$e_data[!(colnames(model_omics$e_data) %in% get_edata_cname(model_omics))])
    edata_cname <- get_edata_cname(new_omics)
    
    ## Should knock out double rows
    if(edata_cname != get_edata_cname(model_omics)){
      new_edata[[get_edata_cname(model_omics)]] <- NULL
    }
    
    ### Inherit e_meta from combined e_metas #### -- eventually implement? doesn't seem needed for the immediate future
    
    #### Generate a new slData
    
    fn_use <- switch(
      class(model_omics)[1],
      "lipidData" = "as.lipidData", 
      # "pepData" = "as.pepData",
      # "isobaricpepData" = "as.isobaricpepData", 
      "proData" = "as.proData",
      "metabData" = "as.metabData", 
      "nmrData" = "as.nmrData",
      "seqData" = "as.seqData"
    )
    
    object_fn <- get(fn_use)
    
    object_fn(
      e_data = new_edata, f_data = new_fdata,
      edata_cname = edata_cname, fdata_cname = fdata_cname,
      data_scale = get_data_scale(model_omics), 
      is_normalized = get_data_norm(model_omics)
    )
    
}
