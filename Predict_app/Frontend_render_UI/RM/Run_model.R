
# run model
observeEvent(input$run_model,{
  req(!is.null(omicsData$obj_scaled))
  req(!is.null(omicsData$obj_pp))
  req(!is.null(omicsData$model$model))
  
  # if the data is rolled up use that version, otherwise use OG slope version
  omicsNewdata = omicsData$obj_pp
  
  orig_names <- attr(omicsData$model$model,"feature_info")
  
  if("pepData" %in% class(omicsData$obj_scaled)){
    
    # remove proteins not found in model features
    pro_edata <- omicsNewdata$e_data
    pro_edata <- pro_edata[unlist(pro_edata[pmartR::get_emeta_cname(omicsNewdata)]) %in% orig_names$names_orig,]
    
    # find which are found in the OG version but not in the new data
    # we will want
    in_og_not_new <- orig_names$names_orig[
      !orig_names$names_orig %in% unlist(pro_edata[pmartR::get_emeta_cname(omicsNewdata)])]
    
    
    in_og_not_new_df <- data.frame(emetname = in_og_not_new,
                                   matrix(0, ncol = nrow(omicsNewdata$f_data),
                                          nrow = length(in_og_not_new))
    )
    
    colnames(in_og_not_new_df)[1] <- pmartR::get_emeta_cname(omicsNewdata)
    colnames(in_og_not_new_df)[-1] <- colnames(pro_edata)[-1]
    
    # add into old dataset
    pro_edata <- dplyr::bind_rows(pro_edata, in_og_not_new_df)
    
    # arrange them to match the original features
    pro_edata <- pro_edata[match(orig_names$names_orig,
                                 unlist(pro_edata[pmartR::get_emeta_cname(omicsNewdata)])),]
    
  } else {
    pro_edata <- omicsNewdata$e_data
    pro_edata <- pro_edata[unlist(pro_edata[pmartR::get_edata_cname(omicsNewdata)]) %in% orig_names$names_orig,]
    # find which are found in the OG version but not in the new data
    # we will want
    in_og_not_new <- orig_names$names_orig[!orig_names$names_orig %in% unlist(pro_edata[pmartR::get_edata_cname(omicsNewdata)])]
    
    in_og_not_new_df <- data.frame(edatname = in_og_not_new,
                                   matrix(0,ncol = nrow(omicsNewdata$f_data),nrow = length(in_og_not_new)))
    edatCol = which(colnames(in_og_not_new_df) == "edatname")
    colnames(in_og_not_new_df)[edatCol] <- pmartR::get_edata_cname(omicsNewdata)
    colnames(in_og_not_new_df)[-edatCol] <- colnames(pro_edata)[-edatCol]
    # add into old dataset
    pro_edata <- dplyr::bind_rows(pro_edata,in_og_not_new_df)
    # arrange them to match the original features
    pro_edata <- pro_edata[match(orig_names$names_orig,unlist(pro_edata[pmartR::get_edata_cname(omicsNewdata)])),]
  }
  
  # now set up new data to be ran with predictions
  new_info <- pro_edata[,-which(colnames(pro_edata) == pmartR::get_edata_cname(omicsNewdata))] %>%
    t() %>% data.frame()
  
  ## This ought to match the feature numbers from model maybe?
  
  names(new_info) <- orig_names$names_compact ## Should be identical with the rearrange above
  
  ###### Need difference for unsupervised here ######
  
  # unique levels in the model
  unique_outcomes <- attr(omicsData$model$model, "response_performance")$Group
  
  # Apply the function to each column and find the matching column name
  ## These may not be identical!
  
  if(!is.null(unique_outcomes)){
    col_matchy <- apply(omicsNewdata$f_data, 2, function(col){
      sum(col %in% unique_outcomes)
    })
  } else col_matchy <- 0
  
  if(any(col_matchy > 0)){
    
    matching_column <- colnames(omicsNewdata$f_data)[as.numeric(which.max(col_matchy))]
    
    fdata_sub <- omicsNewdata$f_data[c(pmartR::get_fdata_cname(omicsNewdata), matching_column)]
    
    ## Join response with data in the correct order
    new_info <- new_info %>%
      tibble::rownames_to_column(var = pmartR::get_fdata_cname(omicsNewdata)) %>%
      dplyr::left_join(fdata_sub, by = pmartR::get_fdata_cname(omicsNewdata))
    
    new_info <- new_info %>%
      dplyr::rename(response = !!as.symbol(matching_column)) %>%
      tibble::column_to_rownames(var = pmartR::get_fdata_cname(omicsNewdata))
    
    new_info[is.na(new_info)] <- 0
    
    # run predictions
    set.seed(2024)
    
    
    prediction_test_df <- tibble(
      response = factor(unlist(omicsNewdata$f_data[matching_column]),levels = unique_outcomes),
      .pred_class = unlist(predict(omicsData$model$model$fit$fit,new_data = new_info))
      ) %>%
      dplyr::bind_cols(
        data.frame(predict(omicsData$model$model$fit$fit,new_data = new_info,type = "prob"))
        ) %>%
      dplyr::mutate(
        `__SAMPNAMES__` = unlist(omicsNewdata$f_data[pmartR::get_fdata_cname(omicsNewdata)])
        )
    
    table_table_current$table$RM__model_eval <- prediction_test_df
    
    ## Replace the "prediction_test" attribute in the model with the new data results
    full_model_test <- omicsData$model$model
    attr(full_model_test, "prediction_test") <- prediction_test_df
    
    omicsData$obj_predictions <- full_model_test
    
    
  } else {
    matching_column <- NULL
    
    new_info[is.na(new_info)] <- 0
    
    # run predictions
    set.seed(2024)
    
    prediction_test_df <- data.frame(
      response = "Unknown", ## Keep the same for kicks and giggles
      # response = unlist(predict(omicsData$model$model$fit$fit,new_data = new_info)), ## Keep the same for kicks and giggles
      .pred_class = unlist(predict(omicsData$model$model$fit$fit,new_data = new_info)),
      predict(omicsData$model$model$fit$fit, 
              new_data = new_info, 
              type = "prob")
      )  %>%
      dplyr::mutate(
        `__SAMPNAMES__` = unlist(omicsNewdata$f_data[pmartR::get_fdata_cname(omicsNewdata)])
      )
    
    table_table_current$table$RM__model_eval <- prediction_test_df
    
    ## Replace the "prediction_test" attribute in the model with the new data results
    full_model_test <- omicsData$model$model
    attr(full_model_test, "prediction_test") <- prediction_test_df
    
    omicsData$obj_predictions <- full_model_test

  }
})