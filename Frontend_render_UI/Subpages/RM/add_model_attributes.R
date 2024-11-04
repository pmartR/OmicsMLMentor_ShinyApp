### Additional SLOPE model attributes

add_model_attributes <- function(model, supervised = T){
  
  model <- add_SLOPE_model_name(model)
  if(supervised) model <- add_attr_response_perf(model)
  model
  
}

add_SLOPE_model_name <- function(model){
  attr(model, "SLOPE_model_name") <- input$input$pick_model_EM
}

add_attr_response_perf <- function(model){
  
  ### Always prioritize test results
  if(!is.null(attr(model, "prediction_test"))){
    pred_df <- attr(model, "prediction_test")
  } else {
    pred_df <- attr(model, "prediction_train")
  }
  
  ### Need continuous catch
  browser()
  
  ### Grab classes
  if (length(unique(pred_df$response)) == 2) {
    pos_class <- names(pred_df)[3]
    p <- yardstick::roc_curve(pred_df, response, dplyr::all_of(pos_class))  
    p[".level"] <- gsub(".pred_", "", pos_class)
  } else {
    pos_class <- names(pred_df)[3:(2+length(unique(pred_df$response)))]
    p <- yardstick::roc_curve(pred_df, response, dplyr::all_of(pos_class))   
  }
  
  ## Summarize data
  auc_by_level = p %>%
    dplyr::group_by(.level) %>%
    dplyr::mutate(spc_diff = specificity - dplyr::lag(specificity), 
                  sens_avg = (sensitivity + dplyr::lag(sensitivity))/2) %>%
    dplyr::summarise(sum(spc_diff*sens_avg, na.rm=T))
  
  ## Rename
  names(auc_by_level)[1] <- "Group"
  names(auc_by_level)[2] <- "AUC of ROC"
  
  attr(model, "response_performance") <- auc_by_level
}
