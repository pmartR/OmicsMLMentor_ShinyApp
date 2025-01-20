
output$transformation_backfill_text <- renderText({
  
  req(!is.null(omicsData$model) && !is.null(omicsData$obj))
  
  model_names_og <- attributes(omicsData$model$model)$feature_info$names_orig
  
  # determine the number of 
  num_in_og <- length(model_names_og)
  
  if("pepData" %in% class(omicsData$obj)){
    # use emeta information
    emeta_cname = pmartR::get_emeta_cname(omicsData$obj)
    num_in_both <- sum(unique(omicsData$obj$e_meta[[emeta_cname]]) %in% model_names_og)
    num_in_just_new <- sum(!unique(omicsData$obj$e_meta[[emeta_cname]]) %in% model_names_og)
  } else {
    edata_cname = pmartR::get_edata_cname(omicsData$obj)
    num_in_both <- sum(unique(omicsData$obj$e_data[[edata_cname]]) %in% model_names_og)
    num_in_just_new <- sum(!unique(omicsData$obj$e_data[[edata_cname]]) %in% model_names_og)
  }
  num_in_og_not_new <- num_in_og - num_in_both
  
  paste0("The original model had ", num_in_og, " distinct molecules. Of those molecules, ", num_in_og_not_new,
         " were not identified in the new dataset and will need to be backfilled as 0s for the model to run.", 
         " Additionally, ", num_in_just_new, " molecules were identified in only the new dataset and therefore ",
         "will be removed prior to running the model.")
  
})

observeEvent(c(omicsData$model, omicsData$obj), {
  
  req(!is.null(omicsData$model) && !is.null(omicsData$obj))
  # determine the number of 
  num_in_og <- length(attributes(omicsData$model$model)$feature_info$names_orig)
  
  if("pepData" %in% class(omicsData$obj)){
    # use emeta information
    emeta_cname = pmartR::get_emeta_cname(omicsData$obj)
    num_in_both <- sum(unique(omicsData$obj$e_meta[[emeta_cname]]) %in% attributes(omicsData$model$model)$feature_info$names_orig)
    num_in_just_new <- sum(!unique(omicsData$obj$e_meta[[emeta_cname]]) %in% attributes(omicsData$model$model)$feature_info$names_orig)
  } else {
    edata_cname = pmartR::get_edata_cname(omicsData$obj)
    num_in_both <- sum(unique(omicsData$obj$e_data[[edata_cname]]) %in% attributes(omicsData$model$model)$feature_info$names_orig)
    num_in_just_new <- sum(!unique(omicsData$obj$e_data[[edata_cname]]) %in% attributes(omicsData$model$model)$feature_info$names_orig)
  }
  num_in_og_not_new <- num_in_og - num_in_both
  backfill_plot <- data.frame(Dataset = factor(c("Only Original","Both", "Only New"),levels = c("Only Original","Both", "Only New")),
                              Value = c(num_in_og_not_new,num_in_both,num_in_just_new)) %>%
    ggplot(aes(x = Dataset, y = Value,fill = Dataset)) + 
    geom_col() +
    geom_text(aes(label = paste0("n = ",Value),y = Value + 200)) +
    theme_bw() + 
    labs(y = "Number of Distinct Proteins Identified",x = "") + 
    guides(fill = "none")
  
  
  plot_table_current$table$PP__backfill <- backfill_plot
  
})

output$transformation_backfill_plot <- renderPlotly({
  ggplotly(plot_table_current$table$PP__backfill)
})
