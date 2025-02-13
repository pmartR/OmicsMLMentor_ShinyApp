


observeEvent(input$done_param_option, {
  updateBoxCollapse(session = session, id = "PO_RM_side_collapse", close = "side_param_RM")
  shinyjs::show("complete_param")
})

output$param_preview_plot_ui <- renderUI({
  
  if(!is.null(omicsData$objHP)){
    
    # hp_res <- attr(omicsData$objHP, "hp_info")
    # 
    # p <- autoplot(hp_res, metric = "roc_auc", select_best = F) + 
    #   ggplot2::theme_bw()
    
    # if(which(colnames(p$data) == "mean") < 3){
      withSpinner(plotlyOutput("param_preview_plotly"))
    # } else {
    #   withSpinner(plotlyOutput("param_preview_plot"))
    # }
    
  } else {
    "No parameters have been optimized."
  }
})

observeEvent(omicsData$objHP, {
  req(!is.null(omicsData$objHP), cancelOutput = T)
  
  hp_res <- attr(omicsData$objHP, "hp_info")
  
  p <- autoplot(hp_res, metric = "roc_auc", select_best = F) + 
    ggplot2::theme_bw()
  
  plot_table_current$table$RM__param_optim <- p
  table_table_current$table$RM__param_optim <- unnest(
    hp_res[2:3], cols = c(".metrics"))
})

output$param_preview_plotly <- renderPlotly({
  
  req(!is.null(plot_table_current$table$RM__param_optim), cancelOutput = T)
  
  plot_table_current$table$RM__param_optim
  
})

# output$param_preview_plot <- renderPlotly({
#   
#   req(!is.null(plot_table_current$table$RM__param_optim), cancelOutput = T)
#   
#   plot_table_current$table$RM__param_optim
#   
# })

## Set the optimum
observeEvent(c(omicsData$objHP, input$rec_param_option), {
  
  list_hp <- list(
    trees = NULL,
    min_n = NULL,
    mtry = NULL,
    cost = NULL,
    svm_margin = NULL,
    margin = NULL,
    degree = NULL,
    scale_factor = NULL,
    rbf_sigma = NULL,
    penalty = NULL,
    mixture = NULL,
    tree_depth = NULL,
    loss_reduction = NULL,
    learn_rate = NULL,
    stop_iter = NULL,
    sample_prop = NULL
  )
  
  if(!is.null(omicsData$objHP)){
    hp_res <- attr(omicsData$objHP, "hp_info")
    
    ## DF terminology doesn't always match ours fyi
    best_df <- hp_res %>% select_best(metric = "roc_auc")
    
    suppressWarnings({
      list_hp <- list(
        trees = best_df$trees,
        min_n = best_df$min_n,
        mtry = best_df$mtry,
        cost = best_df$cost,
        svm_margin = best_df$svm_margin,
        margin = best_df$margin,
        degree = best_df$degree,
        scale_factor = best_df$scale_factor,
        rbf_sigma = best_df$rbf_sigma,
        penalty = best_df$input$penalty,
        mixture = best_df$input$mixture,
        tree_depth = best_df$tree_depth,
        loss_reduction = best_df$loss_reduction,
        learn_rate = best_df$learn_rate,
        stop_iter = best_df$stop_iter,
        sample_prop = best_df$sample_size
      )
    })
    
    if(input$pick_model_EM == "svm"){
      list_hp$svm_margin <- list_hp$margin
    }
    
    list_hp_non_null <- Filter(Negate(is.null), list_hp)
    list_hp_null <- list_hp[!(names(list_hp) %in% names(list_hp_non_null))]
    
    map(names(list_hp_non_null), function(nm){
      updateNumericInput(session, nm, value = list_hp_non_null[[nm]])
    })
    
    map(names(list_hp_null), function(nm){
      reset(nm)
    })
    
  } else {
    map(names(list_hp), function(nm){
      reset(nm)
    })
  }
  
})
