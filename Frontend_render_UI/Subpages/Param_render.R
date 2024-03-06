


observeEvent(input$done_param_option, {
  updateBoxCollapse(session = session, id = "PO_RM_side_collapse", close = "side_param_RM")
  shinyjs::show("complete_param")
})

output$param_preview_plot_ui <- renderUI({
  
  
  if(!is.null(omicsData$objHP)){
    withSpinner(plotlyOutput("param_preview_plot"))
    
  } else {
    "No parameters have been optimized."
  }
})

output$param_preview_plot <- renderPlotly({
  
  req(!is.null(omicsData$objHP), cancelOutput = T)
  
  cv_res <- attr(omicsData$objHP, "hp_info")
  
  browser()
  
  temp <- attributes(res)
  
  autoplot(temp$hp_info, metric = "roc_auc") + ggplot2::theme_bw()
  
  # plot <- autoplot(
  #   mlFitted[[nm]]$worfklows,
  #   rank_metric = input$metricPicker,
  #   metric = input$metricPicker,
  #   select_best = FALSE
  # ) 
  
})