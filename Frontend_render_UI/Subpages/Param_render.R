


observeEvent(input$done_param_option, {
  updateBoxCollapse(session = session, id = "PO_RM_side_collapse", close = "side_param_RM")
  shinyjs::show("complete_param")
})

output$param_preview_plot_ui <- renderUI({
  
  
  
  # withSpinner(plotOutput("param_preview_plot"))
  
  
  "No parameters have been optimized."
  
})