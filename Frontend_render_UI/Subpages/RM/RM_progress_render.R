output$RM_progress_summary <- renderUI({
  DTOutput("RM_progress_summary_table")
})

output$RM_progress_summary_table <- renderDT({
  summary(omicsData$objRM)
})

output$RM_progress_next_steps <- renderUI({
  tags$ul(
    tags$li("Plot Download Selections"),
    tags$li("Table Download Selections"),
    tags$li("Report Creation"),
    tags$li("Saving Your Model"),
  )
})

output$RM_progress_plot <- renderUI({
  plotlyOutput("RM_progress_plot_render")
})

output$RM_progress_plot_render <- renderPlotly({
  plot_table_current$table[[which(str_detect(names(plot_table_current$table), input$RM_progress_plot_view))[1]]]
})

output$RM_progress_inputs_list <- renderUI({
  tableOutput("RM_progress_inputs_table")
})

output$RM_progress_inputs_table <- renderTable({
  df <- data.frame(
    check.names = FALSE,
    `Input` = c(
      "Model Training"
    ),
    `Value` = c(
      input$rm_prompts_train
    )
  )
  
  df <- df %>% add_row(
    `Input` = "Data Subsetting Method",
    `Value` = ifelse(input$cv_perform_option == "loocv", "Leave-one-out", "K-fold")
  )
  
  if (input$cv_perform_option == "kfcv") {
    df <- df %>% add_row(
      `Input` = "Number of Folds",
      `Value` = input$nFolds_cv %>% as.character()
    )
  }
  
  df
})
