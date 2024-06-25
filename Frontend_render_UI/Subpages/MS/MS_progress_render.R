output$MSetup_progress_summary <- renderUI({
  DTOutput("MSetup_progress_summary_table")
})

output$MSetup_progress_summary_table <- renderDT({
  summary(omicsData$objMSU)
})

output$MSetup_progress_next_steps <- renderUI({
  tags$ul(
    tags$li("Scaling and Transformation"),
    tags$li("Data Filtering"),
    tags$li("Data Normalization")
  )
})

output$MSetup_progress_plot <- renderUI({
  tbl(as.data.frame(dashboard()), "Method", NULL)
})

output$MSetup_progress_inputs_list <- renderUI({
  tableOutput("MSetup_progress_inputs_table")
})

output$MSetup_progress_inputs_table <- renderTable({
  df <- data.frame(
    check.names = FALSE,
    `Input` = c(
      "Tracked Sample Information Columns",
      "Categorical Sample Information Columns",
      "Model Type",
      "Model Priority",
      "Selected Model"
    ),
    `Value` = c(
      paste(input$f_data_track, collapse = ", "),
      paste(input$f_data_cats, collapse = ", "),
      str_to_title(input$ag_prompts),
      ifelse(
        is.null(input$ag_prompts_supervised),
        str_to_title(input$ag_prompts_unsupervised),
        str_to_title(input$ag_prompts_supervised)
      ),
      input$pick_model_EM
    )
  )
  
  df
})
