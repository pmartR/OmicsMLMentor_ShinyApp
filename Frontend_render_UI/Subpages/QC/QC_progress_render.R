output$QC_progress_summary <- renderUI({
  DTOutput("QC_progress_summary_table")
})

output$QC_progress_summary_table <- renderDT({
  summary(omicsData$objQC)
})

output$QC_progress_next_steps <- renderUI({
  tags$ul(
    tags$li("Variable Specification"),
    tags$li("Analysis Goal Selection"),
    tags$li("Expert Mentor")
  )
})

output$QC_progress_plot <- renderUI({
  plotlyOutput("QC_progress_plot_render")
})

output$QC_progress_plot_render <- renderPlotly({
  plot_table_current$table[[which(str_detect(names(plot_table_current$table), input$QC_progress_plot_view))[1]]]
})

output$QC_progress_inputs_list <- renderUI({
  tableOutput("QC_progress_inputs_table")
})

output$QC_progress_inputs_table <- renderTable({
  df <- data.frame(
    check.names = FALSE,
    `Input` = c(
      "Single Observation Biomolecules Removed",
      "Outlier Samples Removed",
      "Minimum % Biomolecules Detected",
      "Missing Data Kept"
    ),
    `Value` = c(
      ifelse(is.null(input$QC_add_molfilt) || input$QC_add_molfilt, "Yes", "No"),
      ifelse(is.null(input$QC_rmdfilt_sample_remove), "None", paste(input$QC_rmdfilt_sample_remove, collapse = ", ")),
      input$missing_value_thresh,
      input$keep_missing
    )
  )
  
  if (input$keep_missing == "No") {
    df <- rbind(df, data.frame(
      check.names = FALSE,
      `Input` = c(
        "Missingness Range Estimated",
        "Missingness Range Converted",
        "Missingness Range Removed"
      ),
      `Value` = c(
        ifelse(is.null(missingHandleSliderVals()$md_impute), "None", paste(missingHandleSliderVals()$md_impute, collapse = "-")),
        ifelse(is.null(missingHandleSliderVals()$md_convert), "None", paste(missingHandleSliderVals()$md_convert, collapse = "-")),
        ifelse(is.null(missingHandleSliderVals()$md_remove), "None", paste(missingHandleSliderVals()$md_remove, collapse = "-"))
      )
    ))
  }
  
  df
})
