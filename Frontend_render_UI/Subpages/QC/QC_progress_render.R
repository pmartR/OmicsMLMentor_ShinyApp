output$QC_progress_summary <- renderUI({
  DTOutput("QC_progress_summary_table")
})

output$QC_progress_summary_table <- renderDT({
  summary(omicsData$objQC)
})

output$QC_progress_next_steps <- renderUI({
  user_inputs$qc <- list(
    ref_group = input$Isobaricpepdata_ref_group,
    ref_col = input$Isobaricpepdata_ref_col,
    ref_notation = input$Isobaricpepdata_ref_notation,
    single_obs_removed = ifelse(is.null(input$QC_add_molfilt) || input$QC_add_molfilt, "Yes", "No"),
    outliers_removed = ifelse(is.null(input$QC_rmdfilt_sample_remove), "None", paste(input$QC_rmdfilt_sample_remove, collapse = ", ")),
    min_pct_sample = input$missing_value_thresh,
    missingdata_kept = input$keep_missing,
    handle_impute = ifelse(is.null(missingHandleSliderVals()$md_impute), "None", paste(missingHandleSliderVals()$md_impute, collapse = "-")),
    handle_convert = ifelse(is.null(missingHandleSliderVals()$md_convert), "None", paste(missingHandleSliderVals()$md_convert, collapse = "-")),
    handle_remove = ifelse(is.null(missingHandleSliderVals()$md_remove), "None", paste(missingHandleSliderVals()$md_remove, collapse = "-"))
  )
  
  tagList(
    tags$b("Model Set-Up"),
    tags$ul(
      tags$li("Variable Specification"),
      tags$li("Analysis Goal Selection"),
      tags$li("Expert Mentor")
    )
  )
})

output$QC_progress_plot <- renderUI({
  plotlyOutput("QC_progress_plot_render")
})

output$QC_progress_plot_render <- renderPlotly({
  
  use_plot <- which(
    str_detect(
      names(plot_table_current$table), 
      input$QC_progress_plot_view
    )
  )[1]
  
  validate(
    need(length(plot_table_current$table[[use_plot]]) > 0, 
         'Plot not generated in current pipeline. Return and view on the appropriate page to see this plot.')
  )
  
  plot_table_current$table[[use_plot]]
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
