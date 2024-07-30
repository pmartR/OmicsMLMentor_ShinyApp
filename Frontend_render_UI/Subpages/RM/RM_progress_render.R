output$RM_progress_summary <- renderUI({
  DTOutput("RM_progress_summary_table")
})

output$RM_progress_summary_table <- renderDT({
  if (!supervised()) {
    return(data.frame(Value = c("No data summary available for unsupervised models.", "Please see the plots available to the right.")))
  }
  
  pred_df <- attr(omicsData$objRM, "prediction_train")
  
  if (!is.null(omicsData$objRM_reduced)) {
    pred_df <- attr(omicsData$objRM_reduced, "prediction_train")
  }
  
  if (length(unique(pred_df$response)) == 2) {
    pos_class <- names(pred_df)[3]
  } else {
    pos_class <- names(pred_df)[3:(2+length(unique(pred_df$response)))]
  }
  
  p <- yardstick::roc_curve(pred_df, response, dplyr::all_of(pos_class))      
  
  auc_by_level = p %>%
    dplyr::group_by(.level) %>%
    dplyr::mutate(spc_diff = specificity - dplyr::lag(specificity), sens_avg = (sensitivity + dplyr::lag(sensitivity))/2) %>%
    dplyr::summarise(sum(spc_diff*sens_avg, na.rm=T))
  
  names(auc_by_level)[1] <- "Group"
  names(auc_by_level)[2] <- "AUC of ROC"
  
  auc_by_level
})

output$RM_progress_next_steps <- renderUI({
  tagList(
    tags$b("Download"),
    tags$ul(
      tags$li("Plot Download Selections"),
      tags$li("Table Download Selections"),
      tags$li("Report Creation"),
      tags$li("Saving Your Model"),
    )
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
      "Model Scope",
      "Hyperparameters Used"
    ),
    `Value` = c(
      if(input$rm_prompts_train == "notrain") {
        "Current Data"
      } else {
        "Current and New Data"
      },
      str_to_title(input$rm_prompts_hp)
    )
  )
  
  if (isTruthy(input$cv_perform_option)) {
    df <- df %>% add_row(
      `Input` = "Data Subsetting Method",
      `Value` = ifelse(input$cv_perform_option == "loocv", "Leave-one-out", "K-fold")
    )
  }
  
  if (isTruthy(input$cv_perform_option) && input$cv_perform_option == "kfcv") {
    df <- df %>% add_row(
      `Input` = "Number of Folds",
      `Value` = input$nFolds_cv %>% as.character()
    )
  }
  
  if (isTruthy(input$cv_hp_option)) {
    df <- df %>% add_row(
      `Input` = "Data Subsetting Method",
      `Value` = ifelse(input$cv_hp_option == "loocv", "Leave-one-out", "K-fold")
    )
  }
  
  if (isTruthy(input$cv_hp_option) && input$cv_hp_option == "kfcv") {
    df <- df %>% add_row(
      `Input` = "Number of Folds",
      `Value` = input$nFolds_hp %>% as.character()
    )
  }
  
  df
})
