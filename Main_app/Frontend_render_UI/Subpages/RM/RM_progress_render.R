output$RM_progress_summary <- renderUI({
  p1 <- DTOutput("RM_progress_summary_table")
  p2 <- if(!is.null(omicsData$objRM_reduced)){
    radioGroupButtons("reduced_full_summary", 
                         choices = c("Full model", "Reduced model")
                         )
  } else NULL
    
  div(p1, br(), br(), p2)
})

output$RM_progress_summary_table <- renderDT({
  if (!supervised()) {
    return(data.frame(Value = c("No data summary available for unsupervised models.", "Please see the plots available to the right.")))
  }
  if(!is.null(input$reduced_full_summary) && input$reduced_full_summary == "Reduced model"){
    attr(omicsData$objRM_reduced, "response_performance")
  } else {
    attr(omicsData$objRM, "response_performance")

  }

})

output$RM_progress_next_steps <- renderUI({
  
  df <- data.frame(`Hyperparameter` = character(0), `Value` = character(0))
  if (length(hp_inputs$input_names) > 0) {
    for (param in 1:length(hp_inputs$input_names)) {
      df <- df %>% add_row(
        `Hyperparameter` = hp_inputs$input_labels[[param]],
        `Value` = paste(
          input[[hp_inputs$input_names[[param]]]],
          if (isTruthy(input[[paste0("optimize_", hp_inputs$input_names[[param]])]]))
            "(Optimized)"
          else
            ""
        )
      )
    }
  }
  
  user_inputs$rm <- list(
    model_scope = 
      if (!is.null(input$rm_prompts_train)) {
        if(input$rm_prompts_train == "notrain") {
          "Current Data"
        } else {
          "Current and New Data"
        }
      } else {
        NULL
      },
    hyperparam_source = str_to_title(input$rm_prompts_hp),
    subset_method = 
      if (!is.null(input$rm_prompts_train)) {
        if (input$rm_prompts_hp == "tuned") {
          ifelse(input$cv_hp_option == "loocv", "Leave-one-out", "K-fold")
        } else {
          ifelse(input$cv_perform_option == "loocv", "Leave-one-out", "K-fold")
        }
      } else {
        NULL
      },
    nfolds = 
      if (!is.null(input$rm_prompts_train)) {
        if (input$rm_prompts_hp == "tuned") {
          input$nFolds_hp %>% as.character()
        } else {
          input$nFolds_cv %>% as.character()
        }
      } else {
        NULL
      },
    hyperparams = df,
    vi_choose = 
      if (isTruthy(input$feature_select_posthoc)) {
        input$vi_choose
      },
    vi_value =
      if (isTruthy(input$feature_select_posthoc)) {
        switch(input$vi_choose,
               value = input$vi_thresh,
               count = input$vi_thresh_count,
               percent = input$vi_thresh_pct)
      }
  )
  
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
  
  use_plot <- which(
    str_detect(
      names(plot_table_current$table), 
      input$RM_progress_plot_view
    )
  )[1]
  
  validate(
    need(length(plot_table_current$table[[use_plot]]) > 0, 
         'Plot not generated in current pipeline. Return and view on the appropriate page to see this plot.')
  )
  
  plot_table_current$table[[use_plot]]
})

output$RM_progress_inputs_list <- renderUI({
  tableOutput("RM_progress_inputs_table")
})

output$RM_progress_inputs_table <- renderTable({
  
  df <- data.frame(
    check.names = FALSE,
    `Input` = c(
      if (!is.null(input$rm_prompts_train)) "Model Scope" else NULL,
      "Hyperparameters Used"
    ),
    `Value` = c(
      if (!is.null(input$rm_prompts_train)) {
        if(input$rm_prompts_train == "notrain") {
          "Current Data"
        } else {
          "Current and New Data"
        }
      } else {
        NULL
      },
      str_to_title(input$rm_prompts_hp)
    )
  )
  
  if (!is.null(input$rm_prompts_train)) {
    if (input$rm_prompts_hp == "tuned") {
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
    } else {
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
    }
  }
  
  if (isTruthy(input$feature_select_posthoc)) {
    df <- df %>% add_row(
      `Input` = "Reduced Model Feature Cutoff",
      `Value` = paste0(
        ifelse(input$vi_choose == "value", "", "Top "),
        switch(input$vi_choose,
               value = input$vi_thresh,
               count = input$vi_thresh_count,
               percent = input$vi_thresh_percent),
        " ",
        switch(input$vi_choose,
               value = "",
               count = " features",
               percent = " percent of features")
      )
    )
  }
  
  if (length(hp_inputs$input_names) > 0) {
    for (param in 1:length(hp_inputs$input_names)) {
      df <- df %>% add_row(
        `Input` = paste("Hyperparameter:", hp_inputs$input_labels[[param]]),
        `Value` = paste(
          input[[hp_inputs$input_names[[param]]]],
          if (isTruthy(input[[paste0("optimize_", hp_inputs$input_names[[param]])]]))
            "(Optimized)"
          else
            ""
        )
      )
    }
  }
  
  df
})
