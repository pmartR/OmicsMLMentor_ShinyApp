output$preprocessing_progress_summary <- renderUI({
  DTOutput("preprocessing_progress_summary_table")
})

output$preprocessing_progress_summary_table <- renderDT({
  summary(omicsData$objPP)
})

output$preprocessing_progress_next_steps <- renderUI({
  tagList(
    tags$b("Run Model"),
    tags$ul(
      tags$li("Model Options"),
      tags$li("Training Structure"),
      tags$li("Parameter Optimization"),
      tags$li("Model Evaluation")
    )
  )
})

output$preprocessing_progress_plot <- renderUI({
  if (input$preprocessing_progress_plot_view == "Transformed Boxplots") {
    if (isTruthy(input$transform_preview_plot_load_button) || dim(omicsData$objPP$e_data)[1] < 50000) {
      plotlyOutput("transform_preview_plot")
    } else {
      div(
        "This plot is large and may take a while to render.",
        actionButton("transform_preview_plot_load_button", "Show plot")
      )
    }
  } else {
    plotlyOutput("preprocessing_progress_plot_render")
  }
})

output$preprocessing_progress_plot_render <- renderPlotly({
  plot_table_current$table[[which(str_detect(names(plot_table_current$table), input$preprocessing_progress_plot_view))[1]]]
})

output$preprocessing_progress_inputs_list <- renderUI({
  tableOutput("preprocessing_progress_inputs_table")
})

output$preprocessing_progress_inputs_table <- renderTable({
  df <- data.frame(
    check.names = FALSE,
    `Input` = c(
      "Transform Scale"
    ),
    `Value` = c(
      input$transform
    )
  )
  
  df <- df %>% add_row(
    `Input` = "Molecule Filter Applied",
    `Value` = ifelse(input[[paste0(get_omicsData_type(omicsData$objPP), "_add_molfilt")]], "Yes", "No")
  )
  
  if (input[[paste0(get_omicsData_type(omicsData$objPP), "_add_molfilt")]]) {
    df <- df %>% add_row(
      `Input` = "Molecule Filter Minimum Number Observed",
      `Value` = input[[paste0(get_omicsData_type(omicsData$objPP), "_mol_min_num")]] %>% as.character()
    )
  }
  
  df <- df %>% add_row(
    `Input` = "Biomolecule Detection Filter Applied",
    `Value` = ifelse(input[[paste0(get_omicsData_type(omicsData$objPP), "_add_imputefilt")]], "Yes", "No")
  )
  
  df <- df %>% add_row(
    `Input` = "Coefficient of Variation Filter Applied",
    `Value` = ifelse(input[[paste0(get_omicsData_type(omicsData$objPP), "_add_cvfilt")]], "Yes", "No")
  )
  
  if (input[[paste0(get_omicsData_type(omicsData$objPP), "_add_cvfilt")]]) {
    df <- df %>% add_row(
      `Input` = "Coefficient of Variation Maximum CV",
      `Value` = input[[paste0(get_omicsData_type(omicsData$objPP), "_cv_threshold")]] %>% as.character()
    )
    df <- df %>% add_row(
      `Input` = "CV Filter Uses Groups",
      `Value` = ifelse(input[[paste0(get_omicsData_type(omicsData$objPP), "_cvfilt_use_groups")]], "Yes", "No")
    )
  }
  
  df <- df %>% add_row(
    `Input` = "Custom Biomolecule Filter Applied",
    `Value` = ifelse(input[[paste0(get_omicsData_type(omicsData$objPP), "_add_edata_customfilt")]], "Yes", "No")
  )
  
  if (input[[paste0(get_omicsData_type(omicsData$objPP), "_add_edata_customfilt")]]) {
    df <- df %>% add_row(
      `Input` = "Custom Biomolecule Filter Selected Biomolecule Handling",
      `Value` = input[[paste0(get_omicsData_type(omicsData$objPP), "_edata_remove_or_keep")]]
    )
    df <- df %>% add_row(
      `Input` = "Custom Biomolecule Filter Selected Biomolecules",
      `Value` = input[[paste0(get_omicsData_type(omicsData$objPP), "_edata_customfilt_regex")]]
    )
  }
  
  df <- df %>% add_row(
    `Input` = "Normalization Method",
    `Value` = input[[paste0(get_omicsData_type(omicsData$objPP), "_normalize_option")]]
  )
  
  if (input[[paste0(get_omicsData_type(omicsData$objPP), "_normalize_option")]] == "Global Normalization") {
    df <- df %>% add_row(
      `Input` = "Normalization Function",
      `Value` = input[[paste0(get_omicsData_type(omicsData$objPP), "_norm_fn")]]
    )
    
    subset_names <- c(
      "No Subsetting" = "all",
      "Top L order statistics (los)" = "los",
      "Percentage present (ppp)" = "ppp",
      "Complete" = "complete",
      "Rank invariant (rip)" = "rip",
      "Percentage present and rank invariant (ppp+rip)" = "ppp_rip"
    )
    
    subset_name <- names(subset_names)[
      which(subset_names == input[[paste0(get_omicsData_type(omicsData$objPP), "_subset_fn")]])
    ]
    
    df <- df %>% add_row(
      `Input` = "Subsetting Function",
      `Value` = subset_name
    )
    
    if (input[[paste0(get_omicsData_type(omicsData$objPP), "_subset_fn")]] == "los") {
      df <- df %>% add_row(
        `Input` = "Proportion of Top Order Statistics",
        `Value` = input[[paste0(get_omicsData_type(omicsData$objPP), "_los")]] %>% as.character()
      )
    }
    
    if (input[[paste0(get_omicsData_type(omicsData$objPP), "_subset_fn")]] == "ppp" ||
        input[[paste0(get_omicsData_type(omicsData$objPP), "_subset_fn")]] == "ppp_rip") {
      df <- df %>% add_row(
        `Input` = "Proportion of Percentage Present",
        `Value` = input[[paste0(get_omicsData_type(omicsData$objPP), "_ppp")]] %>% as.character()
      )
    }
    
    if (input[[paste0(get_omicsData_type(omicsData$objPP), "_subset_fn")]] == "rip" ||
        input[[paste0(get_omicsData_type(omicsData$objPP), "_subset_fn")]] == "ppp_rip") {
      df <- df %>% add_row(
        `Input` = "Rank Invariance P Value",
        `Value` = input[[paste0(get_omicsData_type(omicsData$objPP), "_rip")]] %>% as.character()
      )
    }
    
    df <- df %>% add_row(
      `Input` = "Backtransformation Applied",
      `Value` = ifelse(input[[paste0(get_omicsData_type(omicsData$objPP), "_backtransform")]], "Yes", "No")
    )
  }
  
  df
})
