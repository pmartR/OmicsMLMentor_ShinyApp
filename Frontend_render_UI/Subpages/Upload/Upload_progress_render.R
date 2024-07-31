output$Upload_progress_summary <- renderUI({
  DTOutput("Upload_progress_summary_table")
})

output$Upload_progress_summary_table <- renderDT({
  req(!is.null(omicsData$obj))
  summary(omicsData$obj)
})

output$Upload_progress_next_steps <- renderUI({
  tagList(
    tags$b("Quality Control"),
    tags$ul(
      tags$li("Reference Normalization"),
      tags$li("Single Observation Biomolecule Removal"),
      tags$li("Outlier Detection"),
      tags$li("Data Missingness Handling")
    )
  )
})

output$Upload_progress_plot <- renderUI({
  req(!is.null(reactive_dataholder$e_data))
  
  if (input$Upload_progress_plot_view == "Upload__boxplot") {
    if (isTruthy(input$boxplot_UI_load_button) || dim(reactive_dataholder$e_data$file)[1] < 50000) {
      plotlyOutput("Upload_progress_plot_render")
    } else {
      div(
        "This plot is large and may take a while to render.",
        actionButton("boxplot_UI_load_button", "Show plot")
      )
    }
  } else {
    plotlyOutput("Upload_progress_plot_render")
  }
})

output$Upload_progress_plot_render <- renderPlotly({
  
  use_plot <- which(
    str_detect(
      names(plot_table_current$table), 
      input$Upload_progress_plot_view
    )
  )[1]
  
  validate(
    need(length(use_plot) > 0, 
         'Plot not generated in current pipeline. Return and view on the appropriate page to see this plot.')
  )
  
  plot_table_current$table[[use_plot]]
})

output$Upload_progress_inputs_list <- renderUI({
  tableOutput("Upload_progress_inputs_table")
})

output$Upload_progress_inputs_table <- renderTable({
  df <- data.frame(
    check.names = FALSE,
    `Input` = c(
      "Data Type",
      paste(ifelse(input$data_type == "RNA-seq", "Expression Data", "Abundance Data"), "Source"),
      "Biomolecule Information Source",
      paste(ifelse(input$data_type == "RNA-seq", "Expression Data", "Abundance Data"), "Identifier Column"),
      paste(ifelse(input$data_type == "RNA-seq", "Expression Data", "Abundance Data"), "Scale"),
      paste(ifelse(input$data_type == "RNA-seq", "Expression Data", "Abundance Data"), "Normalized"),
      paste(ifelse(input$data_type == "RNA-seq", "Expression Data", "Abundance Data"), "Missing Data Indicator"),
      "Sample Information Source"
    ),
    `Value` = c(
      input$data_type,
      ifelse(input$use_example, "Example Data", "Uploaded Data"),
      ifelse(input$have_emeta, ifelse(input$use_example, "Example Data", "Uploaded Data"), "None"),
      input$e_data_id_col,
      input$datascale,
      input$normalized,
      input$na_symbol,
      ifelse(
        input$use_fdata == "f_data",
        ifelse(
          input$use_example_fdata,
          "Example Data",
          ifelse(
            input$how_make_fdata == "upload",
            "Uploaded Data",
            "Experimental Data Column Names"
          )
        ),
        "None"
      )
    )
  )
  
  if (input$have_emeta) {
    df <- rbind(df, data.frame(
      check.names = FALSE,
      `Input` = c(
        "Biomolecule Information Identifier Column"
      ),
      `Value` = c(
        input$e_meta_id_col
      )
    ))
  }
  
  if (input$use_fdata == "f_data") {
    df <- rbind(df, data.frame(
      check.names = FALSE,
      `Input` = c(
        "Sample Information Identifier Column"
      ),
      `Value` = c(
        input$f_data_id_col
      )
    ))
  }
  
  df
})