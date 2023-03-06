## Groups UI rendering

output$f_data_upload_UI <- renderUI({
  
  req(!is.null(input$use_fdata) && input$use_fdata == "TRUE" && !input$use_example_fdata && input$fdata_options_done > 0 &&
        input$how_make_fdata == "upload")
  
  div(
    collapseBox(
      "Upload Sample Data",
      icon_id = "fdata_params_icon",
      icon = icon("exclamation-sign", lib = "glyphicon"),
      value = "upload_fdata_UI_box",
      collapsed = F,
      fileinput_UI("f_data_upload", "f_data", 
                   is_RNA = (input$data_type == "RNA-seq")),
      actionButton("fdata_upload_done", "Done")
    )
  )
})

output$f_data_generate_UI <- renderUI({
  generate_fdata_UI("f_data")
})

output$f_meta_spec_UI <- renderUI({
  
  req(!is.null(input$use_fdata) && input$use_fdata == "TRUE" && !input$use_example_fdata &&
        !is.null(reactive_dataholder[["f_data"]]$file) &&
        (input$fdata_upload_done > 0 || 
           input$fdata_options_done > 0 && input$use_example_fdata)
      )
  
  collapseBox(
    "Specify Sample Data Properties",
    icon_id = "fdata_params_icon",
    icon = icon("exclamation-sign", lib = "glyphicon"),
    value = "data_props_fdata",
    collapsed = F,
    
    pickerInput(
      "f_data_id_col",
      "Which column identifies samples?",
      choices = colnames(reactive_dataholder[["f_data"]]$file),
      selected = isolate(if(!is.null(input$e_meta_id_col)) input$e_meta_id_col else character(0))
    ),
    
    actionButton("specify_edata_done", "Done")
  )
})