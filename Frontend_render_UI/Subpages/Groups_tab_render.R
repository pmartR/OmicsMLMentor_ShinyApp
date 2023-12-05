## Groups UI rendering

output$f_data_upload_UI <- renderUI({
  
  req(!is.null(input$use_fdata) && input$use_fdata == "f_data" && 
        !input$use_example_fdata && input$fdata_options_done > 0 &&
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
      # div(style="display:inline-block",actionButton("fdata_upload_done", "Done", style="float:right"))
      
      
      fluidRow(
        column(10, ""),
        column(2, actionButton("fdata_upload_done", "Done", style="float:right"))
      )
    )
  )
})

output$f_data_generate_UI <- renderUI({
  req(input[["use_fdata"]] == "f_data" && 
        !is.null(reactive_dataholder[["f_data"]]$file) &&
        input$fdata_options_done > 0)
  generate_fdata_UI("f_data")
})

output$f_meta_spec_UI <- renderUI({
  
  req(!is.null(input$use_fdata) && input$use_fdata == "f_data" &&
        !is.null(reactive_dataholder[["f_data"]]$file) &&
        
        ## Upload, example, or generated fdata
        ((!is.null(input$fdata_upload_done) && input$fdata_upload_done > 0) || 
           input$fdata_options_done > 0 && 
           (input$use_example_fdata || input$how_make_fdata == "colnames"))
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
    # div(style="display:inline-block",actionButton("specify_fdata_done", "Done", style="float:right"))
    
    fluidRow(
      column(10, ""),
      column(2, actionButton("specify_fdata_done", "Done", style="float:right"))
    )
  )
})

output$Group_plot_picker <- renderUI({
  
  req(!is.null(reactive_dataholder$f_data$file))
  df <- reactive_dataholder$f_data$file
  
  pickerInput(
    "Gplot_picker",
    multiple = F,
    choices = colnames(df)[colnames(df) != input$f_data_id_col],
    options = list(`live-search` = T)
  )
})

output$Group_tab_plots <- renderPlot({
  
  req(!is.null(reactive_dataholder$f_data$file) && !is.null(input$Gplot_picker))
  
  df <- reactive_dataholder$f_data$file
  df <- df[colnames(df) != input$f_data_id_col]
  df <- gather(df)
  df <- df[!is.na(df$value),]
  
  df <- df[df$key %in% input$Gplot_picker,]
  
  if(all(is.na(as.numeric(as.character(df$value))))){
    return(
      ggplot(df, aes(x = value, fill = value)) + 
        geom_bar(color = "black", show.legend = F) + theme_bw() + 
        ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        labs(x = "Level", y = "Samples per level", title = input$Gplot_picker)
    )
  } else {
    
    df$value <-  as.numeric(as.character(df$value))
    return(
      ggplot(df, aes(x = value, fill = key)) + geom_histogram(show.legend = F) + theme_bw() + 
      ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      labs(x = "Value", y = "Frequency")
    )
  }
  
})

## Accordion behavior

output$detected_box_group <- renderUI({
  
  req(input$specify_fdata_done > 0)
  collapseBox("Detected Data Properties",
              value = "fdata_plots",
              uiOutput("Group_plot_picker"),
              plotOutput("Group_tab_plots")
              # uiOutput("group_tab_boxplots")
  )
})

observeEvent(input$fdata_upload_done, {
  updateBoxCollapse(session, "groups_collapse_left", close = "upload_fdata_UI_box")
})

observeEvent(input$fdata_options_done, {
  updateBoxCollapse(session, "groups_collapse_left", close = "fdata_options")
})

observeEvent(input[["fdata-add_cols_done"]], {
  updateBoxCollapse(session, "groups_collapse_left", close = "fdata_create_columns")
})

observeEvent(input$specify_fdata_done, {
  updateBoxCollapse(session, "groups_collapse_left", close = "data_props_fdata")
})

observeEvent(input$check_group_cols, {
  
  updateBoxCollapse(session, "groups_collapse_left", 
                    close = c("data_props_fdata", 
                              "fdata_create_columns", 
                              "fdata_options"))
  
  updateBoxCollapse(session, "groups_collapse_right", 
                    close = "data_preview_fdata", open = "fdata_plots")
})


observeEvent(reactive_dataholder$f_data$file, {
  if(is.null(reactive_dataholder$f_data$file)){
    updateBoxCollapse(session, "groups_collapse_right", close = "data_preview_fdata")
  } else {
    updateBoxCollapse(session, "groups_collapse_right", open = "data_preview_fdata")
  }
})


## completion button show
observe({
  cond <- (
    (input$use_fdata != 'f_data' && input$fdata_options_done > 0) ||
      (input$use_fdata == 'f_data' && !is.null(input$specify_fdata_done) &&
         input$specify_fdata_done > 0 && input$fdata_options_done > 0
         )
    )
  
  
  toggleElement("check_group_cols", condition = cond)
})



