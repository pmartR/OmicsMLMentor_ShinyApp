## Groups UI rendering

output$f_data_upload_UI <- renderUI({
  
  req(!is.null(input$use_fdata) && input$use_fdata == "f_data" && 
        !input$use_example_fdata && input$fdata_options_done > 0 &&
        input$how_make_fdata == "upload" && !AWS && !MAP_ACTIVE)
  req(!is.null(input$data_type))
  
  div(
    collapseBox(
      "Upload Sample Information",
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

output$how_make_fdata_UI <- renderUI({
  
  req(input$use_fdata == 'f_data' && !input$use_example_fdata && !AWS)
  validate(need(!MAP_ACTIVE, "Data has been uploaded from MAP"))
  
  div(
    br(),
    radioGroupButtons(
      inputId = "how_make_fdata", label = "Create Sample Information from:",
      choices = c("Uploaded file" = "upload", 
                  "Experimental data column names" = "colnames"),
      selected = character(0)
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
           (input$use_example_fdata || AWS || MAP_ACTIVE || input$how_make_fdata == "colnames"))
      )
  
  check_cols <- colnames(reactive_dataholder[["e_data"]]$file)
  f_data <- reactive_dataholder[["f_data"]]$file
  best_col <- which.max(map_int(colnames(f_data), function(col){
    sum(as.character(f_data[[col]]) %in% check_cols)
  }))
  
  selected <- colnames(f_data)[best_col]
  
  collapseBox(
    "Specify Sample Information Properties",
    icon_id = "fdata_params_icon",
    icon = icon("exclamation-sign", lib = "glyphicon"),
    value = "data_props_fdata",
    collapsed = F,
    
    pickerInput(
      "f_data_id_col",
      "Which column identifies unique samples?",
      choices = colnames(reactive_dataholder[["f_data"]]$file),
      selected = isolate(if(!is.null(input$f_data_id_col)) input$f_data_id_col else selected)
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

output$Group_tab_plots <- renderPlotly({
  
  req(!is.null(reactive_dataholder$f_data$file) && !is.null(input$Gplot_picker))
  
  df <- reactive_dataholder$f_data$file
  df <- df[colnames(df) != input$f_data_id_col]
  df <- gather(df)
  df <- df[!is.na(df$value),]
  
  df <- df[df$key %in% input$Gplot_picker,]
  
  if(all(is.na(as.numeric(as.character(df$value))))){
    p <- ggplot(df, aes(x = value, fill = value)) +
      geom_bar(color = "black", show.legend = F) + theme_bw() +
      ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      labs(x = "Level", y = "Samples per level", title = input$Gplot_picker)
  } else {
    
    df$value <-  as.numeric(as.character(df$value))
    p <- ggplot(df, aes(x = value, fill = key)) + geom_histogram(show.legend = F) + theme_bw() +
      ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      labs(x = "Value", y = "Frequency")
  }
  
  isolate(plot_table_current$table[[paste0("Upload__grouping__", input$Gplot_picker)]] <- p)
  isolate(plot_table_current$names[[paste0("Upload__grouping__", input$Gplot_picker)]] <- paste0("Group info: ", input$Gplot_picker))
  
  p
  
})

## Accordion behavior

output$detected_box_group <- renderUI({
  
  req(input$specify_fdata_done > 0)
  collapseBox("Data Properties",
              value = "fdata_plots",
              uiOutput("Group_plot_picker"),
              plotlyOutput("Group_tab_plots")
              # uiOutput("group_tab_boxplots")
  )
})

observeEvent(input$fdata_upload_done, {
  updateBoxCollapse(session, "groups_collapse_left", close = "upload_fdata_UI_box")
})

observeEvent(c(input$use_fdata, input$use_example_fdata, input$how_make_fdata), {
  
  if (!is.null(input$use_fdata) &&

      (input$use_fdata == "No"  || AWS || MAP_ACTIVE || ## MAP
       isTruthy(input$use_example_fdata) || 
       !is.null(input$how_make_fdata))) {

    shinyjs::enable("fdata_options_done")
    
  # } else if (!is.null(input$use_fdata) && MAP_ACTIVE) {
  #   shinyjs::enable("fdata_options_done")
    
  } else {
    shinyjs::disable("fdata_options_done")
  }
}, ignoreNULL = FALSE)

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

## Warn for 
observeEvent(input$use_fdata, {
  req(!is.null(input$use_fdata) && input$use_fdata == "")
  shinyalert("Warning: predictive models will not be available",
             paste0("Models that can predict experimental groups",
                   " or sample conditions based on biomolecule data",
                   " are not available without a Sample Information file.",
                   " Please make sure this is appropriate for your goals ",
                   "before clicking the 'Confirm selections' button."),
             type = "warning"
  )
  
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



