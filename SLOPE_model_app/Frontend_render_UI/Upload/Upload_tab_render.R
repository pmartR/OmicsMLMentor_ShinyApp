output$e_data_spec_UI <- renderUI({
  
  req(!is.null(reactive_dataholder$e_data))
  
  if(reactive_dataholder$model$datatype == "RNA-seq"){
    choices <- list(
      "Counts" = "counts",
      "Log2 counts per million" = "lcpm",
      "Upper-quantile transformed counts" = "upper",
      "Median counts" = "median"
    )
  } else {
    choices <- list(
      "Raw intensity" = "abundance",
      "Log base 2" = "log2",
      "Log base 10" = "log10",
      "Natural log" = "log"
    )
  }
  
  title <- ifelse(reactive_dataholder$model$datatype == "RNA-seq", "Specify Expression Data Properties",
                  "Specify Abundance Data Properties")
  
  edat <- reactive_dataholder$e_data
  selector <- apply(is.na(apply(edat, 2, as.numeric)), 2, all)
  
  if(any(selector)){
    disabled <- !selector
    selected <- colnames(edat)[min(which(selector))]
  } else {
    disabled <- NULL
    selected <- isolate(if(!is.null(input$e_data_id_col)) 
      input$e_data_id_col else character(0))
  }
  
  div(
    collapseBox(
      title,
      value = "params_box",
      icon_id = "edata_params_icon",
      icon = icon("exclamation-sign", lib = "glyphicon"),
      collapsed = F,
      
      pickerInput(
        "e_data_id_col",
        "Which column identifies unique biomolecules?",
        choices = colnames(edat),
        selected = selected,
        choicesOpt = list(disabled = disabled)
      ),
      
      pickerInput("datascale",
                  "On what scale is this data on?",
                  choices = choices,
                  multiple = TRUE,
                  options = pickerOptions(maxOptions = 1),
                  selected = isolate(if(!is.null(input$datascale)) input$datascale else character(0))
      ),
      uiOutput("RNA_text_warn"),
      
      radioGroupButtons( ## Hide for isobaric non-instrument normalized
        "normalized",
        "Has this data been statistically normalized?",
        choices = c("Yes", "No"),
        selected = isolate(if(!is.null(input$normalized)) input$normalized else character(0))
      ),
      
      textInput("na_symbol",
                "What value denotes missing data?",
                value = isolate(if(!is.null(input$na_symbol)) input$na_symbol else "NA")
      ),
      
      # div(style="display:inline-block",actionButton("specify_edata_done", "Done", style="float:right"))
      
      fluidRow(
        column(10, ""),
        column(2, disabled(actionButton("specify_edata_done", "Done", style="float:right")))
      )
    )
  )
})



output$how_make_fdata_UI <- renderUI({
  
  req(input$use_fdata && !AWS)
  
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
  
  req(!is.null(input$use_fdata) &&
        !is.null(reactive_dataholder[["f_data"]]$file) &&
        
        ## Upload, example, or generated fdata
        ((!is.null(input$fdata_upload_done) && input$fdata_upload_done > 0) || 
           input$fdata_options_done > 0 && 
           (AWS || input$how_make_fdata == "colnames"))
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