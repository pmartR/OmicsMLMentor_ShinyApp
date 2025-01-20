

#### File loading boxes

output$model_load_UI <- renderUI({
  collapseBox(
    collapsed = F,
    value = "model_upload",
    "Upload Model",
    
    uiOutput("model_upload_UI"),
    
    fluidRow(
      column(10, ""),
      column(2, disabled(actionButton("upload_model_done", "Done", style="float:right")))
    )
  )
})

output$e_data_load_UI <- renderUI({
  
  req(!is.null(reactive_dataholder$model) && 
        (AWS || input$upload_model_done > 0 || input$load_example > 0)
      )
  
  dt <- class(reactive_dataholder$model$norm_omics)
  # if we slData as well as other datasets, we run into issues
  if("slData" %in% dt){
    dt <- dt[dt != "slData"]
  }
  
  button <-actionButton("upload_edata_done", "Done", style="float:right")
  
  if(is.null(reactive_dataholder$e_data)){
    button <- disabled(button)
  }
  
  collapseBox(
    value = "data_upload_edata",
    collapsed = input$load_example > 0 || AWS,
    paste0("Upload ",
           ifelse(dt == "proData",
                  "Proteomic", dt),
           ifelse(dt == "seqData",
                  " Expression", " Abundance"),
           " Data"),
    
    uiOutput("e_data_upload_UI"),
    
    fluidRow(
      column(10, ""),
      column(2, button)
    )
    
  )
})

output$f_data_load_UI <- renderUI({
  
  req(!is.null(reactive_dataholder$e_data) && 
        (AWS || input$specify_edata_done > 0 || input$load_example > 0)
      )
  
  
  button <- actionButton("upload_fdata_done", "Done", style="float:right")
  
  if(is.null(reactive_dataholder$f_data)){
    button <- disabled(button)
  }
  
  collapseBox(
    value = "data_upload_fdata",
    collapsed =  input$load_example > 0 || AWS,
    "Upload Sample Information (optional)",
    
    strong("Note: this file can be used to assess model performance on new data"),
    
    br(),br(),br(),
    
    radioGroupButtons(inputId = "use_fdata", "Include Sample Information?",
                      c("No", "Yes"), 
                      selected = ifelse(input$load_example > 0 || AWS, "Yes", "No")),
    # uiOutput("how_make_fdata_UI"),
    
    uiOutput("f_data_upload_UI"),
    
    fluidRow(
      column(10, ""),
      column(2, button)
    )
    
  )
})

output$e_meta_load_UI <- renderUI({
  
  req(!is.null(reactive_dataholder$e_data) && 
        (
          (input$upload_fdata_done > 0 && input$use_fdata == "No") || 
            (input$specify_fdata_done > 0 && input$use_fdata == "Yes") ||
            (AWS || input$load_example > 0)
          )
      )
  
  button <- actionButton("upload_emeta_done", "Done", style="float:right")
  
  if(is.null(reactive_dataholder$e_meta)){
    button <- disabled(button)
  }
  
  switch_use <- radioGroupButtons(inputId = "have_emeta", "Include biomolecule information?",
                              c("No", "Yes"), selected = "Yes")
  
  dt <- class(reactive_dataholder$model$norm_omics)
  
  if(dt == "pepData"){
    switch_use <- disabled(switch_use)
  }
  
  collapseBox(
    value = "data_upload_emeta",
    collapsed =  input$load_example > 0  || AWS,
    "Upload biomolecule information",

    switch_use,
    
    uiOutput("e_meta_upload_UI"),
    
    fluidRow(
      column(10, ""),
      column(2, button)
    )
    
  )
})

#### Fdata generation

# output$how_make_fdata_UI <- renderUI({
#   
#   req(!is.null(input$use_fdata) && input$use_fdata == "Yes" && !AWS)
#   
#   div(
#     br(),
#     radioGroupButtons(
#       inputId = "how_make_fdata", label = "Create Sample Information from:",
#       choices = c("Uploaded file" = "upload", 
#                   "Experimental data column names" = "colnames"),
#       selected = character(0)
#     )
#   )
# })

# output$f_data_generate_UI <- renderUI({
#   req(input[["use_fdata"]] == "Yes" && 
#         !is.null(reactive_dataholder[["f_data"]]$file) &&
#         input$fdata_options_done > 0)
#   generate_fdata_UI("f_data")
# })

#### omicsData info filling

output$e_data_spec_UI <- renderUI({
  
  req(!is.null(reactive_dataholder$e_data) && 
        (AWS || input$upload_edata_done > 0 || input$load_example > 0)
      )
  
  
  
  dt <- class(reactive_dataholder$model$norm_omics)
  
  if(dt == "seqData"){
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
  
  dt <- class(reactive_dataholder$model$norm_omics)
  
  pick_datatype <- if(dt == "proData"){
    
    pickerInput(inputId = "pick_dt", label = "What kind of data do you have?",
                choices = c("Label-free peptide" = "Label-free", 
                            "Isobaric-labeled peptide" = "Isobaric", 
                            "Protein" = "Protein"))
    
  } else NULL
  
  title <- ifelse(dt == "seqData", "Specify Expression Data Properties",
                  "Specify Abundance Data Properties")
  
  edat <- reactive_dataholder$e_data$file
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
      
      
      pick_datatype,
      
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
                  selected = isolate(if(!is.null(input$datascale)) 
                    input$datascale else choices[1])
      ),
      uiOutput("RNA_text_warn"),
      
      radioGroupButtons( ## Hide for isobaric non-instrument normalized
        "normalized",
        "Has this data been statistically normalized?",
        choices = c("Yes", "No"),
        selected = isolate(if(!is.null(input$normalized)) 
          input$normalized else "No")
      ),
      
      textInput("na_symbol",
                "What value denotes missing data?",
                value = isolate(if(!is.null(input$na_symbol)) 
                  input$na_symbol else "NA")
      ),
      
      # div(style="display:inline-block",actionButton("specify_edata_done", "Done", style="float:right"))
      
      fluidRow(
        column(10, ""),
        column(2, actionButton("specify_edata_done", "Done", style="float:right"))
      )
    )
  )
})

output$f_data_spec_UI <- renderUI({
  
  req(!is.null(input$use_fdata) && input$use_fdata == "Yes" &&
        !is.null(reactive_dataholder[["f_data"]]$file) &&
        
        ## Upload, example, or generated fdata
        ((!is.null(input$upload_fdata_done) && input$upload_fdata_done > 0) || 
           (AWS || input$load_example > 0)
           # input$fdata_options_done > 0 && 
           # (AWS || input$how_make_fdata == "colnames")
         )
  )
  
  check_cols <- colnames(reactive_dataholder[["e_data"]]$file)
  f_data <- reactive_dataholder[["f_data"]]$file
  best_col <- which.max(map_int(colnames(f_data), function(col){
    sum(as.character(f_data[[col]]) %in% check_cols)
  }))
  
  selected <- colnames(f_data)[best_col]
  
  
  check_cols <- as.character(unique(reactive_dataholder$model$model$pre$mold$outcomes[[1]]))
  f_data <- reactive_dataholder[["f_data"]]$file
  
  best_col <- which.max(map_int(colnames(f_data), function(col){
    sum(as.character(f_data[[col]]) %in% check_cols)
  }))
  
  selected2 <- colnames(f_data)[best_col]
  
  
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
    
    pickerInput(
      "f_data_response_col",
      "Which column contains expected prediction class?",
      choices = colnames(reactive_dataholder[["f_data"]]$file),
      selected = isolate(if(!is.null(input$f_data_response_col))
        input$f_data_response_col else 
        selected2)
    ),
    
    
    fluidRow(
      column(10, ""),
      column(2, actionButton("specify_fdata_done", "Done", style="float:right"))
    )
  )
})

output$e_meta_spec_UI <- renderUI({

  req(isTruthy(input$have_emeta) && input$have_emeta == "Yes" &&
        (AWS || input$upload_emeta_done > 0 || input$load_example > 0) &&
        !is.null(reactive_dataholder[["e_meta"]]$file))
  
  choices <- colnames(reactive_dataholder[["e_meta"]]$file)
  choices <- choices[choices != input$e_data_id_col]
  
  dt <- class(reactive_dataholder$model$norm_omics)
  
  if(dt == "pepData"){
    text <- "Which column identifies proteins?"
  } else {
    text <- "Which column contains information of interest?"
  }
  
  collapseBox(
    "Specify Biomolecule Information Properties",
    icon_id = "edata_params_icon",
    icon = icon("exclamation-sign", lib = "glyphicon"),
    value = "data_props",
    collapsed = F,
    
    pickerInput(
      "e_meta_id_col",
      text,
      choices = choices,
      selected = isolate(if(!is.null(input$e_meta_id_col)) input$e_meta_id_col else choices[1])
    ),
    
    # div(style="display:inline-block",actionButton("specify_emeta_done", "Done", style="float:right"))
    
    
    fluidRow(
      column(10, ""),
      column(2, 
             actionButton("specify_emeta_done", "Done", style="float:right"))
    )
  )
})
