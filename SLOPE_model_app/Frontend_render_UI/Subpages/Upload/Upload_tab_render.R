output$e_data_spec_UI <- renderUI({
  
  req(!is.null(input$upload_edata))
  
  if(input$data_type == "RNA-seq"){
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
  
  title <- ifelse(input$data_type == "RNA-seq", "Specify Expression Data Properties",
                  "Specify Abundance Data Properties")
  
  edat <- new_edata
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