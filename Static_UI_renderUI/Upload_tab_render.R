
## UI behaviors specific to static ui options
## E.g. text changes, icons, hide/show, accordion behaviors, different stuff for different datatypes

## NOT for reacting to modules!

## Picker specific text for RNA/nonRNA
output$data_select_UI <- renderUI({
  
  req(!is.null(input$data_type))
  
  label_edata <- ifelse(input$data_type == "RNA-seq", 
                        "Expression data", "Abundance data")
  label_emeta <- ifelse(input$data_type == "RNA-seq", 
                        "Transcript information", "Biomolecule information")
  relevant_examples <- ifelse(input$data_type == "RNA-seq", 
                              "Optional (E.g., KEGG pathways, genes, proteins)",
                              "Optional (E.g., KEGG pathways, lipid class, molecular activity)"
                              )
  
  
  choices <- c("e_data","e_meta")
  names(choices) <- c(label_edata, label_emeta)
  
  pickerInput(
    "data_select",
    "What experimental files do you have?",
    multiple = T,
    choices = choices,
    choicesOpt = list(
      disabled = c(1,0),
      subtext = c("Required",
                  relevant_examples
      )),
    selected = if(!is.null(isolate(input$normalized))) input$normalized else "e_data"
  )
})

## Module UI
output$e_data_upload_UI <- renderUI({
  
  req(!input$use_example && input$data_type_done > 0)
  
  label <- ifelse(input$data_type == "RNA-seq", 
                  "Upload Expression File", "Upload Abundance File")
  div(
    collapseBox(
      label,
      icon_id = "edata_params_icon",
      icon = icon("exclamation-sign", lib = "glyphicon"),
      value = "upload_edata_UI_box",
      collapsed = F,
      fileinput_UI("e_data_upload", "e_data", 
                   is_RNA = (input$data_type == "RNA-seq")),
      actionButton("edata_upload_done", "Done")
    )
  )
})

output$e_meta_upload_UI <- renderUI({
  
  req(!input$use_example && 
        input$data_type_done > 0 && 
        input$specify_edata_done > 0)
  req("e_meta" %in% input$data_select)
  
  div(
    collapseBox(
      "Upload Biomolecule Information",
      icon_id = "emeta_params_icon",
      icon = icon("exclamation-sign", lib = "glyphicon"),
      value = "upload_e_meta_UI_box",
      collapsed = F,
      fileinput_UI("e_meta_upload", "e_meta"),
      actionButton("emeta_upload_done", "Done")
    )
  )
  
})

## Picker specific text for RNA/nonRNA
output$e_data_spec_UI <- renderUI({
  
  req(input$data_type_done > 0 && !is.null(input$data_type))
  
  req((input$edata_upload_done > 0 || input$use_example) &&
        !is.null(input$data_type) && 
        !is.null(reactive_dataholder[["e_data"]]$file))
  
  if(input$data_type == "RNA-seq"){
    choices <- list(
      "Counts" = "counts",
      "Natural log counts per million" = "lcpm_log",
      "Log base 2 counts per million" = "lcpm_log2",
      "Log base 10 counts per million" = "lcpm_log10"
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
  
  div(
    collapseBox(
      title,
      value = "params_box",
      icon_id = "edata_params_icon",
      icon = icon("exclamation-sign", lib = "glyphicon"),
      collapsed = F,
      
      pickerInput(
        "e_data_id_col",
        "Which column identifies biomolecules?",
        choices = colnames(reactive_dataholder[["e_data"]]$file),
        selected = isolate(if(!is.null(input$e_data_id_col)) input$e_data_id_col else character(0))
      ),
      
      pickerInput("datascale",
                  "On what scale is this data on?",
                  choices = choices,
                  multiple = TRUE,
                  options = pickerOptions(maxOptions = 1),
                  selected = isolate(if(!is.null(input$datascale)) input$datascale else character(0))
      ),
      
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
      actionButton("specify_edata_done", "Done")
    )
  )
})

output$e_meta_spec_UI <- renderUI({
  
  req("e_meta" %in% input$data_select && 
        (input$emeta_upload_done > 0 || 
           input$specify_edata_done > 0 && input$use_example) &&
        !is.null(reactive_dataholder[["e_meta"]]$file))
  
  req(input$data_type_done > 0 && !is.null(input$data_type))
  
  collapseBox(
    "Specify Biomolecule Information Properties",
    icon_id = "edata_params_icon",
    icon = icon("exclamation-sign", lib = "glyphicon"),
    value = "data_props",
    collapsed = F,
    
    pickerInput(
      "e_meta_id_col",
      "Which column identifies proteins?",
      choices = colnames(reactive_dataholder[["e_meta"]]$file),
      selected = isolate(if(!is.null(input$e_meta_id_col)) input$e_meta_id_col else character(0))
    ),
    
    actionButton("specify_emeta_done", "Done")
  )
})

## Accordion behavior
observeEvent(input$data_type_done, {
  updateBoxCollapse(session, "upload_collapse_left", close = "datselect")
})

observeEvent(input$edata_upload_done, {
  updateBoxCollapse(session, "upload_collapse_left", close = "upload_edata_UI_box")
})

observeEvent(input$emeta_upload_done, {
  updateBoxCollapse(session, "upload_collapse_left", close = "upload_e_meta_UI_box")
})

observeEvent(input$specify_edata_done, {
  updateBoxCollapse(session, "upload_collapse_left", close = "params_box")
})

observeEvent(input$specify_emeta_done, {
  updateBoxCollapse(session, "upload_collapse_left", close = "data_props")
  
})

observeEvent(input$upload_done, {
  updateBoxCollapse(session, "upload_preview_collapse", 
                    close = "summary_tables", open = "summary")
  
})

## completion button show
observe({
  cond <- (!("e_meta" %in% input$data_select) && 
             !is.null(input$specify_edata_done) &&
             input$specify_edata_done > 0) ||
    ("e_meta" %in% input$data_select && 
       !is.null(input$specify_emeta_done) &&
       input$specify_emeta_done > 0)
  
  
  toggleElement("upload_done", condition = cond)
})

