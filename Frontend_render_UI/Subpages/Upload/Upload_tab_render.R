
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
                              "(E.g., KEGG pathways, genes, proteins)",
                              "(E.g., KEGG pathways, lipid class, molecular activity)"
                              )
  
  
  choices <- c("e_data","e_meta")
  names(choices) <- c(label_edata, label_emeta)
  
  div(
    p(tags$b("What experimental files do you have?")),
    
    disabled(prettySwitch("have_edata", label_edata, value = T, fill = T, status = "primary")),
    p("Required", style = "margin-top: -10px;"),
    br(),
    if (input$data_type %in% c("Label-free", "Isobaric")) {
      tagList(
        disabled(prettySwitch("have_emeta", label_emeta, value = T, fill = T, status = "primary")),
        div("Required", relevant_examples, style = "margin-top: -10px;")
      )
    } else {
      tagList(
        div(
          id = 'have_emeta_js',
          prettySwitch("have_emeta", label_emeta, value = F, fill = T, status = "primary")
        ),
        div("Optional", relevant_examples, style = "margin-top: -10px;")
      )
    },
    br()
  )
})

## Upload dataset UI
output$e_data_upload_UI <- renderUI({
  
  req(!input$use_example && input$data_type_done > 0 && !AWS)
  req(!is.null(input$data_type))
  
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
      # div(style="display:inline-block",actionButton("edata_upload_done", "Done", style="float:right"))
      
      fluidRow(
        column(10, ""),
        column(2, actionButton("edata_upload_done", "Done", style="float:right"))
      )
    )
  )
})

output$e_meta_upload_UI <- renderUI({
  
  req(!input$use_example && 
        input$data_type_done > 0 && 
        input$specify_edata_done > 0 && !AWS)
  req(!is.null(input$data_type))
  req(isTruthy(input$have_emeta))
  
  div(
    collapseBox(
      "Upload Biomolecule Information",
      icon_id = "emeta_params_icon",
      icon = icon("exclamation-sign", lib = "glyphicon"),
      value = "upload_e_meta_UI_box",
      collapsed = F,
      fileinput_UI("e_meta_upload", "e_meta"),
      # div(style="display:inline-block",actionButton("emeta_upload_done", "Done", style="float:right"))
      
      fluidRow(
        column(10, ""),
        column(2, actionButton("emeta_upload_done", "Done", style="float:right"))
      )
    )
  )
  
})

## Picker specific text for RNA/nonRNA
output$e_data_spec_UI <- renderUI({
  
  req(input$data_type_done > 0 && !is.null(input$data_type))
  
  req((input$edata_upload_done > 0 || input$use_example || AWS || data_from_map()) &&
        !is.null(input$data_type) && 
        !is.null(reactive_dataholder[["e_data"]]$file))
  
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
  
  edat <- reactive_dataholder[["e_data"]]$file
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

output$RNA_text_warn <- renderUI({
  
  req(!is.null(input$data_type) && 
        input$data_type == "RNA-seq" &&
        !is.null(input$datascale) &&
        !(input$datascale %in% c("counts", "lcpm")))
  
  text_warn <- paste0("Note: If the intent of the model is to",
                      " be used with new data, data must be imported as",
                      " counts or log2 counts per million. If neither of",
                      " these are selected, users will not ",
                      "be able to export to an RDS object.")
  
  div(
    text_warn,
    br(), br()
  )
  
})

output$e_meta_spec_UI <- renderUI({
  
  req(isTruthy(input$have_emeta) && 
        (input$emeta_upload_done > 0 || 
           input$specify_edata_done > 0 && (input$use_example || AWS)) &&
        !is.null(reactive_dataholder[["e_meta"]]$file))
  
  req(input$data_type_done > 0 && !is.null(input$data_type))
  
  choices <- colnames(reactive_dataholder[["e_meta"]]$file)
  choices <- choices[choices != input$e_data_id_col]
  
  if(input$data_type %in% c("Label-free", "Isobaric")){
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
      selected = isolate(if(!is.null(input$e_meta_id_col)) input$e_meta_id_col else character(0))
    ),
    
    # div(style="display:inline-block",actionButton("specify_emeta_done", "Done", style="float:right"))
    
    
    fluidRow(
      column(10, ""),
      column(2, actionButton("specify_emeta_done", "Done", style="float:right"))
    )
  )
})


## Plot for edata

output$boxplot_UI <- renderPlotly({
  
  req(!is.null(reactive_dataholder$e_data))
  
  df <- reactive_dataholder$e_data$file
  df <- df[colnames(df) != input$e_data_id_col]
  df <- gather(df)
  df <- df[!is.na(df$value),]
  
  p <- ggplot(df, aes(x = key, y = value)) + geom_boxplot() + theme_bw() + 
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) +
    labs(x = "", y = ifelse(input$data_type != "RNA-seq", "Abundance", "Counts"))
  
  isolate(plot_table_current$table$Upload__boxplot <- p)
  
  p
  
})

## Accordion behavior

output$detected_box_upload <- renderUI({
  
  req(input$specify_edata_done > 0)
  div(collapseBox(
    "Data Properties",
    value = "summary",
    # uiOutput("Characteristics_module_tabset")
    uiOutput("boxplot_UI_render")#,
    # uiOutput("show_log_UI")
  ))
})

output$boxplot_UI_render <- renderUI({
  if (isTruthy(input$boxplot_UI_load_button) || dim(reactive_dataholder$e_data$file)[1] < 20000) {
    withSpinner(plotlyOutput("boxplot_UI"))
  } else {
    div(
      "This plot is large and may take a while to render.",
      actionButton("boxplot_UI_load_button", "Show plot")
    )
  }
})

observeEvent(input$data_type, {
  if (!is.null(input$data_type)) {
    shinyjs::enable("data_type_done")
    shinyjs::show("use_example")
  } else {
    shinyjs::disable("data_type_done")
    shinyjs::hide("use_example")
  }
}, ignoreNULL = FALSE)

observeEvent(input$data_type_done, {
  updateBoxCollapse(session, "upload_collapse_left", close = "datselect")
})

observeEvent(input$edata_upload_done, {
  updateBoxCollapse(session, "upload_collapse_left", close = "upload_edata_UI_box")
})

observeEvent(input$emeta_upload_done, {
  updateBoxCollapse(session, "upload_collapse_left", close = "upload_e_meta_UI_box")
})

observeEvent(c(input$datascale, input$normalized), {
  if (!is.null(input$datascale) && !is.null(input$normalized)) {
    shinyjs::enable("specify_edata_done")
  } else {
    shinyjs::disable("specify_edata_done")
  }
}, ignoreNULL = FALSE)

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
  cond <- (!(isTruthy(input$have_emeta)) && 
             !is.null(input$specify_edata_done) &&
             input$specify_edata_done > 0) ||
    (isTruthy(input$have_emeta) && 
       !is.null(input$specify_emeta_done) &&
       input$specify_emeta_done > 0)
  
  
  toggleElement("upload_done", condition = cond)
})

