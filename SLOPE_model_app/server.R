options(shiny.maxRequestSize = 250 * 1024^2, 
        ch.dir = TRUE, 
        expressions = 5e5,
        DT.TOJSON_ARGS = list(na = "string"))

shinyServer(function(session,input,output){
  omicsData <- reactiveValues(new_edata = NULL, new_fdata = NULL, model = NULL)

  # edata upload
  observeEvent(input$upload_edata,ignoreInit = TRUE,{
    req(input$upload_edata)
    req(!is.null(input$upload_edata$name))
    print("File upload event triggered")
    # read in RDS object
    new_edata <- tryCatch({
      read.csv(input$upload_edata$datapath)
    }, error = function(e) {
      showNotification(paste("Error in reading CSV file:", e$message), type = "error")
      NULL
    })
    omicsData$new_edata <- new_edata
  })
  
  # fdata upload
  observeEvent(input$upload_fdata,ignoreInit = TRUE,{
    req(input$upload_fdata)
    req(!is.null(input$upload_fdata$name))
    print("File upload event triggered")
    # read in RDS object
    new_fdata <- tryCatch({
      read.csv(input$upload_fdata$datapath)
    }, error = function(e) {
      showNotification(paste("Error in reading CSV file:", e$message), type = "error")
      NULL
    })
    omicsData$new_fdata <- new_fdata
  })
  
  # model upload
  observeEvent(input$upload_model,ignoreInit = TRUE,{
    req(input$upload_model)
    req(!is.null(input$upload_model$name))
    print("File upload event triggered")
    # read in RDS object
    rds_model <- tryCatch({
      readRDS(input$upload_model$datapath)
    }, error = function(e) {
      showNotification(paste("Error in reading RDS file:", e$message), type = "error")
      NULL
    })
    omicsData$rds_model <- rds_model
  })
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
    
    edat <- omicsData$new_edata
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
  
  # fdata ui
  output$f_meta_spec_UI <- renderUI({
    
    req(!is.null(input$upload_fdata))
    
    check_cols <- colnames(omicsData$new_fdata)
    f_data <- omicsData$new_fdata
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
        choices = colnames(omicsData$new_fdata),
        selected = isolate(if(!is.null(input$f_data_id_col)) input$f_data_id_col else selected)
      ),
      # div(style="display:inline-block",actionButton("specify_fdata_done", "Done", style="float:right"))
      
      fluidRow(
        column(10, ""),
        column(2, actionButton("specify_fdata_done", "Done", style="float:right"))
      )
    )
  })
  
  # create omics data
  observeEvent(input$check_selections_upload, {
    
    req(!is.null(input$data_type))
    
    edata <- omicsData$new_edata
    fdata <- omicsData$new_fdata
    
    edata_cname <- input$e_data_id_col
    fdata_cname <- input$f_data_id_col
    
    if(is.null(fdata)){
      fdata <- data.frame(SampleID = colnames(edata)[colnames(edata) != edata_cname],
                          Col1 = colnames(edata)[colnames(edata) != edata_cname])
      fdata_cname <- "SampleID"
    }
    
    data_scale <- input$datascale
    is_normalized <- input$normalized == "Yes"
    na_replace <- input$na_symbol
    
    ## this will need to switch to with naming convention updates
    fn_use <- switch(
      input$data_type,
      "Positive" = "as.lipidData", 
      "Negative" = "as.lipidData", 
      "Label-free" = "as.pepData",
      "Isobaric" = "as.isobaricpepData", 
      "Protein" = "as.proData", 
      "ProteinTMT" = "as.proData", 
      "GC-MS" = "as.metabData", 
      "NMR" = "as.nmrData",
      "RNA-seq" = "as.seqData"
    )
    
    object_fn <- get(fn_use)
    
    if(input$data_type == "RNA-seq"){
      data_scale_hold <- data_scale
      data_scale <- "counts"
    }
    
    # create first object
    od <- tryCatch(
      {
        res <- object_fn(
          e_data = edata, f_data = fdata,
          edata_cname = edata_cname, fdata_cname = fdata_cname,
          data_scale = data_scale, is_normalized = is_normalized
        )
        
        if(input$data_type != "RNA-seq"){
          res <- res %>% edata_replace(na_replace, NA)
        } else {
          attr(res, "data_info")$data_scale_actual <- data_scale_hold
        }
        
        res
      },
      error = function(e) {
        msg <<- paste0("Something went wrong processing your omicsData object \n System error:  ", e)
        # revals$warnings_upload$badupload <- sprintf("<p style = color:red>%s</p>", msg)
        NULL
      }
    )
    
    if (is.null(od)) {
      shinyalert("Something went wrong:", msg)
      return(NULL)
    }
    
    if(is.null(omicsData$new_fdata)){
      od$f_data <- NULL
    }
    
    # isolate({
    #   table_table_current$table$Upload__e_data <- od$e_data
    #   if(!is.null(od$f_data)) table_table_current$table$Upload__f_data <- od$f_data
    #   if(!is.null(od$e_meta)) table_table_current$table$Upload__e_meta <- od$e_meta
    #   table_table_current$table$Upload__summary <- summary(od)
    # })
    
    omicsData$obj <-  od
    
  })
  
  # transformation
  output$transform_picker_UI <- renderUI({
    
    if(inherits(omicsData$obj, "seqData")){
      # return(
      #   div(
      #     br(),
      #     "Data scaling and transformation is not appropriate for count-based RNA-seq data. Click 'Done' to continue.",
      #     br(),br()
      #   )
      #   )
      
      if(attr(omicsData$obj, "data_info")$data_scale_actual == "counts"){
        choices <- list("Log2 counts per million" = "lcpm",
                        "Upper-quantile transformed counts" = "upper",
                        "Median counts" = "median")
        text_warn <- paste0("Note: If the intent of the model is to",
                            " be used with new data, users must use ",
                            "the Log2 counts per million method, in order to",
                            " minimize the impact of batch effects. If ",
                            "this method is not selected, users will not ",
                            "be able to export to an RDS object.")
        
      } else{
        choices <- NULL
        text_warn <- paste0("Transformation for RNA-seq data",
                            " is only available for counts.")
      }
      
      
    } else {
      
      text_warn <- NULL
      
      choices <- list("Raw intensity" = "abundance", 
                      "Log base 2" = "log2", 
                      "Log base 10" = "log10", 
                      "Natural log" = "log")
    }
    
    choices <- choices[!(choices %in% get_data_scale(omicsData$obj))]
    
    # MAY NEED TO UPDATE THIS IF WE ARE NOT WORKING WITH BEGINNER SETTINGS
    rna_ds <- attr(omicsData$obj, "data_info")$data_scale_actual
    
    set <-  if(!is.null(rna_ds) && 
               rna_ds %in% c("upper", "median", "lcpm")){
      "No transformation"
    } else if(get_data_scale(omicsData$obj) == "log2"){
      "No transformation"
    } else if (!is.null(rna_ds) && rna_ds == "counts"){
      "lcpm"
    } else "log2"
    
    out <- disabled(pickerInput("transform", "Transform data to:", 
                                choices = c(choices, "No transformation"),
                                selected = set))
    
    div(
      out,
      text_warn,
      br(),
      br()
    )
    
  })
  
  observeEvent(input$done_tr_box,{
    req(!is.null(input$transform) && !is.null(input$datascale))
    
    if(input$transform != input$datascale){
      omicsData$obj_t <- edata_transform(omicsData$obj,data_scale = input$transform)
    } else {
      omicsData$obj_t <- omicsData$obj
    }
    
    # place holder for real stuff
    # but just convert every missing value to 0 just to have something in place
    omicsData$obj_t$e_data[is.na(omicsData$obj_t$e_data)] <- 0
    
  })
  
  # normalize data
  observeEvent(input$normalize_01_scaling,{
    req(!is.null(omicsData$obj_t))
    norm_object <- pmartR:::zero_one_scale(omicsData$obj_t$e_data,
                                           get_edata_cname(omicsData$obj_t))
    omicsData$obj_norm <- omicsData$obj_t
    omicsData$obj_norm$e_data <- norm_object$transf_data
  })
  
  # run model
  observeEvent(input$run_model,{
    
    req(!is.null(omicsData$obj_norm))
    req(!is.null(omicsData$rds_model))
    
    new_info <- omicsData$obj_norm$e_data[,-which(colnames(omicsData$obj_norm$e_data) == pmartR::get_edata_cname(omicsData$obj_norm))] %>%
      t() %>% data.frame()
    names(new_info) <- paste0("feature_",seq(from = 1, to = ncol(new_info)))
    
    unique_outcomes <- unlist(unique(omicsData$rds_model$pre$mold$outcomes))

    contains_exact_values <- function(column, values) {
      all(sort(column[!is.na(column)]) %in% (values))
    }
    
    # Apply the function to each column and find the matching column name
    matching_column <- colnames(omicsData$obj_norm$f_data)[apply(omicsData$obj_norm$f_data, 2, contains_exact_values, values = unique_outcomes)][1]
    
    fdata_sub <- omicsData$obj_norm$f_data %>%
      dplyr::select(!!as.symbol(pmartR::get_fdata_cname(omicsData$obj_norm)),
                    !!as.symbol(matching_column))
    
    new_info <- new_info %>%
      tibble::rownames_to_column(var = pmartR::get_fdata_cname(omicsData$obj_norm)) %>%
      dplyr::left_join(fdata_sub,by = pmartR::get_fdata_cname(omicsData$obj_norm))
    
    new_info <- new_info %>%
      dplyr::rename(response = !!as.symbol(matching_column)) %>%
      tibble::column_to_rownames(var = pmartR::get_fdata_cname(omicsData$obj_norm))
    omicsData$predictions <- data.frame(predict(omicsData$rds_model,new_data = new_info)) %>%
      dplyr::rename(Predictions = `.pred_class`)
  })
  
  output$predict_plot <- renderPlot({
    req(!is.null(omicsData$predictions))
    pred_plot = ggplot(omicsData$predictions,aes(x = Predictions)) +
      geom_bar() + 
      theme_bw() + 
      labs(title = "Predictions", y = "Count")
    pred_plot
  })
  
  # BROWSER
  observeEvent(input$Browser,{
    browser()
  })
})