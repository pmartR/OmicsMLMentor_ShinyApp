options(shiny.maxRequestSize = 250 * 1024^2, 
        ch.dir = TRUE, 
        expressions = 5e5,
        DT.TOJSON_ARGS = list(na = "string"),
        shiny.fullstacktrace=TRUE)

shinyServer(function(session,input,output){
  
  source("RV_all.R")
  
  file_loads <- c(
    list.files("./Frontend_render_UI", recursive = T, full.names = T)
  )
  
  for (f in grep(".R$", file_loads, value = T)) source(f, local = TRUE)
  
  # # edata upload
  # observeEvent(input$upload_edata,ignoreInit = TRUE,{
  #   req(input$upload_edata)
  #   req(!is.null(input$upload_edata$name))
  #   print("File upload event triggered")
  #   # read in RDS object
  #   new_edata <- tryCatch({
  #     read.csv(input$upload_edata$datapath)
  #   }, error = function(e) {
  #     showNotification(paste("Error in reading CSV file:", e$message), type = "error")
  #     NULL
  #   })
  #   omicsData$new_edata <- new_edata
  # })
  # 
  # # fdata upload
  # observeEvent(input$upload_fdata,ignoreInit = TRUE,{
  #   req(input$upload_fdata)
  #   req(!is.null(input$upload_fdata$name))
  #   print("File upload event triggered")
  #   # read in RDS object
  #   new_fdata <- tryCatch({
  #     read.csv(input$upload_fdata$datapath)
  #   }, error = function(e) {
  #     showNotification(paste("Error in reading CSV file:", e$message), type = "error")
  #     NULL
  #   })
  #   omicsData$new_fdata <- new_fdata
  # })
  
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
  
  output$e_meta_spec_UI <- renderUI({
    req(!is.null(input$upload_emeta))
    # req(isTruthy(input$have_emeta) && 
    #       (input$emeta_upload_done > 0 || 
    #          input$specify_edata_done > 0 && (input$use_example || AWS)) &&
    #       !is.null(reactive_dataholder[["e_meta"]]$file))
    
    # req(input$data_type_done > 0 && !is.null(input$data_type))
    
    #choices <- colnames(reactive_dataholder[["e_meta"]]$file)
    choices <- colnames(omicsData$new_emeta)
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
  
  # create omics data
  observeEvent(input$check_selections_upload, {
    
    req(!is.null(input$data_type))
    
    edata <- omicsData$new_edata
    fdata <- omicsData$new_fdata
    emeta <- omicsData$new_emeta
    
    edata_cname <- input$e_data_id_col
    fdata_cname <- input$f_data_id_col
    emeta_cname <- input$e_meta_id_col
    
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
          e_data = edata, f_data = fdata, e_meta = emeta,
          edata_cname = edata_cname, fdata_cname = fdata_cname, emeta_cname = emeta_cname,
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
  
  output$transformation_backfill_text <- renderText({
    req(!is.null(omicsData$rds_model) && !is.null(omicsData$obj))
    num_in_og <- length(attributes(omicsData$rds_model$full_model)$feature_info$names_orig)
    num_in_both <- sum(unique(omicsData$obj$e_meta$Leading.razor.protein) %in% attributes(omicsData$rds_model$full_model)$feature_info$names_orig)
    num_in_og_not_new <- num_in_og - num_in_both
    num_in_just_new <- sum(!unique(omicsData$obj$e_meta$Leading.razor.protein) %in% attributes(omicsData$rds_model$full_model)$feature_info$names_orig)
    
    paste0("The original model had ", num_in_og, " distinct molecules. Of those molecules, ", num_in_og_not_new,
           " were not identified in the new dataset and will need to be backfilled as 0s for the model to run.", 
           " Additionally, ", num_in_just_new, " molecules were identified in only the new dataset and therefore ",
           "will be removed prior to running the model.")
    
  })
  
  output$transformation_backfill_plot <- renderPlot({
    req(!is.null(omicsData$rds_model) && !is.null(omicsData$obj))
    num_in_og <- length(attributes(omicsData$rds_model$full_model)$feature_info$names_orig)
    num_in_both <- sum(unique(omicsData$obj$e_meta$Leading.razor.protein) %in% attributes(omicsData$rds_model$full_model)$feature_info$names_orig)
    num_in_og_not_new <- num_in_og - num_in_both
    num_in_just_new <- sum(!unique(omicsData$obj$e_meta$Leading.razor.protein) %in% attributes(omicsData$rds_model$full_model)$feature_info$names_orig)
    
    backfill_plot <- data.frame(Dataset = factor(c("Only Original","Both", "Only New"),levels = c("Only Original","Both", "Only New")),
               Value = c(num_in_og_not_new,num_in_both,num_in_just_new)) %>%
      ggplot(aes(x = Dataset, y = Value,fill = Dataset)) + 
      geom_col() +
      geom_text(aes(label = paste0("n = ",Value),y = Value + 200)) +
      theme_bw() + 
      labs(y = "Number of Distinct Proteins Identified",x = "") + 
      guides(fill = "none")
    backfill_plot
    
  })
  
  # transformation
  output$transform_picker_UI <- renderUI({
    req(!is.null(omicsData$obj))
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
  
  observeEvent(input$check_selections_upload,{
    req(!is.null(omicsData$obj))
    
    # pipeline
    omics_processed = omicsData$obj
    
    # log2 or otherwise
    data_scale_info <- attributes(omicsData$rds_model$pp_omics)$data_info$data_scale
    
    # this will be used in all scenarios - will need to build this up
    # more with more filters in the future
    all_filter_functions <- c("custom_filter","molecule_filter","cv_filter")
    names(all_filter_functions) <- c("customFilt","moleculeFilt","cvFilt")
    # imdanova filter()
    # rmd_filter()
    
    # arguments needed for those functions
    all_filter_requirements <- list(
      "moleculeFilt" = list("threshold" = NA,method = list("use_groups" = NA,"use_batch" = NA)),
      "cvFilt" = list("threshold" = NA,method = list("use_groups" = NA)))
    
    # get the different filters used in omics object from original model
    filter_info <- pmartR::get_filters(omicsData$rds_model$norm_omics)
    filter_types <- unlist(sapply(filter_info,function(x) x['type']))
    filter_types <- filter_types[filter_types != "customFilt"]
    
    # subset to ones present in this specific analysis
    all_filter_requirements_specific <- all_filter_requirements[which(names(all_filter_requirements) %in% filter_types)]
    
    # iterate through the specific filters for this dataset
    for(i in 1:length(all_filter_requirements_specific)){
      
      # molecule filter
      if(names(all_filter_requirements_specific)[i] == "moleculeFilt"){
        # which filter in original matches this ?
        og_filter_id = which(unlist(sapply(filter_info,function(x) x['type'])) == "moleculeFilt")
        # add in attributes
        all_filter_requirements_specific[[i]]$threshold = attr(omicsData$rds_model$norm_omics,"filters")[[og_filter_id]]$threshold
        all_filter_requirements_specific[[i]]$method$use_groups = attr(omicsData$rds_model$norm_omics,"filters")[[og_filter_id]]$method$use_groups
        all_filter_requirements_specific[[i]]$method$use_batch = attr(omicsData$rds_model$norm_omics,"filters")[[og_filter_id]]$method$use_batch
      }
      # cv filter
      if(names(all_filter_requirements_specific)[i] == "cvFilt"){
        # which filter in original matches this ?
        og_filter_id = which(unlist(sapply(filter_info,function(x) x['type'])) == "cvFilt")
        # add in attributes
        all_filter_requirements_specific[[i]]$threshold = attr(omicsData$rds_model$norm_omics,"filters")[[og_filter_id]]$threshold
        all_filter_requirements_specific[[i]]$method$use_groups = attr(omicsData$rds_model$norm_omics,"filters")[[og_filter_id]]$method$use_groups
      }
    }
    
    # normalization
    norm_method <- attr(omicsData$rds_model$norm_omics,"data_info")$norm_info$norm_fn

    # rollup
    # if we have pepdata in normalized object but prodata in pp omics object
    # this means that we have undergone protein rollup
    # so we need to rollup as well
    rollup_method = "Not Applicable"
    if("proData" %in% class(omicsData$rds_model$pp_omics) & "pepData" %in% class(omicsData$rds_model$norm_omics)){
      rollup_method = attr(omicsData$rds_model$pp_omics,"pro_quant_info")$method
    }
    
    omicsData$pp_info <- list("scale" = data_scale_info,
                              "normalization" = norm_method,
                              "filters" = all_filter_requirements_specific,
                              "rollup" = rollup_method)
    
    # add in group designation if needed for downstream filters
    if(!is.null(attr(omicsData$rds_model$norm_omics,"group_DF"))){
      # need check that main effect column is in both original and new dataset
      omicsData$obj <- pmartR::group_designation(omicsData$obj,
                                                   main_effects = attributes(attr(omicsData$rds_model$norm_omics,"group_DF"))$main_effects)
    }
    
    # convert to log2 scale immediately
    if(data_scale_info != attributes(omicsData$obj)$data_info$data_scale){
      omicsData$obj_scaled = pmartR::edata_transform(omicsData$obj,data_scale_info)
    } else {
      omicsData$obj_scaled = omicsData$obj
    }
  })

  
  observeEvent(input$confirm_filters,{
    req(omicsData$obj_scaled)
    # pipeline
    omics_processed = omicsData$obj_scaled
    
    # this will be used in all scenarios - will need to build this up
    # more with more filters in the future
    all_filter_functions <- c("custom_filter","molecule_filter","cv_filter")
    names(all_filter_functions) <- c("customFilt","moleculeFilt","cvFilt")
    # imdanova filter()
    # rmd_filter()
    
    # arguments needed for those functions
    all_filter_requirements <- list(
      "moleculeFilt" = list("threshold" = NA,method = list("use_groups" = NA,"use_batch" = NA)),
      "cvFilt" = list("threshold" = NA,method = list("use_groups" = NA)))
    
    # get the different filters used in omics object from original model
    filter_info <- pmartR::get_filters(omicsData$rds_model$norm_omics)
    filter_types <- unlist(sapply(filter_info,function(x) x['type']))
    filter_types <- filter_types[filter_types != "customFilt"]
    
    # subset to ones present in this specific analysis
    all_filter_requirements_specific <- all_filter_requirements[which(names(all_filter_requirements) %in% filter_types)]
    
    # iterate through the specific filters for this dataset
    for(i in 1:length(all_filter_requirements_specific)){
      
      # molecule filter
      if(names(all_filter_requirements_specific)[i] == "moleculeFilt"){
        # which filter in original matches this ?
        og_filter_id = which(unlist(sapply(filter_info,function(x) x['type'])) == "moleculeFilt")
        # add in attributes
        all_filter_requirements_specific[[i]]$threshold = attr(omicsData$rds_model$norm_omics,"filters")[[og_filter_id]]$threshold
        all_filter_requirements_specific[[i]]$method$use_groups = attr(omicsData$rds_model$norm_omics,"filters")[[og_filter_id]]$method$use_groups
        all_filter_requirements_specific[[i]]$method$use_batch = attr(omicsData$rds_model$norm_omics,"filters")[[og_filter_id]]$method$use_batch
        
        filtObj = pmartR::molecule_filter(omics_processed,all_filter_requirements_specific[[i]]$method$use_groups,all_filter_requirements_specific[[i]]$method$use_batch)
        omics_processed = pmartR::applyFilt(filtObj,omics_processed,min_num = all_filter_requirements_specific[[i]]$threshold)
      }
      # cv filter
      if(names(all_filter_requirements_specific)[i] == "cvFilt"){
        # which filter in original matches this ?
        og_filter_id = which(unlist(sapply(filter_info,function(x) x['type'])) == "cvFilt")
        # add in attributes
        all_filter_requirements_specific[[i]]$threshold = attr(omicsData$rds_model$norm_omics,"filters")[[og_filter_id]]$threshold
        all_filter_requirements_specific[[i]]$method$use_groups = attr(omicsData$rds_model$norm_omics,"filters")[[og_filter_id]]$method$use_groups
        
        filtObj = pmartR::cv_filter(omics_processed,all_filter_requirements_specific[[i]]$method$use_groups)
        omics_processed = pmartR::applyFilt(filtObj,omics_processed,cv_threshold = all_filter_requirements_specific[[i]]$threshold)
      }
    }
    
    # convert to normalization immediately
    if((attributes(omicsData$obj)$data_info$norm_info$is_normalized == FALSE)|
       (!is.null(attributes(omicsData$obj)$data_info$norm_info$norm_fn) && attributes(omicsData$obj)$data_info$norm_info$norm_fn != norm_method)){
      omicsData$obj_norm <- pmartR::normalize_zero_one_scaling(omics_processed)
    } else {
      omicsData$obj_norm <- omics_processed
    }
    
    # convert to sl object
    omics_sl <- as.slData(omicsData$obj_norm)
    omicsData$obj_sl <- omics_sl
    
    # rollup
    # if we have pepdata in normalized object but prodata in pp omics object
    # this means that we have undergone protein rollup
    # so we need to rollup as well
    if("proData" %in% class(omicsData$rds_model$pp_omics) & "pepData" %in% class(omicsData$rds_model$norm_omics)){
      rollup_method = attr(omicsData$rds_model$pp_omics,"pro_quant_info")$method
      omicsData$obj_sl_rollup <- slopeR::protein_rollup(omics_sl,method = rollup_method)
    }
  })
  
  output$protein_rollup_pp_UI <- renderUI({
    req(omicsData$obj_sl)
    
    if(!is.null(omicsData$obj_sl_rollup)){
      collapseBox(
        div(
          "Rollup",
        ),
        value = "rollup",
        collapsed = F,
        plotOutput("transformation_rollup_plot")
      )
    }
  })

  
  
  observeEvent(input$done_molecule_filter,{
    req(!is.null(omicsData$obj))
    
    molfilt_1 <- molecule_filter(omicsData$obj)
    omicsData$obj_mol <- applyFilt(molfilt_1, omicsData = omicsData$obj,min_num = 2)
  })
  
  
  
  
  
  # run model
  observeEvent(input$run_model,{
    
    req(!is.null(omicsData$obj_sl)|!is.null(omicsData$obj_sl_rollup))
    req(!is.null(omicsData$rds_model$full_model))
    
    # if the data is rolled up use that version, otherwise use OG slope version
    if(!is.null(omicsData$obj_sl_rollup)){
      omicsNewdata = omicsData$obj_sl_rollup
    } else {
      omicsNewdata = omicsData$obj_sl
    }
    
    orig_names <- attr(omicsData$rds_model$full_model,"feature_info")
    # remove columns not found in training data
    pro_edata <- omicsNewdata$e_data
    pro_edata <- pro_edata[unlist(pro_edata[pmartR::get_emeta_cname(omicsNewdata)]) %in% orig_names$names_orig,]
    # find which are found in the OG version but not in the new data
    # we will want
    in_og_not_new <- orig_names$names_orig[!orig_names$names_orig %in% unlist(pro_edata[pmartR::get_emeta_cname(omicsNewdata)])]
    
    in_og_not_new_df <- data.frame(emetname = in_og_not_new,
                                   matrix(0,ncol = nrow(omicsNewdata$f_data),nrow = length(in_og_not_new)))
    emetCol = which(colnames(in_og_not_new_df) == "emetname")
    colnames(in_og_not_new_df)[emetCol] <- pmartR::get_emeta_cname(omicsNewdata)
    colnames(in_og_not_new_df)[-emetCol] <- colnames(pro_edata)[-emetCol]
    # add into old dataset
    pro_edata <- dplyr::bind_rows(pro_edata,in_og_not_new_df)
    
    # arrange them to match the original features
    pro_edata <- pro_edata[match(orig_names$names_orig,unlist(pro_edata[pmartR::get_emeta_cname(omicsNewdata)])),]
    
    # now set up new data to be ran with predictions
    new_info <- pro_edata[,-which(colnames(pro_edata) == pmartR::get_edata_cname(omicsNewdata))] %>%
      t() %>% data.frame()
    names(new_info) <- paste0("feature_",seq(from = 1, to = ncol(new_info)))
    
    # unique levels in the model
    unique_outcomes <- unlist(unique(omicsData$rds_model$full_model$pre$mold$outcomes))
    
    contains_exact_values <- function(column, values) {
      all(sort(column[!is.na(column)]) %in% (values))
    }
    
    # Apply the function to each column and find the matching column name
    matching_column <- colnames(omicsNewdata$f_data)[apply(omicsNewdata$f_data, 2, contains_exact_values, values = unique_outcomes)][1]
    
    fdata_sub <- omicsNewdata$f_data %>%
      dplyr::select(!!as.symbol(pmartR::get_fdata_cname(omicsNewdata)),
                    !!as.symbol(matching_column))
    
    new_info <- new_info %>%
      tibble::rownames_to_column(var = pmartR::get_fdata_cname(omicsNewdata)) %>%
      dplyr::left_join(fdata_sub,by = pmartR::get_fdata_cname(omicsNewdata))
    
    new_info <- new_info %>%
      dplyr::rename(response = !!as.symbol(matching_column)) %>%
      tibble::column_to_rownames(var = pmartR::get_fdata_cname(omicsNewdata))
    new_info[is.na(new_info)] <- 0
    
    
    # run predictions
    prediction_test_df <- tibble(response = factor(unlist(omicsNewdata$f_data[matching_column]),levels = unique_outcomes),
                                 .pred_class = unlist(predict(omicsData$rds_model$full_model$fit$fit,new_data = new_info))) %>%
      dplyr::bind_cols(data.frame(predict(omicsData$rds_model$full_model$fit$fit,new_data = new_info,type = "prob"))) %>%
      dplyr::mutate(`__SAMPNAMES__` = unlist(omicsNewdata$f_data[pmartR::get_fdata_cname(omicsNewdata)]))
    # full model already an slRes object
    full_model_test <- omicsData$rds_model$full_model
    full_model_train <- omicsData$rds_model$full_model
    
    attributes(full_model_train)$prediction_test <- attributes(full_model_train)$prediction_train
    # update it to match OOB estimates
    #attributes(full_model_train)$prediction_test$.pred_Mock <- full_model_train$fit$fit$fit$votes[,1]
    #attributes(full_model_train)$prediction_test$.pred_RhCMV <- full_model_train$fit$fit$fit$votes[,2]
    #attributes(full_model_train)$prediction_test$.pred_class <- ifelse(attributes(full_model_train)$prediction_test$.pred_Mock > 0.5,"Mock","RhCMV")
    
    # update test dataset too
    attributes(full_model_test)$prediction_test <- prediction_test_df
  
    omicsData$obj_predictions <- full_model_test
  })
  
  # log 2 plot
  output$transformation_scaling_plot <- renderPlot({
    req(!is.null(omicsData$obj_scaled))
    if(!is.null(attributes(omicsData$obj_scaled)$group_DF)){
      plot(omicsData$obj_scaled, use_VizSampNames = T,color_by = "Group")
    } else {
      plot(omicsData$obj_scaled, use_VizSampNames = T)
    }
  })
  
  # norm plot
  output$transformation_norm_plot <- renderPlot({
    req(!is.null(omicsData$obj_norm))
    if(!is.null(attributes(omicsData$obj_norm)$group_DF)){
      plot(omicsData$obj_norm, use_VizSampNames = T,color_by = "Group")
    } else {
      plot(omicsData$obj_norm, use_VizSampNames = T)
    }
  })
  
  # rollup plot
  output$transformation_rollup_plot <- renderPlot({
    req(!is.null(omicsData$obj_sl_rollup))
    if(!is.null(attributes(omicsData$obj_sl_rollup)$group_DF)){
      plot(omicsData$obj_sl_rollup, use_VizSampNames = T,color_by = "Group")
    } else {
      plot(omicsData$obj_sl_rollup, use_VizSampNames = T)
    }
  })
  
  output$predict_plot_ROC <- renderPlot({
    req(!is.null(omicsData$obj_predictions))
    plot(omicsData$obj_predictions, plotType = "roc_curve")
  })
  output$predict_plot_predictBar <- renderPlot({
    req(!is.null(omicsData$obj_predictions))
    plot(omicsData$obj_predictions, plotType = "prediction_bar")
  })
  output$predict_plot_confusionHeatmap <- renderPlot({
    req(!is.null(omicsData$obj_predictions))
    plot(omicsData$obj_predictions, plotType = "confusion_heatmap")
  })
  output$predict_plot_confidenceScatter <- renderPlot({
    req(!is.null(omicsData$obj_predictions))
    plot(omicsData$obj_predictions, plotType = "confidence_scatter")
  })
  
  output$download_processed_data <- downloadHandler(
    filename = function() {
      paste0("SLOPE_output_", gsub("( |:|-)", "_", Sys.time()), ".zip")
    },
    content = function(fname) {
      file.copy(zipped_file$fs, fname)
    },
    contentType = "application/zip"
  )
  
  # download tab
  output$Download_button <- renderUI({
    button <- downloadButton(
      "download_processed_data",
      tags$b("Download Bundle")
    )
    
    div(
      id = "js_downloadbutton",
      style = "margin-left:4px;float:left",
      class = "grey_button",
      button
    )
  })
  # BROWSER
  observeEvent(input$Browser,{
    browser()
  })
})