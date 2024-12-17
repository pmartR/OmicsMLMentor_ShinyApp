
output$missing_options_UI <- renderUI({
  
  if(inherits(omicsData$objQC, "proData")){
    subtext <- c(
      "Estimation of values must be at peptide level data for proteomics data.", 
      "", "")
    disabled <- c(T, F, F)
  } else {
    disabled <- NULL
    subtext <- NULL
  }
  
  pickerInput(
    "missing_options",
    "Preview biomolecule incomplete detection handling:",
    choices = c(
      # "Keep data as-is" = "keep",
      "🟩 Estimate values in samples with no biomolecule detection" = "impute",
      "🟧 Convert undetected biomolcule values to 0, all other values to 1" = "convert",
      "🟥 Remove biomolecules with incomplete detection" = "remove"
    ),
    choicesOpt = list(subtext = subtext, disabled = disabled),
    multiple = T
  )
  
})

## Prevent stupid
observeEvent(input$missing_value_thresh, {
  req(!is.null(input$missing_value_thresh) && !is.na(input$missing_value_thresh))
  if(input$missing_value_thresh > 100){
    updateNumericInput(session, "missing_value_thresh", value = 100)
  } else if (input$missing_value_thresh < 0){
    updateNumericInput(session, "missing_value_thresh", value = 0)
  }
})

# change individual slider values into individual thresholds
missingHandleSliderVals_ <- reactive({
  thresholds <- list(
    md_keep = NULL,
    md_impute = NULL,
    md_convert = NULL,
    md_remove = NULL
  )
  
  if (input$keep_missing == "Yes") {
    thresholds$md_keep <- c(0, 100)
    # keep
    return(thresholds)
  }
  
  if ("impute" %in% input$missing_options) {
    if (length(input$missing_options) == 1) {
      thresholds$md_impute <- c(0, 100)
      # impute
      return(thresholds)
    }
    
    thresholds$md_impute <- c(0, round(input$missingness_handle_slider[1]))
    
    if ("convert" %in% input$missing_options) {
      thresholds$md_convert <- c(round(input$missingness_handle_slider[1]), 100)
      
      if ("remove" %in% input$missing_options) {
        thresholds$md_convert[2] <- round(input$missingness_handle_slider[2])
        thresholds$md_remove <- c(round(input$missingness_handle_slider[2]), 100)
      }
      
      # impute, convert, [remove]
      return(thresholds)
    }
    
    thresholds$md_remove <- c(round(input$missingness_handle_slider[1]), 100)
    # impute, remove
    return(thresholds)
  }
  
  if ("convert" %in% input$missing_options) {
    if (length(input$missing_options) == 1) {
      thresholds$md_convert <- c(0, 100)
      # convert
      return(thresholds)
    }
    
    thresholds$md_convert <- c(0, round(input$missingness_handle_slider[1]))
    thresholds$md_remove <- c(round(input$missingness_handle_slider[1]), 100)
    # convert, remove
    return(thresholds)
  }
  
  if ("remove" %in% input$missing_options) {
    thresholds$md_remove <- c(0, 100)
  }
  
  # none, [remove]
  return(thresholds)
})

missingHandleSliderVals <- missingHandleSliderVals_ %>% debounce(500)

## hist to detected

output$missing_data_hist_biomolecule <- renderPlotly({
  req(!is.null(omicsData$objQC$e_data))
  
  omics_obj <- omicsData$objQC
  
  if (inherits(omicsData$objQC, "pepData")) {
    omics_obj <- pepQCData$objQCPro
  }
  
    thresholds <- list(
      keep = missingHandleSliderVals()$md_keep,
      impute = missingHandleSliderVals()$md_impute,
      convert = missingHandleSliderVals()$md_convert,
      remove = missingHandleSliderVals()$md_remove
    )
    
    data_use <- slopeR::get_transform_df(omics_obj, thresholds)
    
    req(!all(data_use$Handling == "Remove"))
 
    
    text_ylab <- "biomolecules"
    
    p <- ggplot(data_use, aes(x = `Percentage missing`, fill = Handling)) +
      geom_histogram() + theme_bw() + 
      labs(y = paste0("Count of ", text_ylab)) + xlim(c(-1,100)) +
      scale_fill_manual(values = 
                          c("Convert" = "#c87619", 
                            "Estimate" = "#238551", 
                            "Remove" ="#cd4246"))
    
    if (inherits(omicsData$objQC, "pepData")) {
      p <- p + ggtitle("Protein level preview")
    }
    
    isolate(plot_table_current$table$QC__missing_features <- p)
    
    return(p)

})

output$qc_biomolecule_title <- renderText({
  if (inherits(omicsData$objQC, "pepData")) 
    "Protein-level Information" 
  else 
    "Detection by Biomolecule"
})

output$missing_data_hist_sample <- renderPlotly({
  req(!is.null(omicsData$objQC$e_data))
  
  temp_dat <- omicsData$objQC
  
  if(is.null(omicsData$objQC$f_data)){
    temp_dat$f_data <- data.frame(
      SampleID = colnames(temp_dat$e_data)[
        colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
      Temp_col_all = "All"
    )
  }
  
  mis_val <- missingval_result(temp_dat)
  
  mis_val_presence <- 1 - mis_val$na.by.sample$num_NA/
    (mis_val$na.by.sample$num_non_NA + mis_val$na.by.sample$num_NA)
  
  upper <- min(mean(mis_val_presence) + sd(mis_val_presence) * 2, 1)
  lower <- max(mean(mis_val_presence) - sd(mis_val_presence) * 2, 0)
  
  p <- plot(missingval_result(temp_dat), temp_dat, 
       nonmissing = T, proportion = T, display_count = input$missing_data_hist_sample_prop)
  
  # if (!input$missing_data_hist_sample_names) {
  #   p <- p + theme(axis.title.x=element_blank(),
  #                  axis.text.x=element_blank(),
  #                  axis.ticks.x=element_blank())
  # }
  
  isolate(plot_table_current$table$QC__missing_samples <- p)
  isolate(table_table_current$table$QC__missing_samples <- missingval_result(temp_dat)[[1]])
  isolate(table_table_current$table$QC__missing_features <- missingval_result(temp_dat)[[2]])
  
  p
  
  # if(lower < .9){
  #   p + 
  #     ggplot2::geom_hline(ggplot2::aes(yintercept = upper, linetype = "2 std upper bound"), show_guide = F) + 
  #     ggplot2::geom_hline(ggplot2::aes(yintercept = lower, linetype = "2 std lower bound"), 
  #                         show_guide = T) +
  #     ggplot2::scale_linetype_manual(name= c(""), 
  #                                    values = c("2 std upper bound" = "dashed",
  #                                               "2 std lower bound" = "dashed"))
  # } else p
  
})


output$slider_options_ui <- renderUI({
  
  splitter <- length(input$missing_options)
  req(splitter > 0)
  
  sliders <- list()
  if ("impute" %in% input$missing_options &&
      "convert" %in% input$missing_options)
  {
    sliders <- append(sliders, list(
      list(value = ifelse("remove" %in% input$missing_options, 33, 50),
           intentAfter = "warning", intentBefore = "success")
    ))
  }
  if ("convert" %in% input$missing_options &&
      "remove" %in% input$missing_options) {
    sliders <- append(sliders, list(
      list(value = ifelse("impute" %in% input$missing_options, 66, 50),
           intentBefore = "warning", intentAfter = "danger")
    ))
  }
  if ("impute" %in% input$missing_options &&
      "remove" %in% input$missing_options &&
      !"convert" %in% input$missing_options) {
    sliders <- append(sliders, list(
      list(value = 50, intentAfter = "danger", intentBefore = "success")
    ))
  }
  
  out <- div(
    column(1, "  "),
    column(9, MultiSlider.shinyInput(
      "missingness_handle_slider",
      values = sliders,
      min = 0,
      max = 100.00000001,
      labelStepSize = 25
    )
    # uiOutput("missingness_handle_legend")
    ),
    column(2, "  "),

    column(12, 
           br(),
           strong("Use slider above to determine thresholds for each handling method.")
    )
    
  )
  
  if(inherits(omicsData$objQC, "pepData") && !is.null(pepQCData$pepQCData)){
    
    out <- hidden(out)
    
  }
  
  out
  
})


output$missing_data_sample_picker_UI <- renderUI({
  
  req(!is.null(omicsData$objQC$e_data))
  
  choices <- colnames(omicsData$objQC$e_data)
  
  choices <- choices[choices != get_edata_cname(omicsData)]
  
  pickerInput(
    "missing_data_sample_picker",
    choices = choices,
    multiple = T,
    options = list(`live-search` = TRUE, `actions-box` = TRUE)
    
  )
  
  
})


observeEvent(omicsData$objQC, {
  
  if(!any(is.na(omicsData$objQC$e_data))){
    
    do.call(updateRadioGroupButtons, list(
      session = session,
      inputId = "keep_missing",
      selected = "Yes"
    ))
    
    do.call(
      updatePickerInput, list(
        session = session,
        inputId = "missing_options",
        selected = "keep"
      )
    )
    
    disable("missing_options")
  }
})

output$Note_nonmissing <- renderText({
  if(!any(is.na(omicsData$objQC$e_data))){
    
    ## Works better here than above observer oddly enough
    do.call(
      disable,
      list(id = "keep_missing")
    )
    
    "Selections have been disabled, as all biomolecule observations are complete. No further action is needed on this page."
  } else {
    ""
  }
})

observeEvent(input$keep_missing, {
  
  req(!is.null(omicsData$objQC))
  
  if(inherits(omicsData$objQC, "seqData")){
    updateRadioGroupButtons(session, "keep_missing", selected = "Yes")
  }
}, priority = 1)

observeEvent(input$keep_missing, {
  
  req(!is.null(omicsData$objQC))
  
  if(inherits(omicsData$objQC, "seqData")){
    disable("keep_missing")
  }
})

observeEvent(input$keep_missing, {
  if(input$keep_missing == "Yes"){
    shinyjs::hide("missing_options")
  } else {
    shinyjs::show("missing_options")
  }
})

observeEvent(input$done_sample_miss, {
  if(input$done_sample_miss > 0){
    
    
    temp_dat <- omicsData$objQC
    thresh <- input$missing_value_thresh
    if(is.na(thresh)) thresh <- 0
    
    if(is.null(temp_dat$f_data)){
      temp_dat$f_data <- data.frame(
        SampleID = colnames(temp_dat$e_data)[
          colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
        Temp_col_all = "All"
      )
    }
    
    if(inherits(temp_dat, "seqData")){
      res <- missingval_result(temp_dat)$zeros.by.sample
      rmv_fdata <- 1 - res$num_zeros/nrow(temp_dat$e_data) < thresh/100
    } else {
      res <- missingval_result(temp_dat)$na.by.sample
      rmv_fdata <- 1 - res$num_NA/nrow(temp_dat$e_data) < thresh/100
    }
    rmv <- res[rmv_fdata, 1]
    
    tryCatch({
      if(length(rmv) > 0){
        sample_count <- length(temp_dat$f_data[[get_fdata_cname(temp_dat)]])
        
        if (sample_count - length(rmv) < 6) {
          stop("The applied filter removes too many samples. Minimum of 6 samples required, ", sample_count - length(rmv), " remaining.")
        }
        
        temp_dat <- applyFilt(
          custom_filter(temp_dat, 
                        f_data_remove = rmv), 
          temp_dat)
      }
      errorOccurred <- NULL
    }, error = function(e) {
      errorOccurred <<- e
    })
    
    # needs to happen outside trycatch otherwise execution continues
    if (!is.null(errorOccurred)) {
      shinyalert("Something went wrong:", paste0("System error: ", errorOccurred$message))
      return(NULL)
    }
    
    if(is.null(omicsData$objQC$f_data)){
      temp_dat$f_data <- NULL
    }
    
    omicsData$objQC <- auto_remove_na(temp_dat)
    
    updateBoxCollapse(session, "missing_data_box", 
                      close = "missing_data_sample_box", 
                      open = "missing_by_biomolecule")
    
    updateBoxCollapse(session, "qc_missing_plots", 
                      close = "missing_data_sample_plot", 
                      open = "missing_data_biomolecule_plot")
    
    if (inherits(omicsData$objQC, "pepData")) {
      shinyjs::show("protein_rollup_box")
    } else {
      shinyjs::show("qc_biomolecule_detect")
      shinyjs::show("qc_biomolecule_detect_plot")
    }
    
  }
})

observeEvent(input$qc_apply_rollup, {
  req(input$qc_apply_rollup > 0 && 
        !is.null(input$qc_which_rollup) && 
        !is.null(input$qc_which_combine_fn))
  
  shinyjs::show("qc_rollup_busy")
  if (input$qc_which_rollup == "zrollup") {
    single_pep <- TRUE
    single_observation <- TRUE
  } else {
    single_pep <- FALSE
    single_observation <- FALSE
  }
  unregister()
  
  temp_dat <- omicsData$objQC
  
  if(get_data_scale(temp_dat) == "abundance"){
    temp_dat <- edata_transform(temp_dat, "log2")
  }

  if(is.null(temp_dat$f_data)){
    temp_dat$f_data <- data.frame(
      SampleID = colnames(temp_dat$e_data)[
        colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
      Temp_col_all = "All"
    )
  }
  
  
  pepQCData$objQCPro <- protein_quant(temp_dat,
                                      method = input$qc_which_rollup,
                                      qrollup_thresh = input$qc_qrollup_thresh / 100,
                                      single_pep = single_pep,
                                      single_observation = single_observation,
                                      combine_fn = input$qc_which_combine_fn,
                                      parallel = TRUE
  )
  unregister()
  shinyjs::hide("qc_rollup_busy")
  shinyjs::show("qc_biomolecule_detect")
  shinyjs::show("qc_biomolecule_detect_plot")
})

output$qc_rollup_res_UI <- renderUI({
  if (!is.null(pepQCData$objQCPro)) {
    p <- plotlyOutput(paste0("qc_rollup_res"))
  } else {
    p <- "Roll-up has not been applied."
  }
  return(p)
})

output$qc_rollup_res <- renderPlotly({
  plot(pepQCData$objQCPro)
})

observeEvent(input$done_qc_rollup, {
  updateBoxCollapse(session, "missing_data_box", 
                    close = "protein_rollup",
                    open = "missing_by_biomolecule")
  updateBoxCollapse(session, "qc_missing_plots",
                    close = "qc_rollup_res",
                    open = "missing_data_biomolecule_plot")
})

observeEvent(c(input$keep_missing, input$missing_options, missingHandleSliderVals()), {
  
  req(!inherits(omicsData$objQC, "seqData"))
  
  if ((!is.null(input$keep_missing) && input$keep_missing == "Yes") || 
      !is.null(input$missing_options)) {
    # Prevent user from Removing all biomolecules
    
    # Sys.sleep(0.5) ## I think the debounce covers this

    isolate(try({

      thresholds <- list(
        keep = missingHandleSliderVals()$md_keep,
        impute = missingHandleSliderVals()$md_impute,
        convert = missingHandleSliderVals()$md_convert,
        remove = missingHandleSliderVals()$md_remove
      )
  
      if (inherits(omicsData$objQC, "pepData")) {
        transform_df <- slopeR::get_transform_df(pepQCData$objQCPro, thresholds)
      } else {
        transform_df <- slopeR::get_transform_df(omicsData$objQC, thresholds)
      }
      
      if (all(transform_df$Handling == "Remove")) {
        output$warn_missing_biom <- renderText("All biomolecules would be removed with the specified handling. Please ensure at least one biomolecule is kept.")
        shinyjs::disable("done_biom_miss")
      } else {
        output$warn_missing_biom <- renderText("")
        shinyjs::enable("done_biom_miss")
      }
    }))
    
  } else {
    output$warn_missing_biom <- renderText("")
    shinyjs::disable("done_biom_miss")
  }
}, ignoreNULL = FALSE)

observeEvent(input$done_biom_miss, {
  if(input$done_biom_miss > 0){
    
    # if (inherits(omicsData$objQC, "pepData")) {
    #   thresholds <- list(
    #     keep = missingHandleSliderVals()$md_keep,
    #     impute = missingHandleSliderVals()$md_impute,
    #     convert = missingHandleSliderVals()$md_convert,
    #     remove = missingHandleSliderVals()$md_remove
    #   )
    #   
    #   pepQCData$transforms_df <- slopeR::get_transform_df(pepQCData$objQCPro, thresholds)
    # }
    
    updateBoxCollapse(session, "missing_data_box", 
                      close = "missing_by_biomolecule")
    
    updateBoxCollapse(session, "qc_missing_plots", 
                      close = "missing_data_biomolecule_plot")
    
    shinyjs::show("done_md")
  }
})
