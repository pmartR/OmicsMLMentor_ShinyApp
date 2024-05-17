
# change individual slider values into individual thresholds
missingHandleSliderVals <- reactive({
  thresholds <- list(
    md_keep = NULL,
    md_impute = NULL,
    md_convert = NULL,
    md_remove = NULL
  )
  
  if ("keep" %in% input$missing_options) {
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
    
    thresholds$md_impute <- c(0, input$missingness_handle_slider[1])
    
    if ("convert" %in% input$missing_options) {
      thresholds$md_convert <- c(input$missingness_handle_slider[1], 100)
      
      if ("remove" %in% input$missing_options) {
        thresholds$md_convert[2] <- input$missingness_handle_slider[2]
        thresholds$md_remove <- c(input$missingness_handle_slider[2], 100)
      }
      
      # impute, convert, [remove]
      return(thresholds)
    }
    
    thresholds$md_remove <- c(input$missingness_handle_slider[1], 100)
    # impute, remove
    return(thresholds)
  }
  
  if ("convert" %in% input$missing_options) {
    if (length(input$missing_options) == 1) {
      thresholds$md_convert <- c(0, 100)
      # convert
      return(thresholds)
    }
    
    thresholds$md_convert <- c(0, input$missingness_handle_slider[1])
    thresholds$md_remove <- c(input$missingness_handle_slider[1], 100)
    # convert, remove
    return(thresholds)
  }
  
  if ("remove" %in% input$missing_options) {
    thresholds$md_remove <- c(0, 100)
  }
  
  # none, [remove]
  return(thresholds)
})

## hist to detected

output$missing_data_hist_biomolecule <- renderPlotly({
  req(!is.null(omicsData$objQC$e_data))
  
  id_col <- colnames(omicsData$objQC$e_data) %in% pmartR::get_edata_cname(omicsData$objQC)
  df <- omicsData$objQC$e_data[!id_col]
  
  apply_dir <- 1
  total <- ncol(df[-1])
  
  data <- data.frame(`Percentage missing` = 
                       apply(is.na(df[-1]), apply_dir, sum)/total*100, 
                     check.names = F)
  
  data <- cbind(df[1], data)
  
  data <- arrange(data, `Percentage missing`)
  
  data$Handling <- "Unassigned"
  
  sliderVals <- missingHandleSliderVals %>% debounce(500)
  
  if("keep" %in% input$missing_options){
    
    rows <- Reduce("&", list(
      data[["Percentage missing"]] >= sliderVals()$md_keep[1],
      data[["Percentage missing"]] <= sliderVals()$md_keep[2]
    ))
    
    data$Handling[rows] <- "Keep"
    
  }
  
  if("impute" %in% input$missing_options){
    
    rows <- Reduce("&", list(
      data[["Percentage missing"]] >= sliderVals()$md_impute[1],
      data[["Percentage missing"]] <= sliderVals()$md_impute[2]
    ))
    
    data$Handling[rows] <- "Estimate"
    
  }
  
  if("convert" %in% input$missing_options){
    
    rows <- Reduce("&", list(
      data[["Percentage missing"]] >= sliderVals()$md_convert[1],
      data[["Percentage missing"]] <= sliderVals()$md_convert[2]
    ))
    
    data$Handling[rows] <- "Convert"
    
  }
  
  if("remove" %in% input$missing_options){
    
    rows <- Reduce("&", list(
      data[["Percentage missing"]] >= sliderVals()$md_remove[1],
      data[["Percentage missing"]] <= sliderVals()$md_remove[2]
    ))
    
    data$Handling[rows] <- "Remove"
    
  }
  
  text_ylab <- "biomolecules"
  
  p <- ggplot(data, aes(x = `Percentage missing`, fill = Handling)) +
    geom_histogram() + theme_bw() + labs(y = paste0("Count of ", text_ylab))
  
  isolate(plot_table_current$QC$missing_features <- p)
  
  p
  
})


output$missing_data_hist_sample <- renderPlotly({
  req(!is.null(omicsData$objQC$e_data))
  
  temp_dat <- omicsData$objQC
  
  if(is.null(omicsData$objQC$f_data)){
    temp_dat$f_data <- data.frame(
      SampleID = colnames(temp_dat$e_data)[colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
      Temp_col_all = "All"
    )
  }
  
  mis_val <- missingval_result(temp_dat)
  
  mis_val_presence <- 1 - mis_val$na.by.sample$num_NA/
    (mis_val$na.by.sample$num_non_NA + mis_val$na.by.sample$num_NA)
  
  upper <- min(mean(mis_val_presence) + sd(mis_val_presence) * 2, 1)
  lower <- max(mean(mis_val_presence) - sd(mis_val_presence) * 2, 0)
  
  p <- plot(missingval_result(temp_dat), temp_dat, 
       nonmissing = T, proportion = T)
  
  isolate(plot_table_current$QC$missing_samples <- p)
  isolate(table_table_current$QC$missing_samples <- missingval_result(temp_dat)[[1]])
  isolate(table_table_current$QC$missing_features <- missingval_result(temp_dat)[[2]])
  
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

# output$missing_data_hist_sample <- renderPlotly({
#   req(!is.null(omicsData$objQC$e_data))
#   
#   total_biomolecule <- nrow(omicsData$objQC$e_data)
#   
#   temp_dat <- omicsData$objQC
#   
#   if(is.null(omicsData$objQC$f_data)){
#     temp_dat$f_data <- data.frame(
#       SampleID = colnames(temp_dat$e_data)[colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
#       Temp_col_all = "All"
#     )
#   }
#   
#   res <- missingval_result(temp_dat)
#   
#   df <- res$na.by.sample
#   
#   df <- arrange(df, num_NA)
#   
#   df[[1]] <- factor(as.character(df[[1]]), 
#                     levels = unique(as.character(df[[1]])))
#   
#   df$Percentage <- df$num_NA/total_biomolecule*100
#   
#   ggplot(df, aes(x = !!rlang::sym(colnames(df)[1]), 
#                  y = 100-Percentage)) + 
#     # geom_text(label = paste0(round(df$Percentage, 3), "%"), ) +
#     geom_col() + theme_bw() + 
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
#     lims(y = c(0,100)) + labs(y = "Percentage of all observed biomolecules detected", x = "")
#   
#   # plot(missingval_result(omicsData$objQC), omicsData$objQC)
#   
# })


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
  
  MultiSlider.shinyInput(
    "missingness_handle_slider",
    values = sliders,
    min = 0,
    max = 100,
    labelStepSize = 10
  )
  
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
    
    "Selections have been disabled, as all biomolecule observations are complete. No further action is needed on this page"
  } else {
    ""
  }
})

observeEvent(input$keep_missing, {
  if(input$keep_missing == "Yes"){
    updatePickerInput(session, "missing_options", selected = "keep")
  }
})

observeEvent(input$done_sample_miss, {
  if(input$done_sample_miss > 0){
    
    
    temp_dat <- omicsData$objQC
    
    if(is.null(temp_dat$f_data)){
      temp_dat$f_data <- data.frame(
        SampleID = colnames(temp_dat$e_data)[colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
        Temp_col_all = "All"
      )
    }
    
    
    res <- missingval_result(temp_dat)$na.by.sample
    rmv_fdata <- 1 - res$num_NA/nrow(temp_dat$e_data) < input$missing_value_thresh/100
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
    
    omicsData$objQC <- temp_dat
    
    
    
    updateBoxCollapse(session, "missing_data_box", 
                      close = "missing_data_sample_box", 
                      open = "missing_by_biomolecule")
    
    updateBoxCollapse(session, "qc_missing_plots", 
                      close = "missing_data_sample_plot", 
                      open = "missing_data_biomolecule_plot")
    
  }
})

observeEvent(input$done_biom_miss, {
  if(input$done_biom_miss > 0){
    updateBoxCollapse(session, "missing_data_box", 
                      close = "missing_by_biomolecule")
    
    updateBoxCollapse(session, "qc_missing_plots", 
                      close = "missing_data_biomolecule_plot")
    
    shinyjs::show("done_md")
  }
})
