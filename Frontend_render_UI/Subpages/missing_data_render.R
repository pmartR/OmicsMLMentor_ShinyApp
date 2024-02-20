
## hist to detected

output$missing_data_hist_biomolecule <- renderPlot({
  req(!is.null(omicsData$objQC$e_data))
  
  id_col <- colnames(omicsData$objQC$e_data) %in% pmartR::get_edata_cname(omicsData$objQC)
  df <- omicsData$objQC$e_data[!id_col]
  
  apply_dir <- 1
  total <- ncol(df[-1])
  
  data <- data.frame(`Percentage missing` = apply(is.na(df[-1]), apply_dir, sum)/total*100, check.names = F)
  
  data$Handling <- "Unassigned"
  
  if("keep" %in% input$missing_options){
    
    rows <- Reduce("&", list(
      data[["Percentage missing"]] >= input$md_keep[1],
      data[["Percentage missing"]] <= input$md_keep[2]
    ))
    
    data$Handling[rows] <- "Keep"
    
  }
  
  if("impute" %in% input$missing_options){
    
    rows <- Reduce("&", list(
      data[["Percentage missing"]] >= input$md_impute[1],
      data[["Percentage missing"]] <= input$md_impute[2]
    ))
    
    data$Handling[rows] <- "Estimate"
    
  }
  
  if("convert" %in% input$missing_options){
    
    rows <- Reduce("&", list(
      data[["Percentage missing"]] >= input$md_convert[1],
      data[["Percentage missing"]] <= input$md_convert[2]
    ))
    
    data$Handling[rows] <- "Convert"
    
  }
  
  if("remove" %in% input$missing_options){
    
    rows <- Reduce("&", list(
      data[["Percentage missing"]] >= input$md_remove[1],
      data[["Percentage missing"]] <= input$md_remove[2]
    ))
    
    data$Handling[rows] <- "Remove"
    
  }
  
  text_ylab <- "biomolecules"
  
  ggplot(data, aes(x = `Percentage missing`, fill = Handling)) +
    geom_histogram() + theme_bw() + labs(y = paste0("Count of ", text_ylab))
  
})


output$missing_data_hist_sample <- renderPlot({
  req(!is.null(omicsData$objQC$e_data))
  
  temp_dat <- omicsData$objQC
  
  if(is.null(omicsData$objQC$f_data)){
    temp_dat$f_data <- data.frame(
      SampleId = colnames(temp_dat$e_data)[colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
      Temp_col_all = "All"
    )
  }
  
  mis_val <- missingval_result(temp_dat)
  
  mis_val_presence <- 1 - mis_val$na.by.sample$num_NA/
    (mis_val$na.by.sample$num_non_NA + mis_val$na.by.sample$num_NA)
  
  upper <- min(mean(mis_val_presence) + sd(mis_val_presence) * 2, 1)
  lower <- max(mean(mis_val_presence) - sd(mis_val_presence) * 2, 0)
  
  plot(missingval_result(temp_dat), temp_dat, 
       nonmissing = T, proportion = T)
  
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

# output$missing_data_hist_sample <- renderPlot({
#   req(!is.null(omicsData$objQC$e_data))
#   
#   total_biomolecule <- nrow(omicsData$objQC$e_data)
#   
#   temp_dat <- omicsData$objQC
#   
#   if(is.null(omicsData$objQC$f_data)){
#     temp_dat$f_data <- data.frame(
#       SampleId = colnames(temp_dat$e_data)[colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
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
  
  thresholds <- floor(100/splitter * 1:splitter)
  thresholds2 <- c(0, thresholds[-length(thresholds)])
  
  slides <- pmap(list(as.list(input$missing_options), as.list(thresholds), as.list(thresholds2)), 
                 function(lab, t1, t2){
                   
                   vals <- c(t2, t1)
                   
                   text <- ifelse(lab == "impute", "Estimate missing values",
                                  paste0(str_to_title(lab), " missing values"))
                   slide <- sliderInput(paste0("md_", lab), text, 
                               min = 0, max = 100, value = vals, round = T, 
                               width = "100%")
                   
                   if(input$keep_missing == "Yes") slide <- disabled(slide)
                   slide
                   
                 })
  
  tagList(slides)
  
})

## Fix slider relations
observeEvent(c(input$md_keep, input$md_impute, input$md_convert, input$md_remove), {
  
  ## Minimums
  ## Must always be zero if no other options
  min <- 0 ## Always 0
  min1 <- 0 
  min2 <- 0
  min3 <- 0 
  
  ## impute
  if(!is.null(input$md_keep) && 
     "keep" %in% input$missing_options) min1 <- max(input$md_keep)
  
  ## Convert
  if(!is.null(input$md_keep) && 
     "keep" %in% input$missing_options) min2 <- max(input$md_keep)
  if (!is.null(input$md_impute) && 
      "impute" %in% input$missing_options) min2 <- max(input$md_impute)
  
  ## Remove
  if(!is.null(input$md_keep) && 
     "keep" %in% input$missing_options) min3 <- max(input$md_keep)
  if(!is.null(input$md_impute) && 
     "impute" %in% input$missing_options) min3 <- max(input$md_impute)
  if (!is.null(input$md_convert) && 
      "convert" %in% input$missing_options) min3 <- max(input$md_convert)
  
  ## Maximums
  ## Must always be 100 if no other options
  max <- 100
  max1 <- 100 
  max2 <- 100
  max3 <- 100 ## Always 100
  
  ## Keep
  if (!is.null(input$md_remove) && 
      "remove" %in% input$missing_options) max <- min3
  if (!is.null(input$md_convert) && 
      "convert" %in% input$missing_options) max <- min2
  if (!is.null(input$md_impute) && 
      "impute" %in% input$missing_options) max <- min1
  
  ## Impute
  if (!is.null(input$md_remove) && 
      "remove" %in% input$missing_options) max1 <- min3
  if (!is.null(input$md_convert) && 
      "convert" %in% input$missing_options) max1 <- min2
  
  ## Convert
  if (!is.null(input$md_remove) && 
      "remove" %in% input$missing_options) max2 <- min3
  
  updateSliderInput(session, "md_keep", value = c(min, max))
  updateSliderInput(session, "md_impute", value = c(min1, max1))
  updateSliderInput(session, "md_convert", value = c(min2, max2))
  updateSliderInput(session, "md_remove", value = c(min3, max3))
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
        SampleId = colnames(temp_dat$e_data)[colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
        Temp_col_all = "All"
      )
    }
    
    
    res <- missingval_result(temp_dat)$na.by.sample
    rmv_fdata <- 1 - res$num_NA/nrow(temp_dat$e_data) < input$missing_value_thresh/100
    rmv <- res[rmv_fdata, 1]
    
    if(length(rmv) > 0){
      temp_dat <- applyFilt(
        custom_filter(temp_dat, 
                      f_data_remove = rmv), 
        temp_dat)
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