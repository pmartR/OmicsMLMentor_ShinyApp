
QC_rmd <- reactiveValues(res = NULL)

## Add disabling for 0 variance data and "not recommended" subtitle for low proportion missing
output$QC_rmd_metrics_UI <- renderUI({
  
  selected <- if(!is.null(isolate(input$QC_rmd_metrics))){
    isolate(input$QC_rmd_metrics)
  } else {
    if("pepData" %in% class(omicsData$objQC) || 
       "proData" %in% class(omicsData$objQC)){
      c("MAD", "Kurtosis", "Skewness", "Correlation", "Proportion_Missing"
      )
    } else {
      c("MAD", "Kurtosis", "Skewness", "Correlation")
    }
  }
  
  pickerInput(
    "QC_rmd_metrics",
    label = "Select metrics for distance criteria:", 
    choices = c(
      `Median Absolute Deviation` = "MAD", "Kurtosis" = "Kurtosis", 
      "Skewness"= "Skewness", "Correlation" = "Correlation", 
      `Proportion Missing` = "Proportion_Missing"
    ), multiple = T,
    selected = selected
  )
  
})

output$QC_rmdfilt_sample_select_UI <- renderUI({
  
  req(omicsData$objQC)
  
  temp_dat <- omicsData$objQC
  
  if(get_data_scale(temp_dat) == "abundance"){
    temp_dat <- edata_transform(temp_dat, "log2")
  }
  
  if(!is.null(omicsData$objQC$f_data)){
    temp_dat$f_data$Temp_col_all <- "All"
  } else {
    temp_dat$f_data <- data.frame(
      SampleId = colnames(temp_dat$e_data)[colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
      Temp_col_all = "All"
    )
  }
  temp_group <- group_designation(temp_dat, "Temp_col_all")
  
  rmd <- rmd_filter(temp_group, metrics = input$QC_rmd_metrics)
  
  QC_rmd$res <- rmd
  
  out_idN <- summary(rmd,
                     pvalue_threshold = input$QC_pvalue_threshold)$filt
  
  all_samps <- rmd[[1]]
  
  subtext <- rep("", length(all_samps))
  
  subtext[all_samps %in% out_idN] <- "Potential Outlier"
  
  all_samps <- all_samps[rev(order(subtext))]
  subtext <- subtext[rev(order(subtext))]
  
  
  selected <- if(!is.null(isolate(input$QC_rmdfilt_sample_select))){
    isolate(input$QC_rmdfilt_sample_select)
  } else if(any(all_samps %in% out_idN)) {
    all_samps[all_samps %in% out_idN]
  } else {
    NULL
  }
  
  pickerInput("QC_rmdfilt_sample_select",
              "Select samples to inspect:",
              choices = all_samps,
              multiple = T,
              choicesOpt = list(
                subtext = subtext
              ),
              options = list(`live-search`= T),
              selected = selected
  )
})

output$QC_rmdfilt_sample_remove_UI <- renderUI({
  
  req(QC_rmd$res)
  
  rmd <- QC_rmd$res
  
  # temp_dat <- omicsData$objQC
  # 
  # if(get_data_scale(temp_dat) == "abundance"){
  #   temp_dat <- edata_transform(temp_dat, "log2")
  # }
  # 
  # if(!is.null(omicsData$objQC$f_data)){
  #   temp_dat$f_data$Temp_col_all <- "All"
  # } else {
  #   temp_dat$f_data <- data.frame(
  #     SampleId = colnames(temp_dat$e_data)[colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
  #     Temp_col_all = "All"
  #   )
  # }
  # temp_group <- group_designation(temp_dat, "Temp_col_all")
  # 
  # rmd <- rmd_filter(temp_group, metrics = input$QC_rmd_metrics)
  
  out_idN <- summary(rmd, 
                     pvalue_threshold = input$QC_pvalue_threshold)$filt
  
  all_samps <- rmd[[1]]
  
  subtext <- rep("", length(all_samps))
  
  subtext[all_samps %in% out_idN] <- "Potential Outlier"
  
  all_samps <- all_samps[rev(order(subtext))]
  subtext <- subtext[rev(order(subtext))]
  
  pickerInput("QC_rmdfilt_sample_remove",
              "Select samples to remove from dataset:",
              choices = all_samps,
              multiple = T,
              choicesOpt = list(
                subtext = subtext
              ),
              options = list(`live-search`= T)
  )
})

output$rmd_plot_qc_all <- renderPlot({
  
  req(QC_rmd$res)
  
  plot(QC_rmd$res, pvalue_threshold = input$QC_pvalue_threshold) + 
    theme(legend.position = 0)
  
})

output$rmd_plot_qc_select <- renderPlot({
  
  req(QC_rmd$res)
  req(input$QC_rmdfilt_sample_select, cancelOutput = T)
  
  temp_dat <- omicsData$objQC

  if(get_data_scale(temp_dat) == "abundance"){
    temp_dat <- edata_transform(temp_dat, "log2")
  }

  if(!is.null(omicsData$objQC$f_data)){
    temp_dat$f_data$Temp_col_all <- "All"
  } else {
    temp_dat$f_data <- data.frame(
      SampleId = colnames(temp_dat$e_data)[colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
      Temp_col_all = "All"
    )
  }
  temp_group <- group_designation(temp_dat, "Temp_col_all")

  sampId <- input$QC_rmdfilt_sample_select

  sampId <- sampId[sampId %in% temp_group$f_data[[get_fdata_cname(temp_group)]]]

  req(length(sampId) > 0)
  
  plot(QC_rmd$res, sampleID = sampId)
  
})

## Make this iterative, so that these can be removed?
observeEvent(input$outliers_done, {
  
  if(!is.null(input$QC_rmdfilt_sample_remove)){
    
    temp_dat <- omicsData$objQC
    
    if(is.null(temp_dat$f_data)){
      temp_dat$f_data <- data.frame(
        SampleId = colnames(temp_dat$e_data)[colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
        Temp_col_all = "All"
      )
    }
    
    temp_dat <- applyFilt(
      custom_filter(temp_dat, 
                    f_data_remove = input$QC_rmdfilt_sample_remove), 
      temp_dat)
    
    if(is.null(temp_dat$f_data)){
      temp_dat$f_data <- NULL
    }
    
    omicsData$objQC <- temp_dat
  }
  
})

observeEvent(c(input$QC_rmd_metrics, input$QC_pvalue_threshold), {
  
  updateTabsetPanel(session, "QC_outlier_tabset", "all_out")
  
})

observeEvent(c(input$QC_rmdfilt_sample_select#, input$QC_rmdfilt_sample_remove
               ), {
  
  updateTabsetPanel(session, "QC_outlier_tabset", "inspect_samp")
  
})

observeEvent(input$all_outs_inspect_out, {
  
  rmd <- QC_rmd$res
  
  out_idN <- summary(rmd,
                     pvalue_threshold = input$QC_pvalue_threshold)$filt
  
  updatePickerInput(session, "QC_rmdfilt_sample_select", selected = out_idN)
  
})

observeEvent(input$all_outs_remove_out, {
  
  rmd <- QC_rmd$res
  
  out_idN <- summary(rmd,
                     pvalue_threshold = input$QC_pvalue_threshold)$filt
  
  updatePickerInput(session, "QC_rmdfilt_sample_remove", selected = out_idN)
  
})

observeEvent(input$all_outs_inspect_none, {
  
  updatePickerInput(session, "QC_rmdfilt_sample_select", selected = character(0))
  
})

observeEvent(input$all_outs_remove_none, {
  
  updatePickerInput(session, "QC_rmdfilt_sample_remove", selected = character(0))
  
})
