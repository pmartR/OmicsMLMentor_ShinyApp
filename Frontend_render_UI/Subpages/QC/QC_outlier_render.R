
QC_rmd <- reactiveValues(res = NULL)

output$outlier_collapse_UI <- renderUI({
  
  tabname <- "QC"
  
  if(!inherits(omicsData$obj, "seqData")){
    collapseBoxGroup(
      id = "QC_outlier_sidebar",
      # biomolecule filters
      
      collapseBox(
        value = "outlier_QC",
        collapsed = F,
        title = "Robust Mahalanobis distance criteria",
        fluidRow(
          column(
            12,
            div(
              id = paste0( tabname, "_rmd_metrics_js"), 
              style = "color:grey",
              class = "inline-wrapper-1",
              
              uiOutput("outlier_remove_advanced_pval"),
              
              
              ########## Update me laterrr ###############
              uiOutput(paste0( tabname, "_rmd_metrics_UI")),
              uiOutput(paste0( tabname, "_rmd_propmis_warn_icon_UI")),
              hidden(
                div(
                  id = paste0( tabname, "_rmd_novariance_warn_icon"),
                  icon(
                    "exclamation-sign", lib = "glyphicon", 
                    style="color:red;display:inline-block"
                  )
                )
              )
            )
          )
        )
      ),
      
      collapseBox(
        value = "outlier_QC_inspect",
        collapsed = F,
        
        title = "Inspect samples for removal",
        fluidRow(
          column(
            12,
            uiOutput(paste0( tabname, "_rmdfilt_sample_select_UI")),
            
            
            actionButton("all_outs_inspect_out", "Select all outliers for inspection", inline = T),
            actionButton("all_outs_inspect_none", "De-select all", inline = T),
            
            hr(),
            
            ## Remove samples
            uiOutput(paste0( tabname, "_rmdfilt_sample_remove_UI")),
            
            actionButton("all_outs_remove_out", "Select all outliers for removal", inline = T),
            actionButton("all_outs_remove_none", "De-select all", inline = T),
            
            textOutput("warn_rmdfilt_samples_remove")
          )
        ))
    )
  } else {
    collapseBoxGroup(
      id = "QC_outlier_sidebar",
      # biomolecule filters
      
      collapseBox(
        value = "outlier_QC",
        collapsed = F,
        title = "Outlier detection",
        fluidRow(
          column(
            12,
            br(),
            "Currently not supported for RNA-seq data, please click 'Confirm Selections' to continue.",
            br(), br()
              )
            )
          )
        )
  }
})

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
  
  out <- pickerInput(
    "QC_rmd_metrics",
    label = "Select metrics for distance criteria:", 
    choices = c(
      `Median Absolute Deviation` = "MAD", "Kurtosis" = "Kurtosis", 
      "Skewness"= "Skewness", "Correlation" = "Correlation", 
      `Proportion Missing` = "Proportion_Missing"
    ), multiple = T,
    selected = selected
  )
  
  if(input$user_level_pick == "beginner") out <- disabled(out)
  
  out
  
})

output$outlier_remove_advanced_pval <- renderUI({
  
  nm <- str_to_title(class(isolate(omicsData$objQC))[[1]])
  out <- numericInput(paste0("QC_pvalue_threshold"), 
                      "P-value threshold for outliers:", 0.0001, step = 0.0001, max = 1, min = 0)
  
  if(input$user_level_pick == "beginner") out <- disabled(out)

  out
})

output$QC_rmdfilt_sample_select_UI <- renderUI({
  
  req(!is.null(omicsData$objQC) && 
        !inherits(omicsData$objQC, "seqData") &&
        !is.null(input$outliers_done) && input$outliers_done < 1,
      cancelOutput = T
      )
  
  temp_dat <- omicsData$objQC
  
  if(get_data_scale(temp_dat) == "abundance" && 
     !inherits(omicsData$objMSU, "seqData")){
    temp_dat <- edata_transform(temp_dat, "log2")
  }
  
  if(!is.null(omicsData$objQC$f_data)){
    temp_dat$f_data$Temp_col_all <- "All"
  } else {
    temp_dat$f_data <- data.frame(
      SampleID = colnames(temp_dat$e_data)[
        colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
      Temp_col_all = "All"
    )
  }
  temp_group <- group_designation(temp_dat, "Temp_col_all")
  
  metrics <- if(input$user_level_pick == "beginner"){
    
    if("pepData" %in% class(omicsData$objQC) || 
       "proData" %in% class(omicsData$objQC)){
      c("MAD", "Kurtosis", "Skewness", "Correlation", "Proportion_Missing"
      )
    } else {
      c("MAD", "Kurtosis", "Skewness", "Correlation")
    }
    
  } else input$QC_rmd_metrics
  
  ### Metric hierarchy
  metrics_sort <- c("Kurtosis", "Skewness", "MAD", "Correlation", "Proportion_Missing")
  metrics_use <- metrics_sort[metrics_sort %in% metrics]
  
  tryCatch({
    rmd <- rmd_filter(temp_group, metrics = metrics_use)
  }, error = function(e){
    while(all(is.na(rmd$Log2.md)) && length(metrics_use) > 2){
      metrics_use <- metrics_use[-1]
      rmd <- rmd_filter(temp_group, metrics = metrics_use)
    }
  })
  
  while(all(is.na(rmd$Log2.md)) && length(metrics_use) > 2){
    metrics_use <- metrics_use[-1]
    rmd <- rmd_filter(temp_group, metrics = metrics_use)
  }
  
  if(!all(metrics %in% metrics_use) && input$user_level_pick != "beginner"){
    shinyalert("", 
               type = "warning",
               text = paste0(
                 "Too few samples detected in dataset for the number of metrics selected. The reduced list of the following metrics were used instead: ",
                 toString(metrics_use)
                 )
               
               )
  }
  
  
  QC_rmd$res <- rmd
  
  pval <- if(input$user_level_pick == "beginner") 0.0001 else input$QC_pvalue_threshold
  
  out_idN <- summary(rmd,
                     pvalue_threshold = pval)$filt
  
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

  pval <- if(input$user_level_pick == "beginner") 0.0001 else input$QC_pvalue_threshold
    
  out_idN <- summary(rmd, 
                     pvalue_threshold = pval)$filt
  
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

output$rmd_plot_qc_all <- renderPlotly({
  
  req(QC_rmd$res)
  
  pval <- if(input$user_level_pick == "beginner") 0.0001 else input$QC_pvalue_threshold
  
  p <- plot(QC_rmd$res, pvalue_threshold = pval)
  
  isolate(plot_table_current$table$QC__rmd_overall <- p)
  isolate(table_table_current$table$QC__rmd_table <- QC_rmd$res)
  
  p
  
})

output$rmd_plot_qc_select <- renderPlotly({
  
  req(QC_rmd$res)
  req(input$QC_rmdfilt_sample_select, cancelOutput = T)
  
  temp_dat <- omicsData$objQC

  if(get_data_scale(temp_dat) == "abundance" && 
     !inherits(omicsData$objMSU, "seqData")){
    temp_dat <- edata_transform(temp_dat, "log2")
  }

  if(!is.null(omicsData$objQC$f_data)){
    temp_dat$f_data$Temp_col_all <- "All"
  } else {
    temp_dat$f_data <- data.frame(
      SampleID = colnames(temp_dat$e_data)[
        colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
      Temp_col_all = "All"
    )
  }
  temp_group <- group_designation(temp_dat, "Temp_col_all")

  sampId <- input$QC_rmdfilt_sample_select

  sampId <- sampId[sampId %in% temp_group$f_data[[get_fdata_cname(temp_group)]]]

  req(length(sampId) > 0)
  
  p <- plot(QC_rmd$res, sampleID = sampId)
  
  isolate(plot_table_current$table$QC__rmd_outliers <- p)
  
  p
  
})

## Make this iterative, so that these can be removed?
observeEvent(input$outliers_done, {
  
  if(!is.null(input$QC_rmdfilt_sample_remove)){
    
    temp_dat <- omicsData$objQC
    
    if(is.null(temp_dat$f_data)){
      temp_dat$f_data <- data.frame(
        SampleID = colnames(temp_dat$e_data)[
          colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
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
    
    omicsData$objQC <- auto_remove_na(temp_dat)
  }
  
})

observeEvent(c(input$QC_rmd_metrics, input$QC_pvalue_threshold), {
  
  updateTabsetPanel(session, "QC_outlier_tabset", "all_out")
  
})

observeEvent(c(input$QC_rmdfilt_sample_select#, input$QC_rmdfilt_sample_remove
               ), {
  
  updateTabsetPanel(session, "QC_outlier_tabset", "inspect_samp")
  
})

observeEvent(input$QC_rmdfilt_sample_remove, {
  if (length(omicsData$objQC$f_data[[get_fdata_cname(omicsData$objQC)]]) - length(input$QC_rmdfilt_sample_remove) < 6) {
    output$warn_rmdfilt_samples_remove <- renderText("Too many samples were removed. Please ensure at least 6 samples are present.")
    disable("outliers_done")
  } else {
    output$warn_rmdfilt_samples_remove <- renderText("")
    enable("outliers_done")
  }
})

observeEvent(input$all_outs_inspect_out, {
  
  rmd <- QC_rmd$res
  
  pval <- if(input$user_level_pick == "beginner") 0.0001 else input$QC_pvalue_threshold
  
  
  out_idN <- summary(rmd,
                     pvalue_threshold = pval)$filt
  
  updatePickerInput(session, "QC_rmdfilt_sample_select", selected = out_idN)
  
})

observeEvent(input$all_outs_remove_out, {
  
  rmd <- QC_rmd$res
  
  pval <- if(input$user_level_pick == "beginner") 0.0001 else input$QC_pvalue_threshold
  
  out_idN <- summary(rmd,
                     pvalue_threshold = pval)$filt
  
  updatePickerInput(session, "QC_rmdfilt_sample_remove", selected = out_idN)
  
})

observeEvent(input$all_outs_inspect_none, {
  
  updatePickerInput(session, "QC_rmdfilt_sample_select", selected = character(0))
  
})

observeEvent(input$all_outs_remove_none, {
  
  updatePickerInput(session, "QC_rmdfilt_sample_remove", selected = character(0))
  
})
