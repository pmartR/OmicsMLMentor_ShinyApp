
## render

output$transform_picker_UI <- renderUI({
  
  if(inherits(omicsData$objQC, "seqData")){
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
  
  choices <- choices[!(choices %in% get_data_scale(omicsData$objQC))]

  if(input$user_level_pick == "beginner"){
    
    rna_ds <- attr(omicsData$obj, "data_info")$data_scale_actual
    
    set <-  if(!is.null(rna_ds) && 
               rna_ds %in% c("upper", "median", "lcpm")){
      "No transformation"
    } else if(get_data_scale(omicsData$objQC) == "log2"){
      "No transformation"
    } else if (!is.null(rna_ds) && rna_ds == "counts"){
      "lcpm"
    } else "log2"
    
    out <- disabled(pickerInput("transform", "Transform data to:", 
                       choices = c(choices, "No transformation"),
                       selected = set))
    
  } else {
    out <- pickerInput("transform", 
                       "Transform data to:", 
                       choices = c(choices, "No transformation"))
  }
  
  div(
    out,
    text_warn,
    br(),
    br()
  )
  
})

output$transform_preview_plot_render <- renderUI({
  if (isTruthy(input$transform_preview_plot_load_button) || dim(omicsData$objPP$e_data)[1] < 20000) {
    withSpinner(plotlyOutput("transform_preview_plot"))
  } else {
    div(
      "This plot is large and may take a while to render.",
      actionButton("transform_preview_plot_load_button", "Show plot")
    )
  }
})

output$transform_preview_plot <- renderPlotly({
  
  # req(!inherits(omicsData$objMSU, "seqData"))
  
  out <- omicsData$objMSU
  
  if(inherits(omicsData$objMSU, "seqData")){
    
    x <- edata_transform_seq(out, input$transform)
    
    ## Error popping up here, not sure why
    # if(!is.null(get_group_DF(out))){
      # p <- plot(omicsData = x,
      #           color_by = "Group", order_by = "Group")
    # } else {
      p <- plot(x)
    # }
    
    yaxis <- switch(get_data_info(x)$data_scale_actual,
                    lcpm = "Log counts per million",
                    upper = "Upper-quantile transformed counts",
                    median = "Median counts")
    p <- p + labs(y = yaxis)
    
  } else {
    if(input$transform != get_data_scale(omicsData$objMSU) && 
       !is.null(input$transform) && 
       input$transform != "No transformation"){
      out <- edata_transform(out, input$transform)
    }
    
    if(!is.null(get_group_DF(out))){
      p <- plot(out, color_by = "Group", order_by = "Group")
    } else {
      p <- plot(out)
    }
  }
  
  isolate(plot_table_current$table$PP__transform <- p)
  isolate(table_table_current$table$PP__transform <- out$e_data)
  
  p
  
})

observeEvent(input$done_tr_box, {
  
  updateBoxCollapse(session, id = "transform_collapse", close = "transformation")
  shinyjs::show("complete_transform")
  
})

edata_transform_seq <- function(OD, transformation){
  
  temp_data <- OD$e_data
  iCol <- which(names(OD$e_data) == get_edata_cname(OD))
  
  transform_data <- temp_data[, -iCol]
  if (transformation == "lcpm") {
    samp_sum <- apply(transform_data, 2, sum, na.rm = TRUE) + 
      1
    div_sum <- sweep((transform_data + 0.5), 2, samp_sum, 
                     `/`)
    temp_data[, -iCol] <- log2(div_sum * 10^6)
  } else if (transformation == "upper") {
    warning("Zeros will be regarded as NA for 'upper' transformation")
    transform_data[transform_data == 0] <- NA
    samp_upper <- apply(transform_data, 2, quantile, 
                        na.rm = TRUE, probs = 0.75)
    g.q <- quantile(unlist(transform_data), probs = 0.75, 
                    na.rm = TRUE)
    div_75 <- sweep(transform_data, 2, samp_upper, `/`)
    temp_data[, -iCol] <- div_75 * g.q
  } else if (transformation == "median") {
    warning("Zeros will be regarded as NA for 'median' transformation")
    transform_data[transform_data == 0] <- NA
    samp_med <- apply(transform_data, 2, median, na.rm = TRUE)
    div_med <- sweep(transform_data, 2, samp_med, `/`)
    g.q <- median(unlist(transform_data), na.rm = TRUE)
    temp_data[, -iCol] <- div_med * g.q
  }
  
  OD$e_data <- temp_data
  
  ## Plots get upset if not counts
  attr(OD, "data_info")$data_scale_actual <- transformation
  OD

}


observeEvent(input$complete_transform, {
  
  req(!is.null(omicsData$objMSU) &&
        input$complete_transform > 0 && 
        !is.null(input$transform) && 
        input$transform != "No transformation" &&
        input$transform != get_data_scale(omicsData$objMSU))
  
  ## Call from previous so they can redo as they like
  if(!inherits(omicsData$objMSU, "seqData")){
    omicsData$objPP <- edata_transform(omicsData$objMSU, input$transform)
  } else {
    # This has to happen after filters
    # omicsData$objPP <- edata_transform_seq(omicsData$objMSU, input$transform)
  }
  
})

observeEvent(input$complete_filters, {
  
  req(!is.null(omicsData$objMSU) &&
        input$complete_transform > 0 && 
        !is.null(input$transform) && 
        input$transform != "No transformation" &&
        input$transform != get_data_scale(omicsData$objMSU))
  
  if(inherits(omicsData$objMSU, "seqData")){
    # This has to happen after filters
    omicsData$objPP <- edata_transform_seq(omicsData$objPP, input$transform)
  }
  
})

