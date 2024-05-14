
## render

output$transform_picker_UI <- renderUI({
  
  if(inherits(omicsData$objQC, "seqData")){
    return("Data scaling and transformation is not appropriate for count-based RNA-seq data.")
  }
  
  ## Disable for seqdata
  choices <- list("Raw intensity" = "abundance", 
                  "Log base 2" = "log2", 
                  "Log base 10" = "log10", 
                  "Natural log" = "log")
  
  choices <- choices[!(choices %in% get_data_scale(omicsData$objQC))]

  if(input$user_level_pick == "beginner"){
    
    set <- if(get_data_scale(omicsData$objQC) == "log2") 
      "No transformation" else "log2"
    
    out <- disabled(pickerInput("transform", "Transform data to:", 
                       choices = c(choices, "No transformation"),
                       selected = set))
    
  } else {
    out <- pickerInput("transform", "Transform data to:", choices = c(choices, "No transformation"))
  }
  
  out
  
})


output$transform_preview_plot <- renderPlotly({
  
  req(!inherits(omicsData$objMSU, "seqData"))
  
  out <- omicsData$objMSU
  
  if(input$transform != get_data_scale(omicsData$objMSU) && 
     !is.null(input$transform) && 
     input$transform != "No transformation"){
    out <- edata_transform(out, input$transform)
  }
  
  p <- plot(out)
  
  isolate(plot_table_current$PP$transform <- p)
  isolate(table_table_current$PP$transform <- out$e_data)
  
  p
  
})

observeEvent(input$done_tr_box, {
  
  updateBoxCollapse(session, id = "transform_collapse", close = "transformation")
  shinyjs::show("complete_transform")
  
})


observeEvent(input$complete_transform, {
  
  req(!is.null(omicsData$objMSU) && input$complete_transform > 0 && 
        !is.null(input$transform) && 
        input$transform != "No transformation" &&
        input$transform != get_data_scale(omicsData$objMSU))
  
  ## Call from previous so they can redo as they like
    omicsData$objPP <- edata_transform(omicsData$objMSU, input$transform)
  
})
