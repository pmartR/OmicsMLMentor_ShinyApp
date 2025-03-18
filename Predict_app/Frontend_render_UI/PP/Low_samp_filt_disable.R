

observeEvent(omicsData$obj$e_data, {
  
  if(ncol(omicsData$obj$e_data) < 6){
    disable(input$apply_filters)
  }
  
})