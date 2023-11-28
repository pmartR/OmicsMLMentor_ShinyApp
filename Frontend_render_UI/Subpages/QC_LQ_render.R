output$QC_single_mol_plot <- renderPlot({
  
  req(omicsData$objQC)
  
   plot(molecule_filter(omicsData$objQC), min_num = 2)
   
})


## Make this iterative, so that these can be removed?
observeEvent(input$LQ_done, {
  
  if(input$QC_add_molfilt){
    omicsData$objQC <- applyFilt(molecule_filter(omicsData$objQC), 
                               omicsData$objQC, min_num = 2)
  } else {
    omicsData$objQC <- omicsData$objQC
  }
  
})
