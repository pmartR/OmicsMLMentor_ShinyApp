
output$QC_LQ_Advanced_UI <- renderUI({
  
  req(input$user_level_pick != "beginner")
  
  div(
    
    " To keep these instead, please un-select the checkbox below.",
    
    br(), br(),
    
    div(
      
      checkboxInput(
        inputId = "QC_add_molfilt",
        label = "Remove single-observation biomolecules", 
        value = TRUE
      )
      
    )
  )
  
  
})

output$QC_single_mol_plot <- renderPlot({
  
  req(omicsData$objQC)
  
   plot(molecule_filter(omicsData$objQC), min_num = 2)
   
})


## Make this iterative, so that these can be removed?
observeEvent(input$LQ_done, {
  
  if(input$user_level_pick == "beginner" || input$QC_add_molfilt){
    omicsData$objQC <- applyFilt(molecule_filter(omicsData$objQC), 
                               omicsData$objQC, min_num = 2)
  } else {
    omicsData$objQC <- omicsData$objQC
  }
  
})
