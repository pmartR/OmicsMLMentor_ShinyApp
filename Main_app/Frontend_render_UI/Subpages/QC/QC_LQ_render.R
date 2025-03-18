
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

output$QC_single_mol_plot <- renderPlotly({

  req(!is.null(omicsData$objQC) && 
        (length(attr(omicsData$objQC, "filters")) == 0 ||
        !any("molFilt" %in% map(attr(omicsData$objQC, "filters"), 1))
        ),
      cancelOutput = T
        )
  
  
  
  filt <- molecule_filter(omicsData$objQC)
  
   p <- plot(filt, min_num = 2)
   
   isolate(plot_table_current$table$QC__single_obs <- p)
   isolate(table_table_current$table$QC__single_obs <- filt)
   
   p
   
})


## Make this iterative, so that these can be removed?
observeEvent(input$LQ_done, {
  
  n_biomolecules_before <- nrow(omicsData$objQC$e_data)
  
  if(input$user_level_pick == "beginner" || input$QC_add_molfilt){
    omicsData$objQC <- applyFilt(molecule_filter(omicsData$objQC), 
                               omicsData$objQC, min_num = 2)
  } else {
    omicsData$objQC <- auto_remove_na(omicsData$objQC)
  }
  
  user_inputs$filters$single_molfilt <- n_biomolecules_before - nrow(omicsData$objQC$e_data)
  
})
