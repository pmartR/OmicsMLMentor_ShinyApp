output$run_model_UI <- renderUI({
  collapseBox(
    collapsed = F,
    value = "run_model_page",
    "Run Model",
    
    actionButton("run_model","Run Model")
  )
})

