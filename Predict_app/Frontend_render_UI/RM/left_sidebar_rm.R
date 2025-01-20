output$run_model_UI <- renderUI({
  collapseBox(
    collapsed = F,
    value = "run_model_page",
    "Run Model",
    
    br(), br(),
    strong(paste0(
      "Click the button below to use the model to predict the response in the new data.",
      " Predictions of data with a known response will generate additional metrics for assessment."
      )),
    
    br(),
    br(),
    
    actionButton("run_model","Run Model")
  )
})

