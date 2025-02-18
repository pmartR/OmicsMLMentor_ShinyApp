output$run_model_UI <- renderUI({
  collapseBox(
    collapsed = F,
    value = "run_model_page",
    "Run Model",
    
    # textOutput("rm_text"),
    strong(paste0(
      "Click the button below to apply the model to the new data.",
      " Additional visualization options are available if Sample Information is provided."
      )),
    
    br(),
    br(),
    
    actionButton("run_model","Run Model")
  )
})

