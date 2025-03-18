output$run_model_UI <- renderUI({
  collapseBox(
    collapsed = F,
    value = "run_model_page",
    "Run Model",
    
    uiOutput("model_summary"),
    # strong(paste0(
    #   "Click the button below to apply the model to the new data.",
    #   " Additional visualization options are available if Sample Information is provided."
    #   )),
    
    br(),
    br(),
    
    actionButton("run_model","Run Model")
  )
})


output$model_summary <- renderUI({


  method <- attr(omicsData$model$model, "SLOPE_model_name")
  if(is.null(method)){
    method <- attr(omicsData$model$model, "args_unsup")$slMethod
  }
  
  if(method %in% "loglasso"){
    method <- "logistic"
    extra_text <- " This method differs from traditional logistic models by using a penalization method to select for the most important molecules to use for prediction."
  } else if (method %in% "multilasso"){
    method <- "multi"
    extra_text <- " This method differs from traditional multinomial models by using a penalization method to select for the most important molecules to use for prediction."
  } else {
    extra_text <- NULL
  }
  
  div(
    br(),
    strong(names(models_long_name)[models_long_name == method]),
    br(),
    hr(),
    paste0(text_get(method), extra_text)
    
  )
})

