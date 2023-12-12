

observeEvent(input$rec_split, {
  
  reset("nTest_count")
  reset("nTest_prop")
  
})

observeEvent(input$done_crossval_option, {
  updateBoxCollapse(session, id = "TS_side_collapse", close = "train_param_RM")
  shinyjs::show("complete_TS_RM")
})

output$TS_preview_plot <- renderPlot({
  
  data <- omicsData$objPP
  
  group_info <- get_group_DF(data)
  
  if(holdout_valid() && input$rm_prompts_hp == "tuned"){
    if(input$numb_test == "Proportion"){
      req(!is.na(input$nTest_prop))
      test_data <- group_info %>% group_by(Group) %>% slice_sample(prop = input$nTest_prop)
    } else {
      req(!is.na(input$nTest_count))
      prop_count <- input$nTest_count/nrow(group_info)
      test_data <- group_info %>% group_by(Group) %>% slice_sample(prop = prop_count)
    }
    
    group_info$Status <- "Training data"
    
    group_info$Status[group_info[[1]] %in% test_data[[1]]] <- "Testing data"
    
    group_info$Validation <- as.numeric(as.factor(group_info$Status))
    
    out <- ggplot(group_info, aes(x = Status, fill = Group)) + 
      geom_bar() +
      theme_bw() + labs(x = "", fill = "", y = "Number of samples")
    
  } #else if (!holdout_valid() && input$rm_prompts_hp != "tuned") {
    
  # } else if (input$rm_prompts_hp == "tuned") {
  #   
  # }
  
  
  out
  
})




output$nTest_count_ui <- renderUI({
  req(!is.null(omicsData$objPP))
  
  ## Ideally update this based on model requirements as well
  numericInput("nTest_count", "Approximate number of samples for testing data", 
               value = floor(ncol(omicsData$objPP$e_data)*.3), 
               min = 1, max = (ncol(omicsData$objPP$e_data)) - 2)
  
})


output$holdout_set <- renderUI({
  
  req(holdout_valid() && input$rm_prompts_hp == "tuned")
  
  wellPanel(
    br(),
    "Holdout set will be used to evaluate model performance. Default values are set to the recommended holdout for your dataset.",
    
    br(),
    br(),
    
    radioGroupButtons("numb_test", "Define holdout set", choices = c("Proportion", "Count")),
    
    conditionalPanel("input.numb_test == 'Proportion'", 
                     numericInput("nTest_prop", "Proportion of samples for testing data", min = 0, 
                                  max = 1, value = 0.3, step = 0.01)
    ),
    
    ## Make this dependent on size of dataset
    conditionalPanel("input.numb_test == 'Count'", 
                     
                     uiOutput("nTest_count_ui")
    ),
    
    br(),
    
    actionButton("holdout_rec", "Set to recommended options"),
    actionButton("holdout_done", "Done"),
    actionButton("holdout_info", "Tell me more")
  )
})

output$crossval_perform <- renderUI({
  
  req(!holdout_valid() && input$rm_prompts_hp != "tuned")
  
  wellPanel(
    br(),
    "Subsets of the original data will be used to evaluate model performance. Default options are set to the recommended subsetting for your dataset.",
    
    br(),
    br(),
    
    pickerInput(
      "cv_perform_option",
      "What kind of subsetting should be used?",
      
      choices = list(
        "Evaluate multiple models with single samples left out (leave-one-out)" = "loocv",
        "Evaluate multiple models with multiple samples left out (K-fold)" = "kfcv"
      ),
      width = "100%"
    ),
    
    br(),
    
    conditionalPanel(
      "input.cv_hp_option == 'kfcv'",
      numericInput("nFolds", "Number of samples to use in subset", value = 5, min = 2, max = 10)
    ),
    
    br(),
    
    actionButton("cv_perform_rec", "Set to recommended options"),
    actionButton("cv_perform_done", "Done"),
    actionButton("cv_perform_info", "Tell me more")
  )
})

output$crossval_hp <- renderUI({
  
  req(input$rm_prompts_hp == "tuned")
  
  wellPanel(
    br(),
    "Subsets of the original data will be used to determine best model settings. Default options are set to the recommended subsetting for your dataset.",
    
    br(),
    br(),
    
    pickerInput(
      "cv_hp_option",
      "What kind of subsetting should be used?",
      
      choices = list(
        "Evaluate multiple models with single samples left out for testing (leave-one-out)" = "loocv",
        "Evaluate multiple models with multiple samples left out for testing (K-fold)" = "kfcv"
      ),
      width = "100%"
    ),
    
    br(),
    
    conditionalPanel(
      "input.cv_hp_option == 'kfcv'",
      numericInput("nFolds", "Number of samples to use in subset", value = 5, min = 2, max = 10)
    ),
    
    br(),
    
    actionButton("cv_hp_rec", "Set to recommended options"),
    actionButton("cv_hp_done", "Done"),
    actionButton("cv_hp_info", "Tell me more")
  )
})

