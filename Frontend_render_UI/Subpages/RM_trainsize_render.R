

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
  
  req(!is.null(input$cv_perform_option) || 
        !is.null(input$numb_test) || 
        !is.null(input$cv_hp_option))
  
  group_info <- get_group_DF(data)
  
  ## Holdout and tuning -- holdout and cv hp applied
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
    
    group_info$Status[group_info[[1]] %in% test_data[[1]]] <- "Evaluation data"
    
    group_info$Validation <- as.numeric(as.factor(group_info$Status))
    
    out <- ggplot(group_info, aes(x = Status, fill = Group)) + 
      geom_bar() +
      theme_bw() + labs(x = "", fill = "", y = "Number of samples")
    
    ## No holdout and tuning -- cv hp applied
  } else if(holdout_valid() && input$rm_prompts_hp != "tuned"){
    if(input$numb_test == "Proportion"){
      req(!is.na(input$nTest_prop))
      test_data <- group_info %>% group_by(Group) %>% slice_sample(prop = input$nTest_prop)
    } else {
      req(!is.na(input$nTest_count))
      prop_count <- input$nTest_count/nrow(group_info)
      test_data <- group_info %>% group_by(Group) %>% slice_sample(prop = prop_count)
    }
    
    group_info$Status <- "Training data"
    
    group_info$Status[group_info[[1]] %in% test_data[[1]]] <- "Evaluation data"
    
    group_info$Validation <- as.numeric(as.factor(group_info$Status))
    
    out <- ggplot(group_info, aes(x = Status, fill = Group)) + 
      geom_bar() +
      theme_bw() + labs(x = "", fill = "", y = "Number of samples")
    
    ## not tuning and no holdout -- cv perform applied
  } else if (!holdout_valid() && input$rm_prompts_hp != "tuned") {
    
    if(input$cv_perform_option == "loocv"){
      test_data <- sample(group_info$SampleID, 1)
      text <- "Resampled across all samples"
    } else {
      req(!is.na(input$nFolds_cv))
      test_data <- group_info %>% group_by(Group) %>% slice_sample(prop = 1/input$nFolds_cv)
      text <- paste0("For each of ", input$nFolds_cv, " folds")
    }
    
    group_info$Status <- "Training data"
    
    group_info$Status[group_info[[1]] %in% test_data[[1]]] <- "Evaluation data"
    
    group_info$Validation <- as.numeric(as.factor(group_info$Status))
    
    out <- ggplot(group_info, aes(x = Status, fill = Group)) + 
      geom_bar() +
      theme_bw() + labs(x = "", fill = "", y = "Number of samples", 
                        subtitle = text)
    
    ## not tuning and no holdout -- cv perform applied
  } else if (!holdout_valid() && input$rm_prompts_hp == "tuned") {
   
    if(input$cv_hp_option == "loocv"){
      test_data <- sample(group_info$SampleID, 1)
      text <- "Resampled across all samples"
    } else {
      req(!is.na(input$nFolds_cv))
      test_data <- group_info %>% group_by(Group) %>% slice_sample(prop = 1/input$nFolds_cv)
      text <- paste0("For each of ", input$nFolds_cv, " folds")
    }
    
    group_info$Status <- "Training data"
    
    group_info$Status[group_info[[1]] %in% test_data[[1]]] <- "Evaluation data"
    
    group_info$Validation <- as.numeric(as.factor(group_info$Status))
    
    out <- ggplot(group_info, aes(x = Status, fill = Group)) + 
      geom_bar() +
      theme_bw() + labs(x = "", fill = "", y = "Number of samples", 
                        subtitle = text)
    
  }

  out
  
})




output$nTest_count_ui <- renderUI({
  req(!is.null(omicsData$objPP))
  
  ## Ideally update this based on model requirements as well
  numericInput("nTest_count", "Approximate number of samples for evaluation data", 
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
                     numericInput("nTest_prop", "Proportion of samples for evaluation data", min = 0, 
                                  max = 1, value = 0.3, step = 0.01)
    ),
    
    ## Make this dependent on size of dataset
    conditionalPanel("input.numb_test == 'Count'", 
                     
                     uiOutput("nTest_count_ui")
    ),
    
    br(),
    
    actionButton("holdout_rec", "Recommended"),
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
      "input.cv_perform_option == 'kfcv'",
      numericInput("nFolds_cv", "Number of samples to use in subset", value = 5, min = 2, max = 10)
    ),
    
    br(),
    
    actionButton("cv_perform_rec", "Recommended"),
    actionButton("cv_perform_done", "Done"),
    actionButton("cv_perform_info", "Tell me more")
  )
})

observeEvent(input$cv_perform_rec, {
  
  browser()
  
  ## Get correct response variable
  if(isTruthy(input$skip_ag)){
    response <- input$pick_model_group_pick
  } else {
    response <- input$f_data_response_picker
  }
  
  ## Run with correct response variable type
  class_responses <- apply(omicsData$objPP$f_data[response], 2, class)
  rt <- if(all(class_responses %in% c("factor", "character"))) "categorical" else "continuous"
  
  
  data <- as.slData(omicsData$objPP,
                      response_cols = response,
                      response_types = rep(rt, length(response)))
  group_info <- get_group_DF(omicsData$objPP)
  max_nfold <- min(floor(get_group_table(omicsData$objPP)/3))
  
  if(is.null(input$numb_test)){
    test_data <- 0
  } else {
    if(input$numb_test == "Proportion"){
      req(!is.na(input$nTest_prop))
      test_data <- group_info %>% group_by(Group) %>% 
        slice_sample(prop = input$nTest_prop) %>% length()
    } else {
      req(!is.na(input$nTest_count))
      prop_count <- input$nTest_count/nrow(group_info)
      test_data <- group_info %>% group_by(Group) %>% 
        slice_sample(prop = prop_count) %>% length()
    }
  }
  
  res <- map(1:max_nfold, function(fold){
    evaluate_cv( 
      slData = data, 
      nFolds = fold,
      slMethod = input$pick_model_EM,
      nTest = test_data
    )
  })
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
      numericInput("nFolds_hp", "Number of samples to use in subset", value = 5, min = 2, max = 10)
    ),
    
    br(),
    
    actionButton("cv_hp_rec", "Recommended"),
    actionButton("cv_hp_done", "Done"),
    actionButton("cv_hp_info", "Tell me more")
  )
})

