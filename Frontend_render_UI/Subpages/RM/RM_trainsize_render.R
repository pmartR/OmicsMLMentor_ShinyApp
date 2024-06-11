

observeEvent(input$rec_split, {
  
  reset("nTest_count")
  reset("nTest_prop")
  
})

observeEvent(c(input$holdout_done, input$cv_perform_done, input$cv_hp_done), {
  
  inspect_items <- c(!is.null(input$holdout_done), 
    !is.null(input$cv_perform_done), 
    !is.null(input$cv_hp_done))
  
  items <- list(input$holdout_done, 
                input$cv_perform_done, 
                input$cv_hp_done
                )[inspect_items]
  
  if(all(items > 0)){
    updateBoxCollapse(session, id = "TS_side_collapse", close = "train_param_RM")
    shinyjs::show("complete_TS_RM")
  }
  
})

## Options -- holdout_valid => no cv_perform; rm_prompts_hp = tuned cv_hp valid
## Options -- !holdout_valid => cv_perform; rm_prompts_hp = tuned cv_hp valid

## Unique plots: tuning or cv_perform -> fold distribution
  ## Would be super cool if we could get this to be consistent 
  ## with how the folds are determined in backend

## Unique text: holdout valid -- split of holdout and training 
              ##in number of samples and percentage


output$TS_preview_plot <- renderPlotly({
  
  data <- omicsData$objPP
  
  req(!is.null(input$cv_perform_option) || !is.null(input$numb_test) || 
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
    
    group_info$Status <- "Tuning data"
    
    group_info$Status[group_info[[1]] %in% test_data[[1]]] <- "Holdout data"
    
    group_info$Validation <- as.numeric(as.factor(group_info$Status))
    
    out1 <- ggplot(group_info, aes(x = Status, fill = Group)) + 
      geom_bar() +
      theme_bw() + labs(x = "", fill = "", y = "Number of samples", 
                        subset = "Holdout split")
    
    isolate(table_table_current$table$RM__training_structure__performance <- out1$data)
    
    group_info_training <- group_info[group_info$Status == "Tuning data",]
    
    group_info_training$Status <- "Tuning data"
    
    ## Facit plots for training folds
    req(!is.null(input$cv_hp_option))
    
    set.seed(1024)
    
    folds <- rsample::vfold_cv(
      group_info_training,
      v = input$nFolds_hp,
      repeats = 1,
      strata = "Group"
    )
    
    plotter <- map2_dfr(folds$splits, folds$id, function(df, id){
      df <- as.data.frame(df)
      df$fold <- id
      df
    })
    
    out <- ggplot(plotter, aes(x = Group, fill = Group)) + 
      geom_bar(show.legend = F) + facet_wrap(~fold) + 
      theme_bw() + labs(x = "", y = "Number of samples")
    
    isolate(table_table_current$table$RM__training_structure__tuning <- out$data)
    
    # group_info$Status <- "Training data"
    # 
    # group_info$Status[group_info[[1]] %in% test_data[[1]]] <- "Evaluation data"
    # 
    # group_info$Validation <- as.numeric(as.factor(group_info$Status))
    # 
    # out <- ggplot(group_info, aes(x = Status, fill = Group)) + 
    #   geom_bar() +
    #   theme_bw() + labs(x = "", fill = "", y = "Number of samples", 
    #                     subtitle = text)
    
    out <- wrap_plots(out1, out)
    
    ## not tuning with holdout
  } else if (holdout_valid() && input$rm_prompts_hp != "tuned") {
    
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
    
    isolate(table_table_current$table$RM__training_structure__performance <- out$data)
    
    ## tuning and no holdout -- cv hp tuning applied
  } else if (input$rm_prompts_hp == "tuned") {
   
    req(!is.null(input$cv_hp_option))
    if(input$cv_hp_option == "loocv"){
      test_data <- sample(group_info$SampleID, 1)
      text <- "Resampled across all samples"
    } else {
      req(!is.na(input$nFolds_hp))
      test_data <- group_info %>% group_by(Group) %>% slice_sample(prop = 1/input$nFolds_hp)
      text <- paste0("For each of ", input$nFolds_hp, " folds")
    }
    
    set.seed(1024)
    
    folds <- rsample::vfold_cv(
      group_info,
      v = input$nFolds_hp,
      repeats = 1,
      strata = "Group"
    )
    
    plotter <- map2_dfr(folds$splits, folds$id, function(df, id){
      df <- as.data.frame(df)
      df$fold <- id
      df
    })
    
    out <- ggplot(plotter, aes(x = Group, fill = Group)) + 
      geom_bar(show.legend = F) + facet_wrap(~fold) + 
      theme_bw() + labs(x = "", y = "Number of samples")
    
  isolate(table_table_current$table$RM__training_structure__performance <- out$data)
    
  } else if (input$rm_prompts_hp != "tuned") {
    
    if(input$cv_perform_option == "loocv"){
      test_data <- sample(group_info$SampleID, 1)
      text <- "Resampled across all samples"
    } else {
      req(!is.na(input$nFolds_cv))
      test_data <- group_info %>% group_by(Group) %>% 
        slice_sample(prop = 1/input$nFolds_cv)
      text <- paste0("For each of ", input$nFolds_cv, " folds")
    }
    
    set.seed(1024)
    
    folds <- rsample::vfold_cv(
      group_info,
      v = input$nFolds_cv,
      repeats = 1,
      strata = "Group"
    )
    
    plotter <- map2_dfr(folds$splits, folds$id, function(df, id){
      df <- as.data.frame(df)
      df$fold <- id
      df
    })
    
    out <- ggplot(plotter, aes(x = Group, fill = Group)) + 
      geom_bar(show.legend = F) + facet_wrap(~fold) + 
      theme_bw() + labs(x = "", y = "Number of samples")
    
    isolate(table_table_current$table$RM__training_structure__performance <- out$data)
    
    
  }

  isolate(plot_table_current$table$RM__training_structure <- out)
  
  out
  
})




output$nTest_count_ui <- renderUI({
  req(!is.null(omicsData$objPP))
  
  ## Ideally update this based on model requirements as well
  numericInput("nTest_count", "Approximate number of samples for evaluation data", 
               value = floor(ncol(omicsData$objPP$e_data)*.3), 
               min = 20, max = (ncol(omicsData$objPP$e_data)) - 2)
  
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
  
  req(input$rm_prompts_hp != "tuned")
  
  wellPanel(
    br(),
    "Subsets of the original data will be used to evaluate model performance. Default options are set to the recommended subsetting for your dataset.",
    
    br(),
    br(),
    
    pickerInput(
      "cv_perform_option",
      "What kind of subsetting should be used?",
      
      choices = list(
        "Evaluate multiple models with single fold left out (K-fold)" = "kfcv",
        "Evaluate multiple models with single sample left out (leave-one-out)" = "loocv"
      ),
      width = "100%"
    ),
    
    br(),
    
    conditionalPanel(
      "input.cv_perform_option == 'kfcv'",
      numericInput("nFolds_cv", "Number of folds", value = 5, 
                   min = 2, max = 20)
    ),
    
    br(),
    
    hidden(div("Calculating fold recommendation, please wait...",
               id = "perform_nfold_busy",
               class = "fadein-out",
               style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
    )),
    
    br(),
    
    actionButton("cv_perform_rec", "Recommended"),
    actionButton("cv_perform_done", "Done"),
    actionButton("cv_perform_info", "Tell me more")
  )
})

cv_eval <- reactiveValues(result = NULL)

observeEvent(c(input$cv_perform_rec, input$cv_hp_rec), {
  
  req(input$cv_perform_rec > 0 || input$cv_hp_rec > 0)
  
  shinyjs::show("perform_nfold_busy")
  shinyjs::show("tune_nfold_busy")
  
  on.exit({
    shinyjs::hide("perform_nfold_busy")
    shinyjs::hide("tune_nfold_busy")
  })
  
  method <- input$pick_model_EM
  
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
  
  ## Cap the top of folds to test
  max_nfold <- floor(sum(get_group_table(omicsData$objPP))/2)
  
  ## Get default parameters
  custom_args <- list()
  
  if(method == "rf"){
    custom_args <- list(
      trees = input$trees,
      min_n = input$min_n,
      mtry = input$mtry
    )
  } else if (method == "lsvm"){
    custom_args <- list(
      cost = input$cost,
      margin = input$svm_margin
    )
  } else if (method == "psvm"){
    custom_args <- list(
      cost = input$cost,
      margin = input$svm_margin,
      degree = input$degree,
      scale_factor = input$scale_factor
    )
  } else if (method == "rsvm"){
    custom_args <- list(
      cost = input$cost,
      margin = input$svm_margin,
      rbf_sigma = input$rbf_sigma
    )
  } else if (method %in% c("logistic", "loglasso", "multi", "multilasso")){
    custom_args <- list(
      penalty = input$penalty,
      mixture = input$mixture
    )
  } else if (method == "gbtree"){
    custom_args <- list(
      trees = input$trees,
      min_n = input$min_n,
      mtry = input$mtry,
      # cost_complexity,
      tree_depth = input$tree_depth,
      loss_reduction = input$loss_reduction,
      learn_rate = input$learn_rate,
      stop_iter = input$stop_iter,
      sample_size = input$sample_prop
    )
  }  else if (method == "pls") {
    custom_args <- list(
      num_comp = input$pls_num_comp,
      predictor_prop = input$pls_predictor_prop
    )
  }
  
  (max_nfold - 2)/6
  
  list_args <- list(
    slData = data,
    slMethod = method,
    nFolds = 4:max_nfold,
    pTest = 0.2
  )
  
  list_args <- c(list_args, custom_args)
  
  suppressWarnings({
    cv_eval$result <- do.call(slopeR::eval_cv_grid, list_args)
  })
  unregister() ## Remove parallel nonsense
  
  best_fold <- cv_eval$result$nFolds[which.min(cv_eval$result$acc_sds)]
  
  updateNumericInput(session, "nFolds_cv", value = best_fold)
  updateNumericInput(session, "nFolds_hp", value = best_fold)
  
})

observeEvent(cv_eval$result, {
  req(!is.null(cv_eval$result))
  
  appendTab(
    "training_tabset",
    select = T,
    tabPanel(
      "Fold stability",
      plotlyOutput("cv_eval_plot")
    )
    )
  
  output$cv_eval_plot <- renderPlotly({
    p <- plot(cv_eval$result) + theme_bw() + 
      scale_x_continuous(breaks = cv_eval$result$nFolds)
    
    isolate(plot_table_current$table$RM__rec_folds <- p)
    isolate(table_table_current$table$RM__rec_folds <- p$data)
    
    ggplotly(p) %>%
      layout(
        showlegend = F
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
        "Evaluate multiple models with single fold left out (K-fold)" = "kfcv",
        "Evaluate multiple models with single sample left out (leave-one-out)" = "loocv"
      ),
      width = "100%"
    ),
    
    br(),
    
    conditionalPanel(
      "input.cv_hp_option == 'kfcv'",
      numericInput("nFolds_hp", "Number of folds", value = 5, min = 2, max = 20)
    ),
    
    br(),
    
    hidden(div("Calculating fold recommendation, please wait...",
               id = "tune_nfold_busy",
               class = "fadein-out",
               style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
    )),
    
    br(),
    
    actionButton("cv_hp_rec", "Recommended"),
    actionButton("cv_hp_done", "Done"),
    actionButton("cv_hp_info", "Tell me more")
  )
})

