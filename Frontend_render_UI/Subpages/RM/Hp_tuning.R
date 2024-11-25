

observeEvent(input$param_opti, {
  unregister()
  ## Check normalization application
  shinyjs::show("HP_busy")

  on.exit({
    shinyjs::hide("HP_busy")
  })
  
  if (Sys.getenv("TUNE_USE_PARALLEL") == "1") {
    future::plan(future::multisession)
    
    on.exit({
      future::plan(future::sequential)
    }, add = TRUE)
  }
  
  method <- input$pick_model_EM
  
  if(method %in% models_supervised){
    
    ## Get correct response variable
    if(isTruthy(input$skip_ag)){
      response <- input$pick_model_group_pick
    } else {
      response <- input$f_data_response_picker
    }
    
    runner <- as.slData(omicsData$objPP,
                        response_cols = response,
                        response_types = response_types_ag())
    
    ## Get correct train/test split
    if(!is.null(input$numb_test)){
      if(input$numb_test == "Proportion"){
        ptest <- input$nTest_prop
      } else {
        ptest <- input$nTest_count/ncol(runner$e_data[-1])
      }
    } else ptest <- 0
    
    ## Get custom/optimized parameters
    custom_args <- list()
    
    
    ### Replace any tuning parameters ###
    if(method == "rf"){
      custom_args <- list(
        trees = if(input$optimize_trees) tune::tune() else input$trees,
        min_n = if(input$optimize_min_n) tune::tune() else input$min_n,
        mtry = if(input$optimize_mtry) tune::tune() else input$mtry
      )
    } else if (method == "lsvm"){
      custom_args <- list(
        cost = if(input$optimize_cost) tune::tune() else input$cost,
        margin = if(input$optimize_svm_margin) tune::tune() else input$svm_margin
      )
    } else if (method == "psvm"){
      custom_args <- list(
        cost = if(input$optimize_cost) tune::tune() else input$cost,
        margin = if(input$optimize_svm_margin) tune::tune() else input$svm_margin,
        degree = if(input$optimize_degree) tune::tune() else input$degree,
        scale_factor = if(input$optimize_scale_factor) tune::tune() else input$scale_factor
      )
    } else if (method == "rsvm"){
      custom_args <- list(
        cost = if(input$optimize_cost) tune::tune() else input$cost,
        margin = if(input$optimize_svm_margin) tune::tune() else input$svm_margin,
        rbf_sigma = if(input$optimize_rbf_sigma) tune::tune() else input$rbf_sigma
      )
    } else if (method %in% c("logistic", "loglasso", "multi", "multilasso")){
      custom_args <- list(
        penalty = if(input$optimize_penalty) tune::tune() else input$penalty,
        mixture = if(input$optimize_mix) tune::tune() else input$mixture
      )
    } else if (method == "gbtree"){
      custom_args <- list(
        trees = if(input$optimize_trees) tune::tune() else input$trees,
        min_n = if(input$optimize_min_n) tune::tune() else input$min_n,
        mtry = if(input$optimize_mtry) tune::tune() else input$mtry,
        # cost_complexity,
        tree_depth = if(input$optimize_tree_depth) tune::tune() else input$tree_depth,
        loss_reduction = if(input$optimize_loss_reduction) tune::tune() else input$loss_reduction,
        learn_rate = if(input$optimize_learn_rate) tune::tune() else input$learn_rate,
        stop_iter = if(input$optimize_stop_iter) tune::tune() else input$stop_iter,
        sample_size = if(input$optimize_sample_prop) tune::tune() else input$sample_prop
      )
    } else if (method == "pls") {
      custom_args <- list(
        num_comp = if(input$optimize_pls_num_comp) tune::tune() else input$pls_num_comp,
        predictor_prop = if(input$optimize_pls_predictor_prop) tune::tune() else input$pls_predictor_prop
      )
    }
    
    
    cvMethod <- input$cv_hp_option
    nFolds <- input$nFolds_hp
    
    list_args <- list(
      slData = runner,
      slMethod = method,
      cvMethod = cvMethod,
      nFolds = nFolds,
      pTest = ptest,
      return_cv = T
    )
    
    list_args <- c(list_args, custom_args)
    
    omicsData$objHP <- do.call(slopeR::fit, list_args)
    
  }
  
  
})