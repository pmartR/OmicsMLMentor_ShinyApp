
output[["param_opti_UI"]] <- renderUI({
  if(input$rm_prompts_hp == "tuned"){
    out <- actionButton("param_opti", "Optimize parameters")
    if(!any(map_lgl(grep("optimize", names(input), value = T), 
                    function(x) input[[x]])))
      out <- disabled(out)
  } else {
    out <- NULL
  }

  
  div(
    out,
    actionButton("rec_param_option", "Recommended"),
    actionButton("done_param_option", "Done")
  )

})

## Param output

### Functions for valid model parameters

## There are a ton -- https://www.tidymodels.org/find/parsnip/
## Find valid optimizations

rf_params <- function(){

  ## params
  ## "parsnip::rand_forest -- mode, engine, mtry, trees, min_n"
  ## "randomForest:::randomForest.default -- x, y, xtest, ytest, ntree, mtry, weights, replace, classwt, cutoff, strata, sampsize, nodesize, maxnodes, importance, localImp, nPerm, proximity, oob.prox, norm.votes, do.trace, keep.forest, corr.bias, keep.inbag, ..."

  # ## Figure out which ones can be tuned
  # 
  # dials::mtry()
  # dials::trees()
  # dials::min_n()
  # 
  # dials::max_nodes()
  # 
  # ## strata argument for stratified sampling
  # 
  # dials::sample_size()
  # dials::sample_prop()
  # dials::cost_complexity()
  # dials::tree_depth()
  # dials::loss_reduction()
  # dials::prune()
  
  if(isTruthy(input$skip_ag)){
    response <- input$pick_model_group_pick
  } else {
    response <- input$f_data_response_picker
  }
  
  x <- as.data.frame(t(omicsData$objPP$e_data[-1]))
  if(length(response) == 1){
    y <- omicsData$objPP$f_data[[response]]
  } else {
    y <- omicsData$objPP$f_data[response]
    y <- apply(y, 1, paste, sep = "_")
  }
  
  mtry <- if(!is.null(y) && !is.factor(y)) max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x)))
  min_n <- if (!is.null(y) && !is.factor(y)) 5 else 1
  
  div(
    numericInput("trees", "Number of trees in forest", min = 1L, 
                 max = 2000L, value = 500, width = "100%"),
    if(input$rm_prompts_hp == "tuned") 
      checkboxInput("optimize_trees", "Optimize?", value = F),
    br(),
    
    numericInput("min_n", "Minimum number of datapoints for branch split", min = 2L, 
                 max = 40L, value = min_n, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_min_n", "Optimize?", value = F),
    br(),
    
    numericInput("mtry", "Number of predictors to evaluate at each split", min = 1L, 
                 max = 50L, value = mtry, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_mtry", "Optimize?", value = F)
    
  )
  

}

lsvm_params <- function(){

  ## Selected params
  # parsnip::svm_linear -- mode, engine, cost, margin
  # ksvm_fun -- x, data, ..., subset, na.action, scaled

  ## Figure out which ones can be tuned
  ## scaled as TF, when would you not scale?
  # dials::cost() #### is the trans argument needed here?
  # dials::svm_margin() #### is the trans argument needed here?
  
  div(
    numericInput("cost", "A positive number for the cost of predicting a sample within or on the wrong side of the margin", min = 1e-10, 
                 max = 1e5, value = 1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_cost", "Optimize?", value = F),
    br(),
    
    numericInput("svm_margin", "A positive number for the epsilon in the SVM insensitive loss function (regression only)", min = 0, 
                 max = 0.2, value = 0.1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_svm_margin", "Optimize?", value = F),

  )


}

psvm_params <- function(){

  ## Selected params
  # parsnip::svm_poly -- mode, engine, cost, degree, scale_factor, margin
  # ksvm_fun -- x, data, ..., subset, na.action, scaled
  ## Figure out which ones can be tuned

  ## scaled as TF, when would you not scale?
  # dials::cost() ## C #### is the trans argument needed here?
  # dials::svm_margin() ## epsilon.   #### is the trans argument needed here?
  # dials::scale_factor()
  # dials::degree()
  
  
  div(
    numericInput("cost", "A positive number for the cost of predicting a sample within or on the wrong side of the margin", min = 1e-10, 
                 max = 1e5, value = 1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_cost", "Optimize?", value = F),
    br(),
    
    numericInput("svm_margin", "A positive number for the epsilon in the SVM insensitive loss function (regression only)", min = 0, 
                 max = 0.2, value = 0.1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_svm_margin", "Optimize?", value = F),
    
    numericInput("scale_factor", "The scaling parameter of the polynomial and tangent kernel is a convenient way of normalizing patterns without the need to modify the data itself", min = 1e-10,
                 max = 1e-10, value = 1e-1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_scale_factor", "Optimize?", value = F),
    
    
    # numericInput("scale_factor", "The scaling parameter of the polynomial and tangent kernel is a convenient way of normalizing patterns without the need to modify the data itself", min = 10^-10, 
    #              max = 10^-1, value = 1),
    # checkboxInput("optimize_scale_factor", "Optimize?", value = F),
    
    numericInput("degree", "A positive number for polynomial degree", min = 1, 
                 max = 3, value = 1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_degree", "Optimize?", value = F),
    
  )

}

rsvm_params <- function(){

  ## Selected params
  # parsnip::svm_rbf -- mode, engine, cost, rbf_sigma, margin
  # ksvm_fun -- x, data, ..., subset, na.action, scaled

  ## Figure out which ones can be tuned
  ## scaled as TF, when would you not scale?
  # dials::cost()
  # dials::svm_margin()
  # dials::rbf_sigma()
  
  div(
    numericInput("cost", "A positive number for the cost of predicting a sample within or on the wrong side of the margin", 
                 min = 1e-10, 
                 max = 1e5, value = 1e2, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_cost", "Optimize?", value = F),
    br(),
    
    numericInput("svm_margin", "A positive number for the epsilon in the SVM insensitive loss function (regression only)", min = 0, 
                 max = 0.2, value = 0.1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_svm_margin", "Optimize?", value = F),
    br(),
    
    numericInput("rbf_sigma", "The inverse kernel width used by the kernel", min = 1e-10, 
                 max = 1e0, value = 1, width = "100%"),
    
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_rbf_sigma", "Optimize?", value = F),
    
    br()
    ## sigest
    
  )

}

logistic_params <- function(){

  ## Selected params
  # parsnip::logistic_reg -- mode, engine, penalty, mixture
  # glmnet::glmnet -- x, y, family, weights, offset, alpha, nlambda, lambda.min.ratio, lambda, standardize, intercept, thresh, dfmax, pmax, exclude, penalty.factor, lower.limits, upper.limits, maxit, type.gaussian, type.logistic, standardize.response, type.multinomial, relax, trace.it, ...
  ## Figure out which ones can be tuned

  # dials::penalty() ## these should be for feature selection -- these need to come after splits tho?
  # dials::mixture() ## these should be for feature selection  -- these need to come after splits tho?
  # ?dials::weight
  # ?dials::weight_scheme
  # ?dials::weight_func
  
  div(
    numericInput("penalty", "A numeric parameter function representing the amount of penalties (e.g. L1, L2, etc) in regularized models.", min = 1e-10, 
                 max = 0, value = 0, step = 0.1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_penalty", "Optimize?", value = F),
    br(),
    
    numericInput("mixture", "A numeric parameter function representing the relative amount of penalties (e.g. L1, L2, etc) in regularized models", min = 0, 
                 max = 1, value = 0, step = 0.1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_mix", "Optimize?", value = F),
    
  )

}

loglasso_params <- function(){

  ## Selected params
  # parsnip::logistic_reg -- mode, engine, penalty, mixture
  # glmnet::glmnet -- x, y, family, weights, offset, alpha, nlambda, lambda.min.ratio, lambda, standardize, intercept, thresh, dfmax, pmax, exclude, penalty.factor, lower.limits, upper.limits, maxit, type.gaussian, type.logistic, standardize.response, type.multinomial, relax, trace.it, ...
  # ## Figure out which ones can be tuned

  # dials::penalty() ## these should be for feature selection -- these need to come after splits tho?
  # dials::mixture() ## these should be for feature selection  -- these need to come after splits tho?
  # ?dials::weight
  # ?dials::weight_scheme
  # ?dials::weight_func
  
  div(
    numericInput("penalty", "A numeric parameter function representing the amount of penalties (e.g. L1, L2, etc) in regularized models.", min = 1e-10, 
                 max = 1e0, value = 1, step = 0.1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_penalty", "Optimize?", value = F),
    br(),
    
    numericInput("mixture", "A numeric parameter function representing the relative amount of penalties (e.g. L1, L2, etc) in regularized models", min = 0, 
                 max = 1, value = 1, step = 0.1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_mix", "Optimize?", value = F),
    
  )

}

multi_params <- function(){

  ## Selected params
  # parsnip::multinom_reg -- mode, engine, penalty, mixture
  # glmnet::glmnet -- x, y, family, weights, offset, alpha, nlambda, lambda.min.ratio, lambda, standardize, intercept, thresh, dfmax, pmax, exclude, penalty.factor, lower.limits, upper.limits, maxit, type.gaussian, type.logistic, standardize.response, type.multinomial, relax, trace.it, ...
  ## Figure out which ones can be tuned

  # dials::penalty() ## these should be for feature selection -- these need to come after splits tho?
  # dials::mixture() ## these should be for feature selection  -- these need to come after splits tho?
  # ?dials::weight
  # ?dials::weight_scheme
  # ?dials::weight_func
  
  div(
    numericInput("penalty", "A numeric parameter function representing the amount of penalties (e.g. L1, L2, etc) in regularized models.", min = 1e-10, 
                 max = 1e0, value = 0, step = 0.1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_penalty", "Optimize?", value = F),
    br(),
    
    numericInput("mixture", "A numeric parameter function representing the relative amount of penalties (e.g. L1, L2, etc) in regularized models", min = 0, 
                 max = 1, value = 0, step = 0.1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_mix", "Optimize?", value = F),
    
  )

}

multilasso_params <- function(){

  ## Selected params
  # parsnip::multinom_reg -- mode, engine, penalty, mixture
  # glmnet::glmnet -- x, y, family, weights, offset, alpha, nlambda, lambda.min.ratio, lambda, standardize, intercept, thresh, dfmax, pmax, exclude, penalty.factor, lower.limits, upper.limits, maxit, type.gaussian, type.logistic, standardize.response, type.multinomial, relax, trace.it, ...
  # ## Figure out which ones can be tuned

  div(
    numericInput("penalty", "A numeric parameter function representing the amount of penalties (e.g. L1, L2, etc) in regularized models.", min = 1e-10, 
                 max = 1e1, value = 1, step = 0.1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_penalty", "Optimize?", value = F),
    br(),
    
    numericInput("mixture", "A numeric parameter function representing the relative amount of penalties (e.g. L1, L2, etc) in regularized models", min = 0, 
                 max = 1, value = 1, step = 0.1, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_mix", "Optimize?", value = F),
    
  )

}

knn_params <- function(){}

gbtree_params <- function(){

  ## Selected params
  # parsnip::boost_tree -- mode, engine, mtry, trees, min_n, tree_depth, learn_rate, loss_reduction, sample_size, stop_iter
  # parsnip::xgb_train -- x, y, weights, max_depth, nrounds, eta, colsample_bynode, colsample_bytree, min_child_weight, gamma, subsample, validation, early_stop, counts, event_level, ...
  # xgboost::xgb.train -- params, data, nrounds, watchlist, obj, feval, verbose, print_every_n, early_stopping_rounds, maximize, save_period, save_name, xgb_model, callbacks, ...
  # ## Figure out which ones can be tuned

  # ?dials::cost_complexity()
  # ?dials::tree_depth()
  # dials::trees()
  # dials::sample_size()
  # dials::sample_prop()
  # dials::loss_reduction()
  # ?dials::learn_rate()
  # ?dials::stop_iter()
  # dials::min_n()
  # dials::mtry()
  
  
  if(isTruthy(input$skip_ag)){
    response <- input$pick_model_group_pick
  } else {
    response <- input$f_data_response_picker
  }
  
  x <- as.data.frame(t(omicsData$objPP$e_data[-1]))
  if(length(response) == 1){
    y <- omicsData$objPP$f_data[[response]]
  } else {
    y <- omicsData$objPP$f_data[response]
    y <- apply(y, 1, paste, sep = "_")
  }
  
  mtry <- if(!is.null(y) && !is.factor(y)) max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x)))
  min_n <- if (!is.null(y) && !is.factor(y)) 5 else 1

  div(
    numericInput("trees", "Number of trees in boosted ensemble", min = 1L, 
                 max = 2000L, value = 50, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_trees", "Optimize?", value = F),
    br(),
    
    numericInput("min_n", "Minimum number of datapoints for branch split", min = 2L, 
                 max = 40L, value = 20, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_min_n", "Optimize?", value = F),
    br(),
    
    numericInput("mtry", "Number of predictors to evaluate at each split", min = 1L, 
                 max = 50L, value = 20, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_mtry", "Optimize?", value = F),
    br(),
    
    numericInput("cost_complexity", "The cost-complexity parameter in classical CART models", min = 1e-10,
                 max = 1e-1, value = 1e-5),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_cost_complexity", "Optimize?", value = F),
    br(),
    
    numericInput("tree_depth", "The maximum depth of the tree", min = 1L, 
                 max = 15L, value = 6, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_tree_depth", "Optimize?", value = ),
    br(),
    
    numericInput("loss_reduction", "The reduction in the loss function required to split further", min = 1e-10, 
                 max = 10^1.5, value = 1e0, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_loss_reduction", "Optimize?", value = F),
    br(),
    
    numericInput("learn_rate", "Scale factor for the contribution of each tree", min = 1e-10, 
                 max = 1e-1, value = 1e-2, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_learn_rate", "Optimize?", value = F),
    br(),
    
    numericInput("stop_iter", "Number of iterations without an improvement in the objective function occur before training should be halted.", min = 3L, 
                 max = 20L, value = 8L, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_stop_iter", "Optimize?", value = F),
    br(),
    
    numericInput("sample_prop", "The size of the data set used for modeling within an iteration of the modeling algorithm, such as stochastic gradient boosting", 
                 min = 0, 
                 max = 1, value = .7, width = "100%"),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_sample_prop", "Optimize?", value = F)
    
  )
  
  
}


## Split into appropriate later

kmeans_params <- function(){
  
  div(
    numericInput("num_clust", "Number of clusters", value = 2, 
                 min = 1, max = ncol(omicsData$objPP$e_data[-1]), width = "100%"),
    
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_num_clust", "Optimize?", value = F)
  )
  
}

hclust_params <- function(){
  
  div(
    ## This might make more sense as a picture -- hierachical only
    
    if(input$user_level_pick == "beginner"){

        pickerInput("linkage_method", "Linkage method",
                    choices = c("complete"), width = "100%")
    } else {
      pickerInput("linkage_method", "Linkage method", 
                  choices = c("ward.D",
                              "ward.D2",
                              "single",
                              "complete",
                              "average",
                              "mcquitty",
                              "median",
                              "centroid"), width = "100%")
    },
    
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_linkage", "Optimize?", value = F),
    
    br(),
    
    
    radioGroupButtons("height_clust", "Define clusters by:", 
                      choices = c("Height", "Number of clusters")),
    
    conditionalPanel("input.height_clust == 'Height'", {
      div(
      numericInput("cut_height", 
                   "Dendogram height to define clusters from", 
                   value = 0.5, width = "100%"),
      if(input$rm_prompts_hp == "tuned")
        checkboxInput("optimize_height", "Optimize?", value = F)
      )
    }),
    
    conditionalPanel("input.height_clust == 'Number of clusters'", {
      div(
        numericInput("num_clust", "Number of clusters", value = 2, 
                     min = 1, max = ncol(omicsData$objPP$e_data[-1]), 
                     width = "100%"),
        if(input$rm_prompts_hp == "tuned")
          checkboxInput("optimize_num_clust", "Optimize?", 
                        value = F, width = "100%")
      )
    })
  )
  
}


### Not yet available
umap_params <- function(){
  div(strong("Not available for selected model"), br(), br())
}

### Not yet available
pca_params <- function(){
  div(strong("Not available for selected model"), br(), br())
}



output[["model_specific_parameters"]] <- renderUI({
  
  req(!is.null(omicsData$objPP) && 
        !is.null(input$pick_model_EM) &&
        !is.null(input$rm_prompts_hp) &&
        ((!is.null(input$pick_model_group_pick) || 
           !is.null(input$f_data_response_picker)) ||
           input$ag_prompts == "unsupervised"
           )
        )
  
  # fun <- as.character(models_long_name[names(models_long_name) == input$pick_model_EM]) ## while unsup summary is being fixed
  fun <- input$pick_model_EM
  
  function_str <- paste0(fun, "_params()")
  
  out <- eval(parse(text = function_str))
  
})

outputOptions(output, "model_specific_parameters", suspendWhenHidden = FALSE)

# observeEvent(input$pick_model_EM, {
#   
#   fun <- input$pick_model_EM
#   list_parm_strings <- c()
#   
#   if(method == "rf"){
#     list_parm_strings <- c("trees", "min_n", "mtry")
#   } else if (method == "lsvm"){
#     list_parm_strings <- c("cost", "svm_margin")
#   } else if (method == "psvm"){
#     list_parm_strings <- c("cost", "svm_margin", "degree", "scale_factor")
#   } else if (method == "rsvm"){
#     list_parm_strings <- c("cost", "svm_margin", "rbf_sigma")
#   } else if (method %in% c("logistic", "loglasso", "multi", "multilasso")){
#     list_parm_strings <- c("penalty", "mixture")
#   } else if (method == "gbtree"){
#     
#     list_parm_strings <- c("trees", "min_n", "mtry", "tree_depth",
#                            "loss_reduction", "learn_rate", "stop_iter",
#                            "sample_prop")
#   }
#   
#   map(list_parm_strings, function(str){
#     outputOptions(output, "model_specific_parameters", suspendWhenHidden = FALSE)
#   })
#   
# })

