
apply_disabled <- function(el) {
  if (input$rm_prompts_hp == "default")
    disabled(el)
  else
    el
}

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
  
  drop <- which(colnames(omicsData$objPP$e_data) == get_edata_cname(omicsData$objPP)) 
  
  x <- as.data.frame(t(omicsData$objPP$e_data[-drop]))
  if(length(response) == 1){
    y <- omicsData$objPP$f_data[[response]]
  } else {
    y <- omicsData$objPP$f_data[response]
    y <- apply(y, 1, paste, sep = "_")
  }
  
  mtry <- if(!is.null(y) && !is.factor(y)) max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x)))
  min_n <- if (!is.null(y) && !is.factor(y)) 5 else 1
  
  hp_inputs$input_labels <- list(
    "Number of trees in the forest",
    "Minimum number of datapoints for branch split",
    "Number of predictors to evaluate at each split"
  )
  hp_inputs$input_names <- list(
    "trees",
    "min_n",
    "mtry"
  )
  
  div(
    apply_disabled(numericInput("trees", "Number of trees in forest", min = 1L, 
                 value = 500, width = "100%")),
    if(input$rm_prompts_hp == "tuned") 
      checkboxInput("optimize_trees", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("min_n", "Minimum number of datapoints for branch split", min = 1, 
                 max = ncol(x), value = min_n, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_min_n", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("mtry", "Number of predictors to evaluate at each split", min = 1L, 
                 max = nrow(x), value = mtry, width = "100%")),
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
  
  hp_inputs$input_labels <- list(
    "Cost of predicting a sample within or on the wrong side of the margin",
    "Epsilon in the SVM insensitive loss function"
  )
  hp_inputs$input_names <- list(
    "cost",
    "svm_margin"
  )
  
  div(
    apply_disabled(numericInput("cost", "A positive number for the cost of predicting a sample within or on the wrong side of the margin", min = 1e-10, 
                 max = 1e5, value = 1, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_cost", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("svm_margin", "A positive number for the epsilon in the SVM insensitive loss function (regression only)", min = 0, 
                 max = 0.2, value = 0.1, width = "100%")),
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
  
  hp_inputs$input_labels <- list(
    "Cost of predicting a sample within or on the wrong side of the margin",
    "Epsilon in the SVM insensitive loss function",
    "Scaling parameter of the polynomial and tangent kernal",
    "Polynomial degree"
  )
  hp_inputs$input_names <- list(
    "cost",
    "svm_margin",
    "scale_factor",
    "degree"
  )
  
  div(
    apply_disabled(numericInput("cost", "A positive number for the cost of predicting a sample within or on the wrong side of the margin", min = 1e-10, 
                 max = 1e5, value = 1, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_cost", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("svm_margin", "A positive number for the epsilon in the SVM insensitive loss function (regression only)", min = 0, 
                 max = 0.2, value = 0.1, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_svm_margin", "Optimize?", value = F),
    
    apply_disabled(numericInput("scale_factor", "The scaling parameter of the polynomial and tangent kernel is a convenient way of normalizing patterns without the need to modify the data itself", min = 1e-10,
                 max = 1e-10, value = 1e-1, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_scale_factor", "Optimize?", value = F),
    
    
    # numericInput("scale_factor", "The scaling parameter of the polynomial and tangent kernel is a convenient way of normalizing patterns without the need to modify the data itself", min = 10^-10, 
    #              max = 10^-1, value = 1),
    # checkboxInput("optimize_scale_factor", "Optimize?", value = F),
    
    apply_disabled(numericInput("degree", "A positive number for polynomial degree", min = 1, 
                 max = 3, value = 1, width = "100%")),
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
  
  hp_inputs$input_labels <- list(
    "Cost of predicting a sample within or on the wrong side of the margin",
    "Epsilon in the SVM insensitive loss function",
    "Inverse kernal width"
  )
  hp_inputs$input_names <- list(
    "cost",
    "svm_margin",
    "rbf_sigma"
  )
  
  div(
    apply_disabled(numericInput("cost", "A positive number for the cost of predicting a sample within or on the wrong side of the margin", 
                 min = 1e-10, 
                 max = 1e5, value = 1e2, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_cost", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("svm_margin", "A positive number for the epsilon in the SVM insensitive loss function (regression only)", min = 0, 
                 max = 0.2, value = 0.1, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_svm_margin", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("rbf_sigma", "The inverse kernel width used by the kernel", min = 1e-10, 
                 max = 1e0, value = 1, width = "100%")),
    
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
  
  hp_inputs$input_labels <- list(
    "Amount of penalties in regularized models",
    "Relative amount of penalties in regularized models"
  )
  hp_inputs$input_names <- list(
    "penalty",
    "mixture"
  )
  
  div(
    apply_disabled(numericInput("penalty", "A numeric parameter function representing the amount of penalties (e.g. L1, L2, etc) in regularized models.", min = 1e-10, 
                 max = 0, value = 0, step = 0.1, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_penalty", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("mixture", "A numeric parameter function representing the relative amount of penalties (e.g. L1, L2, etc) in regularized models", min = 0, 
                 max = 1, value = 0, step = 0.1, width = "100%")),
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
  
  hp_inputs$input_labels <- list(
    "Amount of penalties in regularized models",
    "Relative amount of penalties in regularized models"
  )
  hp_inputs$input_names <- list(
    "penalty",
    "mixture"
  )
  
  div(
    apply_disabled(numericInput("penalty", "A numeric parameter function representing the amount of penalties (e.g. L1, L2, etc) in regularized models.", min = 1e-10, 
                 max = 1e0, value = 1e-6, step = 0.1, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_penalty", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("mixture", "A numeric parameter function representing the relative amount of penalties (e.g. L1, L2, etc) in regularized models", min = 0, 
                 max = 1, value = 1, step = 0.1, width = "100%")),
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
  
  hp_inputs$input_labels <- list(
    "Amount of penalties in regularized models",
    "Relative amount of penalties in regularized models"
  )
  hp_inputs$input_names <- list(
    "penalty",
    "mixture"
  )
  
  div(
    apply_disabled(numericInput("penalty", "A numeric parameter function representing the amount of penalties (e.g. L1, L2, etc) in regularized models.", min = 1e-10, 
                 max = 1e0, value = 0, step = 0.1, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_penalty", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("mixture", "A numeric parameter function representing the relative amount of penalties (e.g. L1, L2, etc) in regularized models", min = 0, 
                 max = 1, value = 0, step = 0.1, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_mix", "Optimize?", value = F),
    
  )

}

multilasso_params <- function(){

  ## Selected params
  # parsnip::multinom_reg -- mode, engine, penalty, mixture
  # glmnet::glmnet -- x, y, family, weights, offset, alpha, nlambda, lambda.min.ratio, lambda, standardize, intercept, thresh, dfmax, pmax, exclude, penalty.factor, lower.limits, upper.limits, maxit, type.gaussian, type.logistic, standardize.response, type.multinomial, relax, trace.it, ...
  # ## Figure out which ones can be tuned
  
  hp_inputs$input_labels <- list(
    "Amount of penalties in regularized models",
    "Relative amount of penalties in regularized models"
  )
  hp_inputs$input_names <- list(
    "penalty",
    "mixture"
  )
  
  div(
    apply_disabled(numericInput("penalty", "A numeric parameter function representing the amount of penalties (e.g. L1, L2, etc) in regularized models.", min = 1e-10, 
                 max = 1e1, value = 1e-6, step = 0.1, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_penalty", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("mixture", "A numeric parameter function representing the relative amount of penalties (e.g. L1, L2, etc) in regularized models", min = 0, 
                 max = 1, value = 1, step = 0.1, width = "100%")),
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
  
  drop <- which(colnames(omicsData$objPP$e_data) == get_edata_cname(omicsData$objPP)) 
  
  x <- as.data.frame(t(omicsData$objPP$e_data[-drop]))
  if(length(response) == 1){
    y <- omicsData$objPP$f_data[[response]]
  } else {
    y <- omicsData$objPP$f_data[response]
    y <- apply(y, 1, paste, sep = "_")
  }
  
  mtry <- if(!is.null(y) && !is.factor(y)) max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x)))
  min_n <- if (!is.null(y) && !is.factor(y)) 5 else 1
  
  hp_inputs$input_labels <- list(
    "Number of trees in boosted ensemble", 
    "Minimum number of datapoints for branch split", 
    "Number of predictors to evaluate at each split",
    "The cost-complexity parameter in classical CART models",
    "The maximum depth of the tree",
    "The reduction in the loss function required to split further",
    "Scale factor for the contribution of each tree", 
    "Number of iterations without an improvement in the objective function occur before training should be halted.", 
    "The size of the data set used for modeling within an iteration of the modeling algorithm, such as stochastic gradient boosting"
  )
  hp_inputs$input_names <- list(
    "trees",
    "min_n",
    "mtry",
    "cost_complexity",
    "tree_depth",
    "loss_reduction",
    "learn_rate",
    "stop_iter",
    "sample_prop"
  )
  
  div(
    apply_disabled(numericInput("trees", "Number of trees in boosted ensemble", min = 1L, 
                 max = 2000L, value = 50, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_trees", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("min_n", "Minimum number of datapoints for branch split", min = 1, 
                 max = nrow(x), value = 20, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_min_n", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("mtry", "Number of predictors to evaluate at each split", min = 1L, 
                 max = ncol(x), value = 20, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_mtry", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("cost_complexity", "The cost-complexity parameter in classical CART models", min = 1e-10,
                 max = 1e-1, value = 1e-5)),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_cost_complexity", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("tree_depth", "The maximum depth of the tree", min = 1L, 
                 max = 15L, value = 6, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_tree_depth", "Optimize?", value = ),
    br(),
    
    apply_disabled(numericInput("loss_reduction", 
                 "The reduction in the loss function required to split further",
                 min = 1e-10, 
                 max = 10^1.5, value = 1e0, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_loss_reduction", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("learn_rate", "Scale factor for the contribution of each tree", 
                 min = 1e-10, 
                 max = 1e-1, value = 1e-2, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_learn_rate", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("stop_iter", 
                 "Number of iterations without an improvement in the objective function occur before training should be halted.", 
                 min = 3L, 
                 max = 20L, value = 8L, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_stop_iter", "Optimize?", value = F),
    br(),
    
    apply_disabled(numericInput("sample_prop", 
                 "The size of the data set used for modeling within an iteration of the modeling algorithm, such as stochastic gradient boosting", 
                 min = 0, 
                 max = 1, value = .7, width = "100%")),
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_sample_prop", "Optimize?", value = F)
    
  )
  
  
}

#' Parameters for partial least squares.
#' pls_num_comp:  Between 2 and min(n_features, n_samples - 1)
#' pls_predictor_prop:  The proportion of features to keep between 0 and 1.  If this is 1, then it's just pls, anything less than 1 and we are doing sparse pls.
pls_params <- function() {
  n_samps = attributes(omicsData$objPP)$data_info$num_samps
  n_feats = attributes(omicsData$objPP)$data_info$num_edata
  
  hp_inputs$input_labels <- list(
    "Number of components",
    "Proportion of features"
  )
  hp_inputs$input_names <- list(
    "pls_num_comp",
    "pls_predictor_prop"
  )
  
  max_pcs = min(n_feats, n_samps - 1)
  div(
    sliderInput("pls_num_comp", "Number of components", min=2, max=max_pcs, value = 2),
    numericInput("pls_predictor_prop", "Proportion of features", min=0, max=1, value=1)
  )
}

## Split into appropriate later

kmeans_params <- function(){
  
  hp_inputs$input_labels <- list(
    "Number of clusters"
  )
  hp_inputs$input_names <- list(
    "num_clust"
  )
  
  div(
    apply_disabled(numericInput("num_clust", "Number of clusters", value = 2, 
                 min = 1, max = ncol(omicsData$objPP$e_data[-1]), width = "100%")),
    
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
                              "centroid"),
                  width = "100%")
    },
    
    if(input$rm_prompts_hp == "tuned")
      checkboxInput("optimize_linkage", "Optimize?", value = F),
    
    br(),
    
    
    radioGroupButtons("height_clust", "Define clusters by:", 
                      choices = c("Height", "Number of clusters")),
    
    conditionalPanel("input.height_clust == 'Height'", {
      hp_inputs$input_labels <- list(
        "Linkage method",
        "Clusters defined by",
        "Dendogram height to define clusters from"
      )
      hp_inputs$input_names <- list(
        "linkage_methods",
        "height_clust",
        "cut_height"
      )
      
      div(
        apply_disabled(numericInput("cut_height", 
                     "Dendogram height to define clusters from", 
                     value = 0.5, width = "100%")),
        if(input$rm_prompts_hp == "tuned")
          checkboxInput("optimize_height", "Optimize?", value = F)
      )
    }),
    
    conditionalPanel("input.height_clust == 'Number of clusters'", {
      hp_inputs$input_labels <- list(
        "Linkage method",
        "Clusters defined by",
        "Number of clusters"
      )
      hp_inputs$input_names <- list(
        "linkage_methods",
        "height_clust",
        "num_clust"
      )
      
      div(
        apply_disabled(numericInput("num_clust", "Number of clusters", value = 2, 
                     min = 1, max = ncol(omicsData$objPP$e_data[-1]), 
                     width = "100%")),
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

lda_params <- function(){
  div(strong("Not available for selected model"), br(), br())
}

### Not yet available
pca_params <- function(){
  n_samps = attributes(omicsData$objPP)$data_info$num_samps
  n_feats = attributes(omicsData$objPP)$data_info$num_edata
  
  max_pcs = min(n_feats, n_samps - 1)
  
  out <- div(
    sliderInput("pca_num_comp", "Number of components", min=2, max=max_pcs, value = 2)
  )
  
  hp_inputs$input_labels <- list(
    "Number of components"
  )
  hp_inputs$input_names <- list(
    "pca_num_comp"
  )
  
  out
}

### Not yet available
ppca_params <- function(){
  n_samps = attributes(omicsData$objPP)$data_info$num_samps
  n_feats = attributes(omicsData$objPP)$data_info$num_edata
  
  max_pcs = min(n_feats, n_samps - 1)
  
  out <- div(
    sliderInput("ppca_num_comp", "Number of components", min=2, max=max_pcs, value = 2)
  )
  
  hp_inputs$input_labels <- list(
    "Number of components"
  )
  hp_inputs$input_names <- list(
    "ppca_num_comp"
  )
  
  out
}



output[["model_specific_parameters"]] <- renderUI({
  
  req(!is.null(omicsData$objPP) && 
        !is.null(input$pick_model_EM) &&
        !is.null(input$rm_prompts_hp) &&
        ((!is.null(input$pick_model_group_pick) || 
           !is.null(input$f_data_response_picker)) ||
           !supervised()
           )
        )
  
  # fun <- as.character(models_long_name[names(models_long_name) == input$pick_model_EM]) ## while unsup summary is being fixed
  fun <- input$pick_model_EM
  
  function_str <- paste0(fun, "_params()")
  
  out <- eval(parse(text = function_str))
})

outputOptions(output, "model_specific_parameters", suspendWhenHidden = FALSE)

