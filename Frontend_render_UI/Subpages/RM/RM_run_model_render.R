output[["RM_tab_UI"]] <- renderUI({

  if(supervised()){
    supervised_tab()
  } else {
    unsupervised_tab()
  }

})

supervised_tab <- function() {
  
  div(
    # "Supervised",
    value = 'supervised_tab',
    class = "collapse_page",
    fluidRow(
      
      # Data upload column ---------------------------------------------------
      
      # Creates the column on the left-hand-side of the page for the
      # collapsible boxes to select files to upload along with their
      # corresponding options.
      column(
        # The width is 1/3 of the page for the method selection boxes and menus.
        4,
        # Creates a main box that is divided into multiple collapsible boxes.
        
        collapseBoxGroup(
          id = "sl_left",
          open = c("select_sl"),
          multiple = TRUE, # parent collapse div
          
          # Data type and file upload (e_data) ---------------
          collapseBox(
            collapsed = F,
            
            "Selected model",
            
            value = "select_sl",
            
            div(
              uiOutput("model_summary")
            ),
            
            div(),
            hr(),
            # numericInput(
            #   "the_seed",
            #   "Random seed",
            #   value = 0,
            #   step = 1
            # ),
            
            # numericInput(
            #   "vi_thresh",
            #   "Variable importance threshold:",
            #   min = 0, max = 1, value = 0.5,
            #   step = 0.1
            # ),
            
            hidden(div("Performing analysis, please wait...",
                       id = "RM_busy",
                       class = "fadein-out",
                       style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
            )),
            
            
            # disabled(
              actionButton(
                "run_sl",
                "Run model"#,
                # style = "primary"
              )
            # )
          ),
          
          uiOutput("Variable_importance_ui")
          
        ),
        
        hidden(actionButton("complete_RM", "Complete analysis"))
        
      ), # Method selection column (width 4)
      
      # Data preview column --------------------------------------------------
      
      # Creates the column containing the Data Preview and the Boxplot Preview
      # tabs.
      column(
        8, # 2/3 of the page width
        collapseBoxGroup(
          id = "sl_preview_collapse",
          collapseBox(
            
            titletext = "Visualize model accuracy",
            collapsed = F,
            
            value = "results_RM",
            
            div(
              column(6, uiOutput("super_plot_type_UI")),
              column(6, uiOutput("visualize_perf_split_ui")),
              style= "float:right;z-index:1100;"
            ),
          
            uiOutput("performance_tabset_UI")
          ),
          
          uiOutput("VI_tabset_UI_collapse")
        )
      )# main column
    ) # fluidRow
  ) # tabPanel
  
}


output$model_summary <-renderUI({
  
  method <- input$pick_model_EM
  div(
    br(),
    strong(names(models_long_name)[models_long_name == method]),
    br(),
    hr(),
    text_get(method)
    
  )
})


#' @details Make the picker of plot type depend on the task.
output$super_plot_type_UI <- renderUI({
  req(omicsData$objRM)
  task <- attr(omicsData$objRM, 'fit_info')$task
  
  if (task == 'regression') {
    choices = c(
      "Predicted vs Actual Values" = 'predicted_v_actual'
    )
  } else if (task == "classification") {
    choices = c(
      "True positive performance" = "roc_curve",
      "Prediction vs. truth" = "prediction_bar",
      "Classification accuracy" = "confusion_heatmap",
      "Confidence in sample predictions - bar" = "confidence_bar",
      "Confidence in sample predictions - scatter" = "confidence_scatter"
    )
  } else {
    stop(sprintf("Task type '%s' not supported"))  
  }
  
  non_supported_plots <- setdiff(choices, c(slopeR:::CLASSIFICATION_PLOTS, slopeR:::REGRESSION_PLOTS))
  
  if (length(non_supported_plots) > 0) {
    warning(sprintf("Some plot types are not found in slopeR: %s", paste(non_supported_plots, collapse = ", ")))
    choices = intersect(choices, c(slopeR:::CLASSIFICATION_PLOTS, slopeR:::REGRESSION_PLOTS))
  }
  
  pickerInput(label = "Select vizualization type:", "super_plot_type", choices = choices)
})

# determine the split used
output$visualize_perf_split_ui <- renderUI({
  req(input$performance_tabset)
  has_test_preds <- if(input$performance_tabset == "Full model") {
    !is.null(attr(omicsData$objRM, "prediction_test"))
  } else if (input$performance_tabset == "Reduced model") {
    !is.null(attr(omicsData$objRM_reduced, "prediction_test"))
  }
  
  selected = if (has_test_preds) "test" else 'train'
  choicesOpt = list("disabled" = c(FALSE, !has_test_preds))
  
  pickerInput("visualize_perf_which_split",
              "Plot performance for which split:",
              choices = c("Training" = "train", "Testing" = "test"),
              selected = selected,
              choicesOpt = choicesOpt
              )
})

output[["VI_tabset_UI_collapse"]] <- renderUI({
  
  
  req(!is.null(omicsData$objRM) && !is.null(attr(omicsData$objRM, "vi_info")))
  
  collapseBox(
    
    titletext = "Visualize variable importance",
    collapsed = F,
    
    value = "results_VI",
    
    br(),
    
    uiOutput("VI_tabset_UI")
  )
  
})

output$Variable_importance_ui <- renderUI({
  req(!is.null(omicsData$objRM) && !is.null(attr(omicsData$objRM, "vi_info")))
  
  collapseBox(
    collapsed = F,
    value = "select_vi",

    "Post-hoc feature selection",
    
    radioButtons(
      "vi_choose",
      "Build reduced model which includes...",
      choiceNames = list(
        "top ___ features",
        "top ___% of features",
        "features with importance value above ___"
      ),
      choiceValues = c(
        "count",
        "percent",
        "value"
      )
    ),
    
    uiOutput("numeric_fill"),
    
    br(),
    
    textOutput("preview_n_features"),
    
    br(),
    
    hidden(div("Performing analysis, please wait...",
               id = "RM_busy_reduced",
               class = "fadein-out",
               style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
    )),

    actionButton("feature_select_posthoc",
                 "Build reduced model")
  )
  
})

output[["numeric_fill"]] <- renderUI({
  
  req(!is.null(omicsData$objRM) && !is.null(attr(omicsData$objRM, "vi_info")))
  
  vi_info <- attr(omicsData$objRM, "vi_info")
  
  set_val <- quantile(vi_info$var_import[vi_info$var_import != 0], .80)
  
  if(input$vi_choose == "count"){
    numericInput(
      "vi_thresh_count",
      "Count",
      min = 0, max = dim(vi_info)[1], 
      value = isolate(ifelse(is.null(input$vi_thresh_count), 10,
                             input$vi_thresh_count)),
      step = 1
    )
    
  } else if (input$vi_choose == "percent"){
    numericInput(
      "vi_thresh_pct",
      "Percent",
      min = 0, max = 100, 
      value = isolate(ifelse(is.null(input$vi_thresh_pct), 5,
                             input$vi_thresh_pct))
    )
  } else if (input$vi_choose == "value"){
    numericInput(
      "vi_thresh",
      "Value",
      min = 0, 
      value = isolate(ifelse(is.null(input$vi_thresh), signif(set_val),
                             input$vi_thresh))
    )
  }
  
})

output$preview_n_features <- renderText({
  req(!is.null(omicsData$objRM) && (
    !is.null(input$vi_thresh_count) || !is.null(input$vi_thresh_count) ||
      !is.null(input$vi_thresh)
  ))
  
  vi_info <- attr(omicsData$objRM, "vi_info")
  
  if(input$vi_choose == "count"){
    kept_features <- input$vi_thresh_count
    removed_features <- nrow(vi_info) - input$vi_thresh_count
  } else if(input$vi_choose == "percent") {
    kept_features <- round(nrow(vi_info)*input$vi_thresh_pct/100)
    removed_features <- nrow(vi_info) - kept_features
  } else if(input$vi_choose == "value"){
    vi_cutoff <- input$vi_thresh
    
    kept_features <- nrow(vi_info[vi_info$var_import >= vi_cutoff,])
    removed_features <- nrow(vi_info[vi_info$var_import < vi_cutoff,])
  }
  
  req(!is.null(kept_features) && !is.null(removed_features) && 
        !is.na(removed_features) && !is.na(kept_features))
  if (kept_features == 0 || removed_features == 0) {
    shinyjs::disable("feature_select_posthoc")
  } else {
    shinyjs::enable("feature_select_posthoc")
  }
  
  paste0("At current cut-off, ", kept_features, " features will be kept and ", 
         removed_features, " features will be excluded from the reduced model.")
  
})

observeEvent(c(input$vi_choose, input$vi_thresh_count), {
  req(input$vi_choose == "count")
  req(!is.null(input$vi_thresh_count))
  req(!is.null(omicsData$objRM))
  vi_info <- attr(omicsData$objRM, "vi_info")
  req(!is.null(vi_info))
  updateNumericInput(session, "vi_thresh", value = rev(sort(vi_info$var_import))[floor(input$vi_thresh_count)] - 0.0001)
}, ignoreInit = FALSE)

observeEvent(c(input$vi_choose, input$vi_thresh_pct), {
  req(input$vi_choose == "percent")
  req(!is.null(input$vi_thresh_pct))
  req(!is.null(omicsData$objRM))
  vi_info <- attr(omicsData$objRM, "vi_info")
  req(!is.null(vi_info))
  updateNumericInput(session, "vi_thresh", value = rev(sort(vi_info$var_import))[floor((input$vi_thresh_pct) / 100 * length(vi_info$var_import))] - 0.0001)
}, ignoreInit = FALSE)

output$VI_tabset_UI <- renderUI({
  
  if(is.null(omicsData$objRM)) return("Please run model to view variable importance")
  
  full_tabset <- tabPanel(
    
    "Full model",
    
    div(
      
      br(),
      plotlyOutput("Variable_importance_plot"),
      br(),
      
      sliderInput(
        "full_vi",
        "Show the top X features:",
                  min = 3,
                  max = min(nrow(attr(omicsData$objRM, "vi_info")), 50),
                  value = 10
      )
      
    )
    
  )
  
  reduced_tabset <- if(!is.null(omicsData$objRM_reduced)){
    
    tabPanel(
    
      "Reduced model",
      
    div(
      
      br(),
      plotlyOutput("Variable_importance_plot_reduced"),
      br(),
      
      sliderInput(
        "reduced_vi",
        "Show the top X features:",
        min = 3,
        max = min(nrow(attr(omicsData$objRM_reduced, "vi_info")), 50),
        value = 10
      )
      
    ))
    
  } else NULL
  
  do.call(tabsetPanel, list(id = "vi_plots", reduced_tabset, full_tabset))
  
})

output$Variable_importance_plot <- renderPlotly({
  
  req(!is.null(attr(omicsData$objRM, "feature_info")) &&
        !is.null(attr(omicsData$objRM, "vi_info")))
  
  plotting_df <- left_join(attr(omicsData$objRM, "feature_info"), 
                           attr(omicsData$objRM, "vi_info"), 
                           by = c(names_compact = "var_name"))
  
  plotting_df <- arrange(plotting_df, desc(var_import))
  plotting_df <- plotting_df[1:input$full_vi,]
  
  plotting_df$names_orig <- factor(plotting_df$names_orig, 
                                   levels = plotting_df$names_orig)
  
  req(!is.null(plotting_df) && nrow(plotting_df) > 0)
  
  p <- ggplot(plotting_df, aes(x = names_orig, y = var_import)) +
    geom_col() +
    ggplot2::theme_bw() + 
    ggplot2::labs(x = "", y = "Variable importance") +
    scale_x_discrete(label=function(x){
      map_chr(x, function(x2){
        if(nchar(x2) > 15){
          paste0(strtrim(x2, 15), "...")
        } else x2
      })
    }) +
    theme(
      # plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      # axis.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
  
  isolate(plot_table_current$table$RM__variable_importance__full <- p)
  isolate(table_table_current$table$RM__variable_importance__full <- plotting_df)
  
  p
  
})

output$Variable_importance_plot_reduced <- renderPlotly({
  
  req(!is.null(omicsData$objRM_reduced) && !is.null(input$reduced_vi))
  
  plotting_df <- left_join(attr(omicsData$objRM_reduced, "feature_info"), 
                           attr(omicsData$objRM_reduced, "vi_info"), 
                           by = c(names_compact = "var_name"))
  
  plotting_df <- arrange(plotting_df, desc(var_import))
  plotting_df <- plotting_df[1:input$reduced_vi,]
  
  plotting_df$names_orig <- factor(plotting_df$names_orig, 
                                   levels = plotting_df$names_orig)
  
  req(nrow(plotting_df) > 0)
  
  p <- ggplot(plotting_df, aes(x = names_orig, y = var_import)) +
    geom_col() +
    ggplot2::theme_bw() + 
    ggplot2::labs(x = "", y = "Variable importance") +
    scale_x_discrete(label=function(x){
      map_chr(x, function(x2){
        if(nchar(x2) > 15){
          paste0(strtrim(x2, 15), "...")
        } else x2
      })
    }) +
    theme(
      # plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      # axis.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
  
  isolate(plot_table_current$table$RM__variable_importance__reduced <- p)
  isolate(table_table_current$table$RM__variable_importance__reduced <- plotting_df)
  
  p
  
})

output$performance_tabset_UI <- renderUI({
  
  if(is.null(omicsData$objRM)) return("Please run model to see results")
  
  full_tabset <- tabPanel(
    
    "Full model",
    
    br(),
    
    withSpinner(plotlyOutput("performance_plot_RM")),
    br(),
    uiOutput("true_pos_picker_ui")
    
  )
  
  reduced_tabset <- if(!is.null(omicsData$objRM_reduced)){
    
    tabPanel(
      
      "Reduced model",
      
      br(),
      
      withSpinner(plotlyOutput("performance_plot_RM_reduced")),
      br(),
      uiOutput("true_pos_picker_ui_reduced")
      )
    
  } else NULL
  
  do.call(tabsetPanel, list(id = "performance_tabset", 
                            reduced_tabset,
                            full_tabset))

})

unsupervised_tab <- function() {
  
  div(
    # "Unsupervised",
    value = 'unsupervised_tab',
    class = "collapse_page",
    fluidRow(
      
      # Data upload column ---------------------------------------------------
      
      # Creates the column on the left-hand-side of the page for the
      # collapsible boxes to select files to upload along with their
      # corresponding options.
      column(
        # The width is 1/3 of the page for the method selection boxes and menus.
        4,
        # Creates a main box that is divided into multiple collapsible boxes.
        
        collapseBoxGroup(
          id = "sl_left",
          open = c("select_sl"),
          multiple = TRUE, # parent collapse div
          
          # Data type and file upload (e_data) ---------------
          collapseBox(
            collapsed = F,
            
            value = "select_sl",
            
            div(
              "Selected model summary",
              uiOutput("model_summary")
            ),
            
            div(),
            hr(),
            numericInput(
              "the_seed",
              "Random seed",
              value = 0,
              step = 1
            ),
            
            radioGroupButtons("pick_axis", "Look at structure of:",
                              choices = c("Samples" = "samples", 
                                          "Biomolecules" = "biomolecules")),
            
            "Note: Many biomolecules may take time to plot.",
            br(),
            br(),
            br(),
            
            hidden(div("Performing analysis, please wait...",
                       id = "RM_busy",
                       class = "fadein-out",
                       style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
            )),
            
            # disabled(
            actionButton(
              "run_sl",
              "Run model"#,
              # style = "primary"
            )
            # )
          )
        ),
        
        hidden(actionButton("complete_RM", "Complete analysis"))
        
      ), # Method selection column (width 4)
      
      # Data preview column --------------------------------------------------
      
      # Creates the column containing the Data Preview and the Boxplot Preview
      # tabs.
      column(
        8, # 2/3 of the page width
        collapseBoxGroup(
          id = "sl_preview_collapse",
          collapseBox(
            "Visualize model results",
            value = "unsup_plots",
            collapsed = F,
            
            br(),
            
            tabsetPanel(
              
              id = "performance_tabset",
              
              tabPanel(
                "Structure plot",
                # splitLayout(
                  # DTOutput("train_metrics"),
                br(),
                  withSpinner(plotlyOutput("structure_plot")),
                br(),
                column(6, uiOutput("unsup_res_aes_UI")),
                column(6, uiOutput("unsup_slider_UI"))
                
              )
              
              # tabPanel(
              #   "Feature importance",
              #   # splitLayout(
              #   #   DTOutput("test_metrics"),
              #   #   uiOutput("roc_plot_test")
              #   # )
              # )
            )
          )
        )
      ) # main column
    ) # fluidRow
  ) # tabPanel
  
}

observeEvent(input$run_sl, {
  
  ## Check normalization application
  shinyjs::show("RM_busy")
  
  on.exit({
    shinyjs::hide("RM_busy")
  })

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
    } else if (method == "pls") {
      custom_args <- list(
        num_comp = input$pls_num_comp,
        predictor_prop = input$pls_predictor_prop
      )
    }
    
    if(holdout_valid() && input$rm_prompts_hp == "tuned"){
      cvMethod <- input$cv_hp_option
      nFolds <- input$nFolds_hp
    } else if(holdout_valid() && input$rm_prompts_hp != "tuned"){
      cvMethod <- input$cv_perform_option
      nFolds <- input$nFolds_cv
    } else if(!holdout_valid() && input$rm_prompts_hp == "tuned"){
      cvMethod <- input$cv_hp_option
      nFolds <- input$nFolds_hp
    } else if(!holdout_valid() && input$rm_prompts_hp != "tuned"){
      cvMethod <- input$cv_perform_option
      nFolds <- input$nFolds_cv
    }
    
    list_args <- list(
      slData = runner,
      slMethod = method,
      cvMethod = cvMethod,
      nFolds = nFolds,
      pTest = ptest,
      return_cv = T
    )
    
    list_args <- c(list_args, custom_args)
    
    unregister()
    future::plan(future::sequential)
    omicsData$objRM <- tryCatch({
      do.call(slopeR::variable_importance, list_args)
    }, error = function(e){
      if(str_detect(e$message, "No variable importance method implemented for method")){
        do.call(slopeR::fit, list_args)
      } else {
        unregister()
        omicsData$objRM <- tryCatch({
          do.call(slopeR::variable_importance, list_args)
        }, error = function(e){
          if(str_detect(e$message, "No variable importance method implemented for method")){
            do.call(slopeR::fit, list_args)
          } else {
            shinyalert("Something went wrong: ", paste0("System error: ", e$message))
            NULL
          }
      })
      }
    })
    
  } else {

    runner <- as.slData(omicsData$objPP)


    # method <- models_long_name[input$pick_model_EM] ## While summary getting fixed

    method <- input$pick_model_EM

    if(method %in% c("hclust", "kmeans")){

      if(!is.null(input$height_clust) && input$height_clust == "Height"){
        num_clust <- NULL
        cut_height <- input$cut_height
      } else {
        num_clust <- input$num_clust
        cut_height <- NULL
      }

      linkage_method <- input$linkage_method

      omicsData$objRM <- slopeR:::cluster_unsup(runner,
                                                slMethod = method,
                                                axis = input$pick_axis,
                                                num_clusters = input$num_clust,

                                                ## Not supported at the moment
                                                #,
                                                cut_height = cut_height,
                                                linkage_method = linkage_method
      )

    } else {
      args = list(
        slData = runner,
        slMethod = method,
        axis = input$pick_axis,
        bake = F
      )
      
      if (method == 'pca') {
        args[['num_comp']] = input$pca_num_comp
      } else if (method == 'ppca') {
        args[['num_comp']] = input$ppca_num_comp
      }
      
      
      omicsData$objRM <- do.call(
        slopeR:::embed_unsup,
        args
      )

    }

  }
  
  
  shinyjs::show("complete_RM")

})


observeEvent(input$feature_select_posthoc, {
  
  ## Check normalization application
  shinyjs::show("RM_busy_reduced")
  
  on.exit({
    shinyjs::hide("RM_busy_reduced")
  })
  
  method <- input$pick_model_EM
  
  ## Do feature selection here, otherwise can't get vi_info from 
  # variable_importance since no viThreshold argument
  prior_info <- left_join(attr(omicsData$objRM, "feature_info"), 
            attr(omicsData$objRM, "vi_info"), 
            by = c(names_compact = "var_name"))
  
  prior_info <- arrange(prior_info, desc(var_import))

  if(input$vi_choose == "count"){
    e_data_keep <- prior_info$names_orig[1:input$vi_thresh_count]
  } else if(input$vi_choose == "percent"){
    numb <- round(length(prior_info$names_orig)*input$vi_thresh_pct/100)
    e_data_keep <- prior_info$names_orig[1:numb]
  } else {
    e_data_keep <- prior_info$names_orig[prior_info$var_import >= input$vi_thresh]
  }
    
  temp_omics <- omicsData$objPP
  
  temp_omics <- applyFilt(custom_filter(temp_omics, e_data_keep = e_data_keep),
                          temp_omics)
  
    ## Get correct response variable
    if(isTruthy(input$skip_ag)){
      response <- input$pick_model_group_pick
    } else {
      response <- input$f_data_response_picker
    }

    runner <- as.slData(temp_omics,
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
    
    if(holdout_valid() && input$rm_prompts_hp == "tuned"){
      cvMethod <- input$cv_hp_option
      nFolds <- input$nFolds_hp
    } else if(holdout_valid() && input$rm_prompts_hp != "tuned"){
      cvMethod <- input$cv_perform_option
      nFolds <- input$nFolds_cv
    } else if(!holdout_valid() && input$rm_prompts_hp == "tuned"){
      cvMethod <- input$cv_hp_option
      nFolds <- input$nFolds_hp
    } else if(!holdout_valid() && input$rm_prompts_hp != "tuned"){
      cvMethod <- input$cv_perform_option
      nFolds <- input$nFolds_cv
    }
    
    list_args <- list(
      slData = runner,
      slMethod = method,
      cvMethod = cvMethod,
      nFolds = nFolds,
      pTest = ptest,
      return_cv = T
    )
    
    list_args <- c(list_args, custom_args)
    
    unregister()
    omicsData$objRM_reduced <- do.call(slopeR::variable_importance, list_args)
  
    shinyjs::show("complete_RM")

})

#######

output$true_pos_picker_ui <- renderUI({
  req(input$super_plot_type %in% c("confidence_scatter"))
  
  if(isTruthy(input$skip_ag)){
    response <- input$pick_model_group_pick
  } else {
    response <- input$f_data_response_picker
  }
  
  levels_responses <- unique(omicsData$objPP$f_data[[response]])
  
  pickerInput("true_pos_picker", 
              "Designate true positive event", 
              choices = as.character(levels_responses)
              )
  
})

output$true_pos_picker_ui_reduced <- renderUI({
  req(input$super_plot_type %in% c("confidence_scatter"))
  
  if(isTruthy(input$skip_ag)){
    response <- input$pick_model_group_pick
  } else {
    response <- input$f_data_response_picker
  }
  
  levels_responses <- unique(omicsData$objPP$f_data[[response]])
  
  pickerInput("true_pos_picker_reduced", 
              "Designate true positive event", 
              choices = as.character(levels_responses)
              )
  
})

#### Performance Plots ####

#' Consolidated performance plot, takes in the input `super_plot_type` which maps to the `plotType` argument in `plot.slRes`.  Plot-type-specific adjustments are made in some cases.
output$performance_plot_RM <- renderPlotly({
  req(!is.null(omicsData$objRM))
  validate(need(input$visualize_perf_which_split, "Specify which data split to evaluate performance on"))
  validate(need(input$super_plot_type, "Specify which plot type"))
  
  p <- plot(omicsData$objRM, input$super_plot_type, split = input$visualize_perf_which_split)
  
  if (input$super_plot_type == "confidence_bar") {
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))
  }
  
  isolate(plot_table_current$table[[paste0("RM__model_eval__full__", input$super_plot_type)]] <- p)
  isolate(table_table_current$table[[paste0("RM__model_eval__full__", input$super_plot_type)]] <- p$data)

  return(p)
})

#' Consolidated performance plot for the reduced model, for comparison.  Takes in the input `super_plot_type` which maps to the `plotType` argument in `plot.slRes`.  Plot-type-specific adjustments are made in some cases.
output$performance_plot_RM_reduced <- renderPlotly({
  req(!is.null(omicsData$objRM_reduced))
  validate(need(input$visualize_perf_which_split, "Specify which data split to evaluate performance on"))
  validate(need(input$super_plot_type, "Specify which plot type"))
  
  p <- plot(omicsData$objRM_reduced, input$super_plot_type, split = input$visualize_perf_which_split)
  
  if (input$super_plot_type == "confidence_bar") {
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))
  }
  
  isolate(plot_table_current$table[[paste0("RM__model_eval__reduced__", input$super_plot_type)]] <- p)
  isolate(table_table_current$table[[paste0("RM__model_eval__reduced__", input$super_plot_type)]] <- p$data)

  return(p)
})

# 
# 
# output$test_plots <- renderPlotly({
#
#   req(!is.null(omicsData$objRM))
#
#   req(length(omicsData$objRM$fit$fit$lvl) == 2)
#
#   plot(omicsData$objRM, "roc_curve")
#   # plot( omicsData$objRM, "variable_importance")
#   plot( omicsData$objRM, plotType = "confidence_bar")
#   plot( omicsData$objRM, plotType = "prediction_bar")
#   plot( omicsData$objRM, plotType = "confusion_heatmap")
#   plot( omicsData$objRM, plotType = "confidence_scatter")
#
#   #
#   # #
#   # runner <- as.slData(omicsData$objPP,
#   #                     response_cols = response,
#   #                     response_types = rep(rt, length(response)))
#
#
#   # method <- models_long_name[input$pick_model_EM] ## While summary getting fixed
#
#   # "rf" - random forest,
#   # "lsvm" - linear support vector machine,
#   # "psvm" - polynomial support vector machine,
#   # "rsvm" - radial basis support vector machine,
#   # "logistic" - logistic regression,
#   # "loglasso" - logistic regression with LASSO,
#   # "multi" - multinomial regression, "multilasso" - multinomial regression with LASSO,
#   # "gbtree" - gradient boosting trees.
#
#   # omicsData$objRM <- slopeR::fit(runner,
#   #                                slMethod = method,
#   #                                # cvMethod = input$training_type,
#   #                                # nFolds = input$nFolds,
#   #                                nTest = 5
#   #                                # viThreshold = input$vi_thresh ## Need feature_selection results first??
#   # )
#
# })
# 
output$unsup_res_aes_UI <- renderUI({

  req(!is.null(input$pick_model_EM) && !supervised())

  method <- input$pick_model_EM ## While summary getting fixed

  element_list <- list()
  
  # req(method %in% c())

  if(input$pick_axis == "samples"){
    colors <- colnames(omicsData$objPP$f_data)
  } else {
    colors <- colnames(omicsData$objPP$e_meta)
  }

  colors <- colors[!(colors %in% c(get_fdata_cname(omicsData$objPP),
                                 get_edata_cname(omicsData$objPP)))]

  req(length(colors) > 0)
  
  choices <- if(method %in% c("pca", "ppca", "umap")){
    colors
  } else {
    c("Parameter clusters", colors)
  }

  color_by_picker <- pickerInput("color_by_unsup", "Color by:", choices = choices)
  
  element_list[[length(element_list) + 1]] <- color_by_picker
  ### plot the pcs
  
  if (inherits(omicsData$objRM$steps[[1]], c("step_pca", "step_ppca"))) {
    element_list[[length(element_list) + 1]] <- uiOutput("pc_xaxis_UI")
    element_list[[length(element_list) + 1]] <- uiOutput("pc_yaxis_UI")
  }
  
  return(do.call(tagList, element_list))
  
})

output$pc_xaxis_UI <- renderUI({
  req(inherits(omicsData$objRM$steps[[1]], c("step_pca", "step_ppca")))
  num_comp <- omicsData$objRM$steps[[1]]$num_comp
  
  choices = c(1:num_comp)
  
  picker_out <- pickerInput(
    "unsup_pca_xaxis_pc", "Plot which component on x-axis?" , choices = choices,
    selected = input$unsup_pca_xaxis_pc
  )
  
  return(picker_out)
})

output$pc_yaxis_UI <- renderUI({
  req(inherits(omicsData$objRM$steps[[1]], c("step_pca", "step_ppca")))
  num_comp <- omicsData$objRM$steps[[1]]$num_comp
  
  choices = c(1:num_comp)
  
  selected = if(!is.null(input$unsup_pca_yaxis_pc)) input$unsup_pca_yaxis_pc else choices[2] 
  
  picker_out <- pickerInput(
    "unsup_pca_yaxis_pc", "Plot which component on y-axis?", choices = choices,
    selected = selected
  )
  
  return(picker_out)
})

output$unsup_slider_UI <- renderUI({
  
  req(input$pick_model_EM == "hclust")
  
  sliderInput("expand_y", "Adjust sample name margin", min = 0, max = 1, value = 0.5)
  
})

# 
# ## Make plotly so we can hover over the points
# 
output$structure_plot <- renderPlotly({
  
  req(!is.null(input$pick_model_EM) && 
        !supervised())
  validate(
    need(!is.null(omicsData$objRM), 
         "No model results found.  Please run the model to see results plots."))

  method <- input$pick_model_EM ## While summary getting fixed

  color_by <- if(isTruthy(input$color_by_unsup) && input$color_by_unsup != "Parameter clusters") 
                sym(input$color_by_unsup) else NULL

  runner <- as.slData(omicsData$objPP)

  ## Allow coloring by fdata for axis = "samples" and emeta if axis = "biomolecules"

  if(method == "kmeans"){

    clusters <- tidyclust::extract_centroids(omicsData$objRM)
    
    ## How to snag contributing points out of this? Add hovers too
    clust <- omicsData$objRM$fit$fit$fit$cluster
    temp <- as.data.frame(omicsData$objRM$pre$mold$predictors)
    
    
    p <- fviz_cluster(omicsData$objRM$fit$fit$fit, data = temp,
                      # palette = c("#2E9FDF", "#00AFBB"),
                      geom = "point",
                      ellipse.type = "convex", 
                      ggtheme = theme_bw()
    )

  } else if (method == "hclust"){


    if(input$pick_axis == "samples"){
    ## hclust method needs to include sample names for plotting
    omicsData$objRM$fit$fit$fit$labels <- colnames(omicsData$objPP$e_data)[
      -which(colnames(omicsData$objPP$e_data) == get_edata_cname(omicsData$objPP))]

    } else {

      omicsData$objRM$fit$fit$fit$labels <- omicsData$objPP$e_data[get_edata_cname(omicsData$objPP)]

    }

    if(length(color_by) > 0){

      list_cols <- as.numeric(
        as.factor(omicsData$objPP$f_data[[as.character(color_by)]]))
      names(list_cols) <- omicsData$objPP$f_data[[get_fdata_cname(omicsData$objPP)]]
      
      if(!is.null(input$height_clust) && input$height_clust == "Height"){
        dend <- dendro_data_k(omicsData$objRM$fit$fit$fit, 
                              h = input$cut_height,
                              custom_color = list_cols)
      } else {
        dend <- dendro_data_k(omicsData$objRM$fit$fit$fit, 
                              k = input$num_clust,
                              custom_color = list_cols)
      }
      
    } else {
      
      if(!is.null(input$height_clust) && input$height_clust == "Height"){
        dend <- dendro_data_k(omicsData$objRM$fit$fit$fit, 
                              h = input$cut_height)
      } else {
        dend <- dendro_data_k(omicsData$objRM$fit$fit$fit, 
                              k = input$num_clust)
      }

    }
    
    p <- plot_ggdendro_multi(dend, branch.size = 0.5, expand.y = input$expand_y)

    # hclust_fit %>%
    #   extract_centroids(num_clusters = 2)

    # hclust_fit %>%
    #   extract_centroids(cut_height = 250)

  } else if (method %in% c("pca", "ppca")){
    args = list(
      slData = runner,
      axis = input$pick_axis,
      bake = T
    )
    
    if (method == 'pca') {
      args[['num_comp']] = input$pca_num_comp
    } else if (method == 'ppca') {
      args[['num_comp']] = input$ppca_num_comp
    }
    
    df <- do.call(
      slopeR:::embed_unsup,
      args
    )

    df <- as.data.frame(df)

    if(input$pick_axis == "samples"){
      df[[get_fdata_cname(runner)]] <- runner$f_data[[get_fdata_cname(runner)]]
      select_cols <- unique(c(get_fdata_cname(runner), input$color_by_unsup))
      df <- left_join(df, runner$f_data[select_cols])
    } else {
      df[[get_edata_cname(runner)]] <- runner$e_data[[get_edata_cname(runner)]]
      select_cols <- unique(c(get_edata_cname(runner), input$color_by_unsup))
      df <- left_join(df, runner$e_meta[select_cols])
    }
    
    xvar = if (!is.null(input$unsup_pca_xaxis_pc)) paste0("PC", input$unsup_pca_xaxis_pc) else "PC1"
    yvar =  if (!is.null(input$unsup_pca_yaxis_pc)) paste0("PC", input$unsup_pca_yaxis_pc) else "PC2"
    
    if(length(color_by) > 0){
      p <- ggplot(df, aes(x = .data[[xvar]], y = .data[[yvar]], color = !!color_by)) + 
               geom_point(size = 3) + theme_bw()
    } else {
      ## Where is R2?
      p <- ggplot(df, aes(x = .data[[xvar]], y = .data[[yvar]])) + geom_point(size = 3) + theme_bw()
    }

  } else {

    ## this seems off maybe
    df <- slopeR:::embed_unsup(runner,
                               slMethod = "umap",
                               axis = input$pick_axis,
                               bake = T
    )

    df <- as.data.frame(df)

    if(input$pick_axis == "samples"){
      df[[get_fdata_cname(runner)]] <- runner$f_data[[get_fdata_cname(runner)]]
      if(input$color_by_unsup != "Parameter clusters"){
        select_cols <- unique(c(get_fdata_cname(runner), input$color_by_unsup))
        df <- left_join(df, runner$f_data[select_cols])
      }
    } else {

      df[[get_edata_cname(runner)]] <- runner$e_data[[get_edata_cname(runner)]]
      if(input$color_by_unsup != "Parameter clusters"){
        select_cols <- unique(c(get_edata_cname(runner), input$color_by_unsup))
        df <- left_join(df, runner$e_meta[select_cols])
      }
    }

    if(length(color_by) > 0){
      p <- ggplot(df, aes(x = UMAP1, y = UMAP2, color = !!color_by)) + 
               geom_point(size = 3) + theme_bw()
    } else {
      ## Where is R2?
      p <- ggplot(df, aes(x = UMAP1, y = UMAP2)) + 
               geom_point(size = 3) + theme_bw()
    }

  }
  
  isolate(plot_table_current$table[[paste0("RM__model_eval__", method)]] <- p)
  isolate(plot_table_current$names[[paste0("RM__model_eval__", method)]] <- paste0("Model evaluation: ", method))
  isolate(table_table_current$table$RM__model_eval <- p$data)

  p
})
