output[["RM_tab_UI"]] <- renderUI({

  req(!is.null(input$ag_prompts))
  
  if(input$ag_prompts == "supervised"){
    supervised_tab()
  } else {
    unsupervised_tab()
  }

})

supervised_tab <- function() {
  
  tabPanel(
    "Supervised",
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
            
            value = "select_sl",
            
            div(
              "Selected model",
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
            
            br(),
            
            uiOutput("performance_tabset_UI")
          ),
          
          uiOutput("VI_tabset_UI_collapse")
        
        )
      ) # main column
    ) # fluidRow
  ) # tabPanel
  
}

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
  
  vi_info <- attr(omicsData$objRM, "vi_info")
  
  set_val <- quantile(vi_info$var_import[vi_info$var_import != 0], .80)
  
  collapseBox(
    collapsed = F,
    value = "select_vi",

    "Post-hoc feature selection",

    numericInput(
      "vi_thresh",
      "Build reduced model with variable importance equal to or above:",
      min = 0, max = max(vi_info$var_import), value = signif(set_val, 3),
      step = 0.001,
      width = "100%"
    ),
    
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

output$preview_n_features <- renderText({
  req(!is.null(omicsData$objRM) && !is.null(input$vi_thresh))
  
  vi_cutoff <- input$vi_thresh
  vi_info <- attr(omicsData$objRM, "vi_info")
  
  kept_features <- nrow(vi_info[vi_info$var_import >= vi_cutoff,])
  removed_features <- nrow(vi_info[vi_info$var_import < vi_cutoff,])
  
  paste0("At current cut-off, ", kept_features, " features will be kept and ", 
         removed_features, " features will be excluded from the reduced model.")
  
})

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
  
  do.call(tabsetPanel, list(id = "vi_plots", full_tabset, reduced_tabset))
  
})

output$Variable_importance_plot <- renderPlotly({
  
  plotting_df <- left_join(attr(omicsData$objRM, "feature_info"), 
                           attr(omicsData$objRM, "vi_info"), 
                           by = c(names_compact = "var_name"))
  
  plotting_df <- arrange(plotting_df, desc(var_import))
  plotting_df <- plotting_df[1:input$full_vi,]
  
  plotting_df$names_orig <- factor(plotting_df$names_orig, 
                                   levels = plotting_df$names_orig)
  
  ggplot(plotting_df, aes(x = names_orig, y = var_import)) +
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
  
})

output$Variable_importance_plot_reduced <- renderPlotly({
  
  plotting_df <- left_join(attr(omicsData$objRM_reduced, "feature_info"), 
                           attr(omicsData$objRM_reduced, "vi_info"), 
                           by = c(names_compact = "var_name"))
  
  plotting_df <- arrange(plotting_df, desc(var_import))
  plotting_df <- plotting_df[1:input$reduced_vi,]
  
  plotting_df$names_orig <- factor(plotting_df$names_orig, 
                                   levels = plotting_df$names_orig)
  
  ggplot(plotting_df, aes(x = names_orig, y = var_import)) +
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
  
})

output$performance_tabset_UI <- renderUI({
  
  out <- tabsetPanel(
    
    id = "performance_tabset",
    
    # tabPanel(
    #   "Training Performance Metrics",
    #   splitLayout(
    #     DTOutput("train_metrics"),
    #     uiOutput("roc_plot_train")
    #   )
    # ),
    
    # tabPanel(
    #   "Testing Performance Metrics",
    #   splitLayout(
    #     DTOutput("test_metrics"),
    #     uiOutput("roc_plot_test")
    #   )
    # ),
    
    # tabPanel(
    #   "Data comparison -- Training vs. Testing",
    #   # splitLayout(
    #   #   DTOutput("test_metrics"),
    #   #   uiOutput("roc_plot_test")
    #   # )
    # ),
    
    tabPanel(
      "True positive performance",
      # splitLayout(
      #   DTOutput("test_metrics"),
      #   uiOutput("roc_plot_test")
      # )
      br(),
      withSpinner(plotOutput("roc_curve"))
    ),
    
    tabPanel(
      "Prediction vs. truth",
      # splitLayout(
      #   DTOutput("test_metrics"),
      #   uiOutput("roc_plot_test")
      # )
      br(),
      withSpinner(plotOutput("prediction_bar"))
    ),
    
    tabPanel(
      "Classification accuracy",
      # splitLayout(
      #   DTOutput("test_metrics"),
      #   uiOutput("roc_plot_test")
      # )
      br(),
      withSpinner(plotOutput("confusion_heatmap"))
    ),
    
    tabPanel(
      "Confidence in sample predictions - bar",
      # splitLayout(
      #   DTOutput("test_metrics"),
      #   uiOutput("roc_plot_test")
      # )
      br(),
      withSpinner(plotOutput("confidence_bar"))
    ),
    
    tabPanel(
      "Confidence in sample predictions - scatter",
      # splitLayout(
      #   DTOutput("test_metrics"),
      #   uiOutput("roc_plot_test")
      # )
      br(),
      withSpinner(plotOutput("confidence_scatter")),
      br(),
      uiOutput("true_pos_picker_ui")
    )
  )
  
  if(is.null(omicsData$objRM)){
    out <- "Please run model to see results"
  }
  out
})

unsupervised_tab <- function() {
  
  tabPanel(
    "Unsupervised",
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
                  withSpinner(plotOutput("structure_plot")),
                br(),
                column(6, uiOutput("unsup_picker_UI")),
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

    ## Run with correct response variable type
    class_responses <- apply(omicsData$objPP$f_data[response], 2, class)
    rt <- if(all(class_responses %in% c("factor", "character"))) "categorical" else "continuous"

    runner <- as.slData(omicsData$objPP,
                        response_cols = response,
                        response_types = rep(rt, length(response)))

    ## Get correct train/test split
    if(!is.null(input$numb_test)){
      if(input$numb_test == "Proportion"){
        ntest <- floor(input$nTest_prop * ncol(runner$e_data[-1]))
      } else {
        ntest <- input$nTest_count
      }
    } else ntest <- 0

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
      nTest = ntest
    )
    
    list_args <- c(list_args, custom_args)
    
    omicsData$objRM <- tryCatch({
      do.call(slopeR::variable_importance, list_args)
    }, error = function(e){
      if(str_detect(e$message, "No variable importance method implemented for method")){
        do.call(slopeR::fit, list_args)
      } else {
        browser()
        NULL
      }
    })
    
    # omicsData$objRM <- slopeR::fit(runner,
    #                           slMethod = method,
    #                           cvMethod = input$training_type,
    #                           nFolds = input$nFolds,
    #                           nTest = ntest
    #                           )

    # feature_selection(runner,
    #                   slMethod = method,
    #                   cvMethod = input$training_type,
    #                   nFolds = input$nFolds,
    #                   nTest = 9
    #                   )
    #
    # slopeR::fit(runner,
    #             slMethod = method,
    #             slRes = omicsData$objRM,
    #             cvMethod = input$training_type,
    #             nFolds = input$nFolds,
    #             viThreshold = input$vi_thresh ## Need results first??
    # )
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

      omicsData$objRM <- slopeR:::embed_unsup(runner,
                                              slMethod = method,
                                              axis = "samples", ## should be able to change this
                                              bake = F
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
  e_data_keep <- prior_info$names_orig[prior_info$var_import >= input$vi_thresh]
    
  temp_omics <- omicsData$objPP
  
  temp_omics <- applyFilt(custom_filter(temp_omics, e_data_keep = e_data_keep),
                          temp_omics)
  
    ## Get correct response variable
    if(isTruthy(input$skip_ag)){
      response <- input$pick_model_group_pick
    } else {
      response <- input$f_data_response_picker
    }
    
    ## Run with correct response variable type
    class_responses <- apply(temp_omics$f_data[response], 2, class)
    rt <- if(all(class_responses %in% c("factor", "character"))) "categorical" else "continuous"
    
    runner <- as.slData(temp_omics,
                        response_cols = response,
                        response_types = rep(rt, length(response)))
    
    ## Get correct train/test split
    if(!is.null(input$numb_test)){
      if(input$numb_test == "Proportion"){
        ntest <- floor(input$nTest_prop * ncol(runner$e_data[-1]))
      } else {
        ntest <- input$nTest_count
      }
    } else ntest <- 0
    
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
      nTest = ntest
    )
    
    list_args <- c(list_args, custom_args)
    
    omicsData$objRM_reduced <- do.call(slopeR::variable_importance, list_args)
  
    shinyjs::show("complete_RM")

})

output$true_pos_picker_ui <- renderUI({
  
  if(isTruthy(input$skip_ag)){
    response <- input$pick_model_group_pick
  } else {
    response <- input$f_data_response_picker
  }
  
  levels_responses <- unique(omicsData$objPP$f_data[[response]])
  
  pickerInput("true_pos_picker", 
              "Designate true positive event", 
              choices = as.character(levels_responses))
  
})

output$roc_curve <- renderPlot({

  req(!is.null(omicsData$objRM))

  plot(omicsData$objRM, "roc_curve", pos_class = input$true_pos_picker)

})



output$confidence_bar <- renderPlot({

  req(!is.null(omicsData$objRM))

  plot( omicsData$objRM, plotType = "confidence_bar") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))

})

output$prediction_bar <- renderPlot({

  req(!is.null(omicsData$objRM))

  plot( omicsData$objRM, plotType = "prediction_bar")

})

output$confusion_heatmap <- renderPlot({

  req(!is.null(omicsData$objRM))

  plot( omicsData$objRM, plotType = "confusion_heatmap")

})

output$confidence_scatter <- renderPlot({

  req(!is.null(omicsData$objRM))

  plot( omicsData$objRM, plotType = "confidence_scatter", pos_class = input$true_pos_picker)

})
# 
# 
# output$test_plots <- renderPlot({
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
output$unsup_picker_UI <- renderUI({

  req(!is.null(input$pick_model_EM) && input$ag_prompts != "supervised")

  method <- input$pick_model_EM ## While summary getting fixed

  # req(method %in% c())

  if(input$pick_axis == "samples"){
    colors <- colnames(omicsData$objPP$f_data)
  } else {
    colors <- colnames(omicsData$objPP$e_meta)
  }

  colors <- colors[!(colors %in% c(get_fdata_cname(omicsData$objPP),
                                 get_edata_cname(omicsData$objPP)))]

  req(length(colors) > 0)
  
  choices <- if(method == "pca"){
    colors
  } else {
    c("Parameter clusters", colors)
  }

  pickerInput("color_by_unsup", "Color by:", choices = choices)

})

output$unsup_slider_UI <- renderUI({
  
  req(input$pick_model_EM == "hclust")
  
  sliderInput("expand_y", "Adjust sample name margin", min = 0, max = 1, value = 0.5)
  
})

# 
# ## Make plotly so we can hover over the points
# 
output$structure_plot <- renderPlot({
  
  req(!is.null(input$pick_model_EM) && 
        input$ag_prompts != "supervised" && 
        !is.null(omicsData$objRM))

  method <- input$pick_model_EM ## While summary getting fixed

  color_by <- if(input$color_by_unsup != "Parameter clusters") sym(input$color_by_unsup) else NULL

  runner <- as.slData(omicsData$objPP)

  ## Allow coloring by fdata for axis = "samples" and emeta if axis = "biomolecules"

  if(method == "kmeans"){

    clusters <- tidyclust::extract_centroids(omicsData$objRM)
    
    ## How to snag contributing points out of this? Add hovers too
    clust <- omicsData$objRM$fit$fit$fit$cluster
    temp <- as.data.frame(omicsData$objRM$pre$mold$predictors)
    
    return(fviz_cluster(omicsData$objRM$fit$fit$fit, data = temp,
                 # palette = c("#2E9FDF", "#00AFBB"),
                 geom = "point",
                 ellipse.type = "convex", 
                 ggtheme = theme_bw()
    ))
    

    # centers <- tibble(
    #   cluster = factor(1:3),
    #   num_points = c(100, 150, 50),  # number points in each cluster
    #   x1 = c(5, 0, -3),              # x1 coordinate of cluster center
    #   x2 = c(-1, 1, -2)              # x2 coordinate of cluster center
    # )
    # 
    # labelled_points <-
    #   centers %>%
    #   mutate(
    #     x1 = map2(num_points, x1, rnorm),
    #     x2 = map2(num_points, x2, rnorm)
    #   ) %>%
    #   select(-num_points) %>%
    #   unnest(cols = c(x1, x2))
    # 
    # ggplot(labelled_points, aes(x1, x2, color = cluster)) +
    #   geom_point(alpha = 0.3) + theme_bw() + theme(text = element_text(size = 12))

  } else if (method == "hclust"){


    if(input$pick_axis == "samples"){
    ## hclust method needs to include sample names for plotting
    omicsData$objRM$fit$fit$fit$labels <- colnames(omicsData$objPP$e_data)[-which(colnames(omicsData$objPP$e_data) == get_edata_cname(omicsData$objPP))]

    } else {

      omicsData$objRM$fit$fit$fit$labels <- omicsData$objPP$e_data[get_edata_cname(omicsData$objPP)]

    }

    if(length(color_by) > 0){

      list_cols <- as.numeric(as.factor(omicsData$objPP$f_data[[as.character(color_by)]]))
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
    
    return(plot_ggdendro_multi(dend, branch.size = 0.5, expand.y = input$expand_y))
    # hclust_fit %>%
    #   extract_centroids(num_clusters = 2)

    # hclust_fit %>%
    #   extract_centroids(cut_height = 250)

  } else if (method == "pca"){

    ## this seems off maybe
    df <- slopeR:::embed_unsup(runner,
                         slMethod = "pca",
                         axis = input$pick_axis,
                         bake = T
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

    if(length(color_by) > 0){
      return(ggplot(df, aes(x = PC1, y = PC2, color = !!color_by)) + 
               geom_point(size = 3) + theme_bw())
    } else {
      ## Where is R2?
      return(ggplot(df, aes(x = PC1, y = PC2)) + geom_point(size = 3) + theme_bw())
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
      select_cols <- unique(c(get_fdata_cname(runner), input$color_by_unsup))
      df <- left_join(df, runner$f_data[select_cols])
    } else {

      df[[get_edata_cname(runner)]] <- runner$e_data[[get_edata_cname(runner)]]
      select_cols <- unique(c(get_edata_cname(runner), input$color_by_unsup))
      df <- left_join(df, runner$e_meta[select_cols])
    }

    if(length(color_by) > 0){
      return(ggplot(df, aes(x = UMAP1, y = UMAP2, color = !!color_by)) + 
               geom_point(size = 3) + theme_bw())
    } else {
      ## Where is R2?
      return(ggplot(df, aes(x = UMAP1, y = UMAP2)) + 
               geom_point(size = 3) + theme_bw())
    }

  }

})
