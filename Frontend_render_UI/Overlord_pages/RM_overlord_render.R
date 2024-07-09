
output$RM_ui <- renderUI({
  
  if(supervised()){
    
    div(
      fluidRow(
        align = "center",
        
        column(1, ""), ## spacer
        
        column(1, 
               actionBttn(
                 inputId = "show_model_options",
                 label = HTML("Model<br/>Options"),
                 style = "jelly", 
                 color = "default",
                 size = "s"
               )
        ),
        
        column(
          1,
          br(),
          shinyWidgets::progressBar(id = "MO_done", value = 0, size = "xs")
        ),
        
        
        column(1, 
               disabled(actionBttn(
                 inputId = "show_TrainSize",
                 label = HTML("Training<br/>Structure<br/>"),
                 style = "jelly", 
                 color = "default",
                 size = "s"
               ))
        ),
        
        column(
          1,
          br(),
          shinyWidgets::progressBar(id = "TS_done", value = 0, size = "xs")
        ),
        
        column(1, 
               # disabled(
               actionBttn(
                 inputId = "show_parameters",
                 label = HTML("Parameter<br/>Optimization"),
                 style = "jelly", 
                 color = "default",
                 size = "s"
               )
               # )
        ),
        
        column(
          1,
          br(),
          shinyWidgets::progressBar(id = "params_done", value = 0, size = "xs")
        ),
        
        
        column(1, 
               # disabled(
               actionBttn(
                 inputId = "show_runmodel",
                 label = HTML("Model<br/>Evaluation"),
                 style = "jelly", 
                 color = "default",
                 size = "s"
               )
               # )
        ),
        
        column(
          1,
          br(),
          shinyWidgets::progressBar(id = "RM_done", value = 0, size = "xs")
        ),
        
        column(1, 
               disabled(
               actionBttn(
                 inputId = "review_RM",
                 label = HTML("Review<br/>Results"),
                 style = "jelly", 
                 color = "default",
                 size = "s"
               )
               )
        ),
        
        column(1, "") ## spacer
        
      ),
      
      fluidRow(
        
        column(
          12,
          
          br(),
          
          div(
            id = "rm_prompt_box",
            
            br(),
            Prompt_RM_UI()
            # TS_RM_UI()
            
          ),
          
          hidden(div(
            id = "train_box",
            
            br(),
            TS_RM_UI()
            
          )),
          
          hidden(div(
            id = "param_box",
            
            
            br(),
            param_RM_UI()
            
          )),
          
          hidden(div(
            id = "RM_box",
            
            br(),
            uiOutput("RM_tab_UI")
            # norm_tab()
            
          )),
          
          hidden(div(
            id = "RM_result_box",
            
            
            br(),
            progress_tab(
              "RM",
              plot_choices = c(
                "Recommended Folds" = "RM__rec_folds",
                "Parameter Optimization" = "RM__param_optim",
                "Training Structure" = "RM__training_structure",
                "Model Evaluation: ROC Curve (full)" = "RM__model_eval__full__roc_curve",
                "Model Evaluation: Confidence Bar (full)" = "RM__model_eval__full__confidence_bar",
                "Model Evaluation: Prediction Bar (full)" = "RM__model_eval__full__prediction_bar",
                "Model Evaluation: Confusion Heatmap (full)" = "RM__model_eval__full__confusion_heatmap",
                "Model Evaluation: Confidence Scatter (full)" = "RM__model_eval__full__confidence_scatter",
                "Model Evaluation: ROC Curve (reduced)" = "RM__model_eval__reduced__roc_curve",
                "Model Evaluation: Confidence Bar (reduced)" = "RM__model_eval__reduced__confidence_bar",
                "Model Evaluation: Prediction Bar (reduced)" = "RM__model_eval__reduced__prediction_bar",
                "Model Evaluation: Confusion Heatmap (reduced)" = "RM__model_eval__reduced__confusion_heatmap",
                "Model Evaluation: Confidence Scatter (reduced)" = "RM__model_eval__reduced__confidence_scatter",
                "Variable Importance (full)" = "RM__variable_importance__full",
                "Variable Importance (reduced) " = "RM__variable_importance__reduced"
              ),
              done_btn = actionButton("complete_results_review", "Continue to Download"),
              reset_btn = actionButton("reset_rm", "Revert to start of Run Model")
            ),
            
          ))
          
        )
      )
    )
    
  } else {
    
    div(
      fluidRow(
        align = "center",
        
        column(2, ""), ## spacer
        
        column(1, 
               actionBttn(
                 inputId = "show_model_options",
                 label = HTML("Model<br/>Options"),
                 style = "jelly", 
                 color = "default",
                 size = "s"
               )
        ),
        
        column(
          1,
          br(),
          shinyWidgets::progressBar(id = "MO_done", value = 0, size = "xs")
        ),
        
        column(1, 
               actionBttn(
                 inputId = "show_parameters",
                 label = HTML("Parameter<br/>Optimization"),
                 style = "jelly", 
                 color = "default",
                 size = "s"
               )
        ),
        
        column(
          1,
          br(),
          shinyWidgets::progressBar(id = "params_done", value = 0, size = "xs")
        ),
        
        
        column(1, 
               disabled(
               actionBttn(
                 inputId = "show_runmodel",
                 label = HTML("Model<br/>Evaluation"),
                 style = "jelly", 
                 color = "default",
                 size = "s"
               )
               )
        ),
        
        column(
          1,
          br(),
          shinyWidgets::progressBar(id = "RM_done", value = 0, size = "xs")
        ),
        
        column(1, 
               disabled(
               actionBttn(
                 inputId = "review_RM",
                 label = HTML("Review<br/>Results"),
                 style = "jelly", 
                 color = "default",
                 size = "s"
               )
               )
        ),
        
        column(2, "") ## spacer
        
      ),
      
      fluidRow(
        
        column(
          12,
          
          br(),
          
          div(
            id = "rm_prompt_box",
            br(),
            Prompt_RM_UI_unsup()
            
          ),
          
          hidden(div(
            id = "param_box",
            
            br(),
            param_RM_UI()
            
          )),
          
          hidden(div(
            id = "RM_box",
            
            br(),
            uiOutput("RM_tab_UI")
            
          )),
          
          hidden(div(
            id = "RM_result_box",
            
            br(),
            progress_tab(
              "RM",
              done_btn = actionButton("complete_results_review", "Continue to Download"),
              reset_btn = actionButton("reset_rm", "Revert to start of Run Model")
            ),
            
          ))
          
        )
      )
    )
    
  }
  
  
  
  
  
})

## upload overlord observers

observeEvent(input$show_model_options, ignoreInit = F, ignoreNULL = F, {
  
  shinyjs::show(id = "rm_prompt_box")
  shinyjs::hide(id = "train_box")
  shinyjs::hide(id = "param_box")
  shinyjs::hide(id = "RM_box")
  shinyjs::hide(id = "RM_result_box")
  
  shinyjs::addClass("show_model_options", "blueoutline")
  shinyjs::removeClass("show_TrainSize", "blueoutline")
  shinyjs::removeClass("show_parameters", "blueoutline")
  shinyjs::removeClass("show_runmodel", "blueoutline")
  shinyjs::removeClass("review_RM", "blueoutline")
  
})

observeEvent(input$show_TrainSize, {
  
    shinyjs::hide(id = "rm_prompt_box")
    shinyjs::show(id = "train_box")
    shinyjs::hide(id = "param_box")
    shinyjs::hide(id = "RM_box")
    shinyjs::hide(id = "RM_result_box")
    
    
    shinyjs::removeClass("show_model_options", "blueoutline")
    shinyjs::addClass("show_TrainSize", "blueoutline")
    shinyjs::removeClass("show_parameters", "blueoutline")
    shinyjs::removeClass("show_runmodel", "blueoutline")
    shinyjs::removeClass("review_RM", "blueoutline")

})

observeEvent(input$show_parameters,
             {

    shinyjs::hide(id = "rm_prompt_box")
    shinyjs::hide(id = "train_box")
    shinyjs::show(id = "param_box")
    shinyjs::hide(id = "RM_box")
    shinyjs::hide(id = "RM_result_box")
    
    shinyjs::removeClass("show_model_options", "blueoutline")
    shinyjs::removeClass("show_TrainSize", "blueoutline")
    shinyjs::addClass("show_parameters", "blueoutline")
    shinyjs::removeClass("show_runmodel", "blueoutline")
    shinyjs::removeClass("review_RM", "blueoutline")

  
})

observeEvent(input$show_runmodel, {
  
  shinyjs::hide(id = "rm_prompt_box")
  shinyjs::hide(id = "train_box")
  shinyjs::hide(id = "param_box")
  shinyjs::show(id = "RM_box")
  shinyjs::hide(id = "RM_result_box")
  
  shinyjs::removeClass("show_model_options", "blueoutline")
  shinyjs::removeClass("show_TrainSize", "blueoutline")
  shinyjs::removeClass("show_parameters", "blueoutline")
  shinyjs::addClass("show_runmodel", "blueoutline")
  shinyjs::removeClass("review_RM", "blueoutline")
  
})

observeEvent(input$review_RM, {
  
  shinyjs::hide(id = "rm_prompt_box")
  shinyjs::hide(id = "train_box")
  shinyjs::hide(id = "param_box")
  shinyjs::hide(id = "RM_box")
  shinyjs::show(id = "RM_result_box")
  
  shinyjs::removeClass("show_model_options", "blueoutline")
  shinyjs::removeClass("show_TrainSize", "blueoutline")
  shinyjs::removeClass("show_parameters", "blueoutline")
  shinyjs::removeClass("show_runmodel", "blueoutline")
  shinyjs::addClass("review_RM", "blueoutline")
  
})

observeEvent(input$complete_RM_prompts, ignoreInit = T, {
  
  if(!is.null(input$complete_RM_prompts) && input$complete_RM_prompts > 0){
    enable("show_TrainSize")
    # disable("show_parameters")
    # disable("show_runmodel")
    disable("review_RM")
  }
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F,
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 
                 if(input$pick_model_EM %in% models_unsupervised){
                   shinyjs::hide(id = "rm_prompt_box")
                   shinyjs::hide(id = "train_box")
                   shinyjs::show(id = "param_box")
                   shinyjs::hide(id = "RM_box")
                   shinyjs::hide(id = "RM_result_box")
                   
                   
                   shinyjs::removeClass("show_model_options", "blueoutline")
                   shinyjs::removeClass("show_TrainSize", "blueoutline")
                   shinyjs::addClass("show_parameters", "blueoutline")
                   shinyjs::removeClass("show_runmodel", "blueoutline")
                   shinyjs::removeClass("review_RM", "blueoutline")
                 } else {
                   shinyjs::hide(id = "rm_prompt_box")
                   shinyjs::show(id = "train_box")
                   shinyjs::hide(id = "param_box")
                   shinyjs::hide(id = "RM_box")
                   shinyjs::hide(id = "RM_result_box")
                   
                   
                   shinyjs::removeClass("show_model_options", "blueoutline")
                   shinyjs::addClass("show_TrainSize", "blueoutline")
                   shinyjs::removeClass("show_parameters", "blueoutline")
                   shinyjs::removeClass("show_runmodel", "blueoutline")
                   shinyjs::removeClass("review_RM", "blueoutline")
                 }
               }
             })
  
  updateProgressBar(session, "MO_done", value = 100)
  updateProgressBar(session, "TS_done", value = 0)
  updateProgressBar(session, "params_done", value = 0)
  updateProgressBar(session, "RM_done", value = 0)
  
})

observeEvent(input$complete_TS_RM, ignoreInit = T, {

  if(!is.null(input$complete_TS_RM) && input$complete_TS_RM > 0){
    enable("show_parameters")
    # disable("show_runmodel")
    disable("review_RM")
  }


  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F,
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 shinyjs::hide(id = "rm_prompt_box")
                 shinyjs::hide(id = "train_box")
                 shinyjs::show(id = "param_box")
                 shinyjs::hide(id = "RM_box")
                 shinyjs::hide(id = "RM_result_box")
                 
                 shinyjs::removeClass("show_model_options", "blueoutline")
                 shinyjs::removeClass("show_TrainSize", "blueoutline")
                 shinyjs::addClass("show_parameters", "blueoutline")
                 shinyjs::removeClass("show_runmodel", "blueoutline")
                 shinyjs::removeClass("review_RM", "blueoutline")
                 
               }
             })

  updateProgressBar(session, "TS_done", value = 100)
  updateProgressBar(session, "params_done", value = 0)
  updateProgressBar(session, "RM_done", value = 0)

})

observeEvent(input$complete_param, ignoreInit = T, {
  
  if(!is.null(input$complete_param) && input$complete_param > 0){
    enable("show_runmodel")
    disable("review_RM")
  }
  
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F,
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 shinyjs::hide(id = "rm_prompt_box")
                 shinyjs::hide(id = "train_box")
                 shinyjs::hide(id = "param_box")
                 shinyjs::show(id = "RM_box")
                 shinyjs::hide(id = "RM_result_box")
                 
                 shinyjs::removeClass("show_model_options", "blueoutline")
                 shinyjs::removeClass("show_TrainSize", "blueoutline")
                 shinyjs::removeClass("show_parameters", "blueoutline")
                 shinyjs::addClass("show_runmodel", "blueoutline")
                 shinyjs::removeClass("review_RM", "blueoutline")
               }
             })
  
  updateProgressBar(session, "params_done", value = 100)
  updateProgressBar(session, "RM_done", value = 0)
  
})

observeEvent(input$complete_RM, ignoreInit = T, {
  
  if(!is.null(input$complete_RM) && input$complete_RM > 0){
    enable("review_RM")
  }
  
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F,
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 shinyjs::hide(id = "rm_prompt_box")
                 shinyjs::hide(id = "train_box")
                 shinyjs::hide(id = "param_box")
                 shinyjs::hide(id = "RM_box")
                 shinyjs::show(id = "RM_result_box")
                 
                 shinyjs::removeClass("show_model_options", "blueoutline")
                 shinyjs::removeClass("show_TrainSize", "blueoutline")
                 shinyjs::removeClass("show_parameters", "blueoutline")
                 shinyjs::removeClass("show_runmodel", "blueoutline")
                 shinyjs::addClass("review_RM", "blueoutline")
               }
             })
  
  updateProgressBar(session, "RM_done", value = 100)
  
})


observeEvent(input$complete_results_review, ignoreInit = T, {

  if(!is.null(input$complete_results_review) && input$complete_results_review > 0){
    updateNavbarPage(session, "top_page", "Download")
  }

})
