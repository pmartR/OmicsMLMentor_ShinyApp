
output$RM_ui <- renderUI({
  
  if(input$ag_prompts == "supervised"){
    
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
          progressBar(id = "MO_done", value = 0, size = "xs")
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
          progressBar(id = "TS_done", value = 0, size = "xs")
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
          progressBar(id = "params_done", value = 0, size = "xs")
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
          progressBar(id = "RM_done", value = 0, size = "xs")
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
            
            # box(
            #   # id = "experimental_upload_box",
            #   title = "Specify Training/Testing set-up",
            #   width = 12,
            #   status = "primary",
            #   headerBorder = F
            # ),
            
            br(),
            Prompt_RM_UI()
            # TS_RM_UI()
            
          ),
          
          hidden(div(
            id = "train_box",
            
            # box(
            #   # id = "experimental_upload_box",
            #   title = "Specify Training/Testing set-up",
            #   width = 12,
            #   status = "primary",
            #   headerBorder = F
            # ),
            
            br(),
            TS_RM_UI()
            
          )),
          
          hidden(div(
            id = "param_box",
            
            # box(
            #   # id = "experimental_upload_box",
            #   title = "Parameter Optimization",
            #   width = 12,
            #   status = "primary",
            #   headerBorder = F
            # ),
            
            br(),
            param_RM_UI()
            
          )),
          
          hidden(div(
            id = "RM_box",
            
            # box(
            #   # id = "experimental_upload_box",
            #   title = "Evaluate Model",
            #   width = 12,
            #   status = "primary",
            #   headerBorder = F
            # ),
            
            br(),
            uiOutput("RM_tab_UI")
            # norm_tab()
            
          )),
          
          hidden(div(
            id = "RM_result_box",
            
            # box(
            #   # id = "experimental_upload_box",
            #   title = "Review Results",
            #   width = 12,
            #   status = "primary",
            #   headerBorder = F
            # ),
            
            br(),
            progress_tab("results_review"),
            
            actionButton("complete_results_review", "Done")
            
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
          progressBar(id = "MO_done", value = 0, size = "xs")
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
          progressBar(id = "params_done", value = 0, size = "xs")
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
          progressBar(id = "RM_done", value = 0, size = "xs")
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
            
            # box(
            #   # id = "experimental_upload_box",
            #   title = "Specify Training/Testing set-up",
            #   width = 12,
            #   status = "primary",
            #   headerBorder = F
            # ),
            
            br(),
            Prompt_RM_UI_unsup()
            
          ),
          
          hidden(div(
            id = "param_box",
            
            # box(
            #   # id = "experimental_upload_box",
            #   title = "Parameter Optimization",
            #   width = 12,
            #   status = "primary",
            #   headerBorder = F
            # ),
            
            br(),
            param_RM_UI()
            
          )),
          
          hidden(div(
            id = "RM_box",
            
            # box(
            #   # id = "experimental_upload_box",
            #   title = "Evaluate Model",
            #   width = 12,
            #   status = "primary",
            #   headerBorder = F
            # ),
            
            br(),
            uiOutput("RM_tab_UI")
            
          )),
          
          hidden(div(
            id = "RM_result_box",
            
            # box(
            #   # id = "experimental_upload_box",
            #   title = "Review Results",
            #   width = 12,
            #   status = "primary",
            #   headerBorder = F
            # ),
            
            br(),
            progress_tab("results_review"),
            
            actionButton("complete_results_review", "Done")
            
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