
## upload overlord observers


observeEvent(omicsData$objPP, once = T, ignoreNULL = T, {
  output$preprocessing_tab_overlord_ui <- renderUI({
    
    if(inherits(isolate(omicsData$objPP), "pepData")){
      
      div(
        fluidRow(
          align = "center",
          
          column(1, ""), ## spacer
          
          column(1, 
                 actionBttn(
                   inputId = "show_transform",
                   label = HTML("Scaling and<br/>Transformation"),
                   style = "jelly", 
                   color = "default",
                   size = "s"
                 )
          ),
          
          column(
            1,
            br(),
            progressBar(id = "transform_done", value = 0, size = "xs")
          ),
          
          column(1, 
                 disabled(actionBttn(
                   inputId = "show_filters",
                   label = HTML("Data<br/>Filtering"),
                   style = "jelly", 
                   color = "default",
                   size = "s"
                 ))
          ),
          
          column(
            1,
            br(),
            progressBar(id = "filters_done", value = 0, size = "xs")
          ),
          
          
          column(1, 
                 disabled(actionBttn(
                   inputId = "show_normalization",
                   label = HTML("Data<br/>Normalization"),
                   style = "jelly", 
                   color = "default",
                   size = "s"
                 ))
          ),
          
          column(
            1,
            br(),
            progressBar(id = "norm_done", value = 0, size = "xs")
          ),
          
          column(1, 
                 disabled(actionBttn(
                   inputId = "show_protein_roll",
                   label = HTML("Protein<br/>Roll Up"),
                   style = "jelly", 
                   color = "default",
                   size = "s"
                 ))
          ),
          
          column(
            1,
            br(),
            progressBar(id = "rollup_done", value = 0, size = "xs")
          ),
          
          column(1, 
                 disabled(actionBttn(
                   inputId = "review_PP",
                   label = HTML("Review<br/>Selections"),
                   style = "jelly", 
                   color = "default",
                   size = "s"
                 ))
          ),
          
          column(2, "") ## spacer
          
        ),
        
        fluidRow(
          
          column(
            12,
            
            br(),
            
            div(
              id = "transform_box",
              
              # box(
              #   # id = "experimental_upload_box",
              #   title = "Scale and/or Transform Data",
              #   width = 12,
              #   status = "primary",
              #   headerBorder = F
              # ),
              
              br(),
              transformation_UI()
              
            ),
            
            div(
              id = "filter_box",
              
              # box(
              #   # id = "experimental_upload_box",
              #   title = "Filter Data",
              #   width = 12,
              #   status = "primary",
              #   headerBorder = F
              # ),
              
              br(),
              uiOutput("filter_page")
              
            ),
            
            div(
              id = "norm_box",
              
              # box(
              #   # id = "experimental_upload_box",
              #   title = "Normalize Data",
              #   width = 12,
              #   status = "primary",
              #   headerBorder = F
              # ),
              
              br(),
              uiOutput("norm_tab")
              # norm_tab()
              
            ),
            
            div(
              id = "rollup_box",
              
              # box(
              #   # id = "experimental_upload_box",
              #   title = "Roll Up to Protein-level Data",
              #   width = 12,
              #   status = "primary",
              #   headerBorder = F
              # ),
              
              br(),
              uiOutput("rollup_tab")
              
            ),
            
            div(
              id = "PP_review_selection_box",
              
              # box(
              #   # id = "experimental_upload_box",
              #   title = "Review Selections",
              #   width = 12,
              #   status = "primary",
              #   headerBorder = F
              # ),
              
              br(),
              progress_tab("preprocessing"),
              
              actionButton("complete_ppreview", "Done")
              
            )
            
          )
        )
      )
    } else {
      
      div(
        fluidRow(
          align = "center",
          
          column(1, ""), ## spacer
          
          column(1, 
                 actionBttn(
                   inputId = "show_transform",
                   label = HTML("Scaling and<br/>Transformation"),
                   style = "jelly", 
                   color = "default",
                   size = "s"
                 )
          ),
          
          column(
            2,
            br(),
            progressBar(id = "transform_done", value = 0, size = "xs")
          ),
          
          column(1, 
                 disabled(actionBttn(
                   inputId = "show_filters",
                   label = HTML("Data<br/>Filtering"),
                   style = "jelly", 
                   color = "default",
                   size = "s"
                 ))
          ),
          
          column(
            2,
            br(),
            progressBar(id = "filters_done", value = 0, size = "xs")
          ),
          
          
          column(1, 
                 disabled(actionBttn(
                   inputId = "show_normalization",
                   label = HTML("Data<br/>Normalization"),
                   style = "jelly", 
                   color = "default",
                   size = "s"
                 ))
          ),
          
          column(
            2,
            br(),
            progressBar(id = "norm_done", value = 0, size = "xs")
          ),
          
          column(1, 
                 disabled(actionBttn(
                   inputId = "review_PP",
                   label = HTML("Review<br/>Selections"),
                   style = "jelly", 
                   color = "default",
                   size = "s"
                 ))
          ),
          
          column(1, "") ## spacer
          
        ),
        
        fluidRow(
          
          column(
            12,
            
            br(),
            
            div(
              id = "transform_box",
              
              # box(
              #   # id = "experimental_upload_box",
              #   title = "Scale and/or Transform Data",
              #   width = 12,
              #   status = "primary",
              #   headerBorder = F
              # ),
              
              br(),
              transformation_UI()
              
            ),
            
            div(
              id = "filter_box",
              
              # box(
              #   # id = "experimental_upload_box",
              #   title = "Filter Data",
              #   width = 12,
              #   status = "primary",
              #   headerBorder = F
              # ),
              
              br(),
              uiOutput("filter_page")
              
            ),
            
            div(
              id = "norm_box",
              
              # box(
              #   # id = "experimental_upload_box",
              #   title = "Normalize Data",
              #   width = 12,
              #   status = "primary",
              #   headerBorder = F
              # ),
              
              br(),
              uiOutput("norm_tab")
              # norm_tab()
              
            ),
            
            div(
              id = "PP_review_selection_box",
              
              # box(
              #   # id = "experimental_upload_box",
              #   title = "Review Selections",
              #   width = 12,
              #   status = "primary",
              #   headerBorder = F
              # ),
              
              br(),
              progress_tab("preprocessing"),
              
              actionButton("complete_ppreview", "Done")
              
            )
            
          )
        )
      )
    }
    
  })
})

observeEvent(input$show_transform, ignoreNULL = F, {
  
  shinyjs::show(id = "transform_box")
  shinyjs::hide(id = "filter_box")
  shinyjs::hide(id = "norm_box")
  shinyjs::hide(id = "rollup_box")
  shinyjs::hide(id = "PP_review_selection_box")
  
  shinyjs::addClass("show_transform", "blueoutline")
  shinyjs::removeClass("show_filters", "blueoutline")
  shinyjs::removeClass("show_normalization", "blueoutline")
  shinyjs::removeClass("show_protein_roll", "blueoutline")
  shinyjs::removeClass("review_PP", "blueoutline")
  
})

observeEvent(input$show_filters, {
  
  shinyjs::hide(id = "transform_box")
  shinyjs::show(id = "filter_box")
  shinyjs::hide(id = "norm_box")
  shinyjs::hide(id = "rollup_box")
  shinyjs::hide(id = "PP_review_selection_box")
  
  shinyjs::removeClass("show_transform", "blueoutline")
  shinyjs::addClass("show_filters", "blueoutline")
  shinyjs::removeClass("show_normalization", "blueoutline")
  shinyjs::removeClass("show_protein_roll", "blueoutline")
  shinyjs::removeClass("review_PP", "blueoutline")
  
})

observeEvent(input$show_normalization, {
  
  shinyjs::hide(id = "transform_box")
  shinyjs::hide(id = "filter_box")
  shinyjs::show(id = "norm_box")
  shinyjs::hide(id = "rollup_box")
  shinyjs::hide(id = "PP_review_selection_box")
  
  shinyjs::removeClass("show_transform", "blueoutline")
  shinyjs::removeClass("show_filters", "blueoutline")
  shinyjs::addClass("show_normalization", "blueoutline")
  shinyjs::removeClass("show_protein_roll", "blueoutline")
  shinyjs::removeClass("review_PP", "blueoutline")
  
})

observeEvent(input$show_protein_roll, {
  
  shinyjs::hide(id = "transform_box")
  shinyjs::hide(id = "filter_box")
  shinyjs::hide(id = "norm_box")
  shinyjs::show(id = "rollup_box")
  shinyjs::hide(id = "PP_review_selection_box")
  
  shinyjs::removeClass("show_transform", "blueoutline")
  shinyjs::removeClass("show_filters", "blueoutline")
  shinyjs::removeClass("show_normalization", "blueoutline")
  shinyjs::addClass("show_protein_roll", "blueoutline")
  shinyjs::removeClass("review_PP", "blueoutline")
  
})

observeEvent(input$review_PP, {
  
  shinyjs::hide(id = "transform_box")
  shinyjs::hide(id = "filter_box")
  shinyjs::hide(id = "norm_box")
  shinyjs::hide(id = "rollup_box")
  shinyjs::show(id = "PP_review_selection_box")
  
  
  shinyjs::removeClass("show_transform", "blueoutline")
  shinyjs::removeClass("show_filters", "blueoutline")
  shinyjs::removeClass("show_normalization", "blueoutline")
  shinyjs::removeClass("show_protein_roll", "blueoutline")
  shinyjs::addClass("review_PP", "blueoutline")
  
})

observeEvent(input$complete_transform, ignoreInit = T, {
  
  print("transform")
  
  if(!is.null(input$complete_transform) && input$complete_transform > 0){
    enable("show_filters")
    disable("show_normalization")
    disable("show_protein_roll")
    disable("review_PP")
  }
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 shinyjs::hide(id = "transform_box")
                 shinyjs::show(id = "filter_box")
                 shinyjs::hide(id = "norm_box")
                 shinyjs::hide(id = "rollup_box")
                 shinyjs::hide(id = "PP_review_selection_box")
                 
                 shinyjs::removeClass("show_transform", "blueoutline")
                 shinyjs::addClass("show_filters", "blueoutline")
                 shinyjs::removeClass("show_normalization", "blueoutline")
                 shinyjs::removeClass("show_protein_roll", "blueoutline")
                 shinyjs::removeClass("review_PP", "blueoutline")
               }
             })
  
  updateProgressBar(session, "transform_done", value = 100)
  updateProgressBar(session, "filters_done", value = 0)
  updateProgressBar(session, "norm_done", value = 0)
  updateProgressBar(session, "rollup_done", value = 0)
  
})

observeEvent(input$complete_filters, ignoreInit = T, {
  
  print("filters")
  
  if(!is.null(omicsData$objfilters)){
    omicsData$objPP <- omicsData$objfilters
  }
  disable("apply_filters")
  
  if(!is.null(input$complete_filters) && input$complete_filters > 0){
    enable("show_normalization")
    disable("show_protein_roll")
    disable("review_PP")
  }
  
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 shinyjs::hide(id = "transform_box")
                 shinyjs::hide(id = "filter_box")
                 shinyjs::show(id = "norm_box")
                 shinyjs::hide(id = "rollup_box")
                 shinyjs::hide(id = "PP_review_selection_box")
                 
                 shinyjs::removeClass("show_transform", "blueoutline")
                 shinyjs::removeClass("show_filters", "blueoutline")
                 shinyjs::addClass("show_normalization", "blueoutline")
                 shinyjs::removeClass("show_protein_roll", "blueoutline")
                 shinyjs::removeClass("review_PP", "blueoutline")
               }
             })
  
  updateProgressBar(session, "filters_done", value = 100)
  updateProgressBar(session, "norm_done", value = 0)
  updateProgressBar(session, "rollup_done", value = 0)
  
})

observeEvent(input$complete_norm, ignoreInit = T, {
  
  print("norm")
  
  if(!is.null(input$complete_norm) && input$complete_norm > 0){
    enable("show_protein_roll")
    disable("review_PP")
  }
  
  tabname <- isolate(str_to_title(class(omicsData$objPP)[[1]]))
  
  if(!is.null(omicsData$objNorm) && 
     input[[paste0(tabname, "_normalize_option")]] != "No Normalization"){
    omicsData$objPP <- omicsData$objNorm
  }
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 
                 if(inherits(omicsData$objPP, "pepData")){
                   shinyjs::hide(id = "transform_box")
                   shinyjs::hide(id = "filter_box")
                   shinyjs::hide(id = "norm_box")
                   shinyjs::show(id = "rollup_box")
                   shinyjs::hide(id = "PP_review_selection_box")
                   
                   shinyjs::removeClass("show_transform", "blueoutline")
                   shinyjs::removeClass("show_filters", "blueoutline")
                   shinyjs::removeClass("show_normalization", "blueoutline")
                   shinyjs::addClass("show_protein_roll", "blueoutline")
                   shinyjs::removeClass("review_PP", "blueoutline")
                   
                 } else {
                   shinyjs::hide(id = "transform_box")
                   shinyjs::hide(id = "filter_box")
                   shinyjs::hide(id = "norm_box")
                   shinyjs::hide(id = "rollup_box")
                   shinyjs::show(id = "PP_review_selection_box")
                   
                   shinyjs::removeClass("show_transform", "blueoutline")
                   shinyjs::removeClass("show_filters", "blueoutline")
                   shinyjs::removeClass("show_normalization", "blueoutline")
                   shinyjs::removeClass("show_protein_roll", "blueoutline")
                   shinyjs::addClass("review_PP", "blueoutline")
                   
                 }
               }
             })
  
  updateProgressBar(session, "norm_done", value = 100)
  updateProgressBar(session, "rollup_done", value = 0)
  
})

observeEvent(input$complete_rollup, ignoreInit = T, {
  
  print("rollup")
  
  if(!is.null(input$complete_rollup) && input$complete_rollup > 0){
    enable("review_PP")
  }
  
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 shinyjs::hide(id = "transform_box")
                 shinyjs::hide(id = "filter_box")
                 shinyjs::hide(id = "norm_box")
                 shinyjs::hide(id = "rollup_box")
                 shinyjs::show(id = "PP_review_selection_box")
                 
                 shinyjs::removeClass("show_transform", "blueoutline")
                 shinyjs::removeClass("show_filters", "blueoutline")
                 shinyjs::removeClass("show_normalization", "blueoutline")
                 shinyjs::removeClass("show_protein_roll", "blueoutline")
                 shinyjs::addClass("review_PP", "blueoutline")
               }
             })
  
  updateProgressBar(session, "rollup_done", value = 100)
  
})

observeEvent(input$complete_ppreview, ignoreInit = T, {
  
  print("preview")
  
  if(!is.null(input$complete_ppreview) && input$complete_ppreview > 0){
    updateNavbarPage(session, "top_page", "Run Model")
  }
  
})
