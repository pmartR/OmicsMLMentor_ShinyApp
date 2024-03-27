
## upload overlord observers

observeEvent(input$show_exp_upload, ignoreNULL = F, {
  
  shinyjs::show(id = "experimental_upload_box")
  shinyjs::hide(id = "sample_upload_box")
  shinyjs::hide(id = "review_upload_box")
  
  shinyjs::addClass("show_exp_upload", "blueoutline")
  shinyjs::removeClass("show_sample_upload", "blueoutline")
  shinyjs::removeClass("review_upload", "blueoutline")
  
})

observeEvent(input$show_sample_upload, ignoreInit = T, {
  
  shinyjs::hide(id = "experimental_upload_box")
  shinyjs::show(id = "sample_upload_box")
  shinyjs::hide(id = "review_upload_box")
  
  shinyjs::removeClass("show_exp_upload", "blueoutline")
  shinyjs::addClass("show_sample_upload", "blueoutline")
  shinyjs::removeClass("review_upload", "blueoutline")
  
})

observeEvent(input$review_upload, ignoreInit = T, {
  
  shinyjs::hide(id = "experimental_upload_box")
  shinyjs::hide(id = "sample_upload_box")
  shinyjs::show(id = "review_upload_box")
  
  shinyjs::removeClass("show_exp_upload", "blueoutline")
  shinyjs::removeClass("show_sample_upload", "blueoutline")
  shinyjs::addClass("review_upload", "blueoutline")
  
})

observeEvent(input$upload_done, ignoreInit = T, {
  
  if(!is.null(input$upload_done) && input$upload_done > 0){
    enable("show_sample_upload")
    disable("review_upload")
  }
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 shinyjs::hide(id = "experimental_upload_box")
                 shinyjs::show(id = "sample_upload_box")
                 shinyjs::hide(id = "review_upload_box")
                 
                 shinyjs::removeClass("show_exp_upload", "blueoutline")
                 shinyjs::addClass("show_sample_upload", "blueoutline")
                 shinyjs::removeClass("review_upload", "blueoutline")
               }
             })
  
  updateProgressBar(session, "upload_exp_done", value = 100)
  updateProgressBar(session, "upload_samp_done", value = 0)
  
})

observeEvent(input$check_group_cols, ignoreInit = T, {
  
  if(!is.null(input$check_group_cols) && input$check_group_cols > 0){
    enable("review_upload")
  }
  
  
  tryCatch({
    if(!inherits(omicsData$obj, "seqData"))
      cv_filter(omicsData$obj)
    
    shinyalert(title = "Success!", "Continue to next page or review results?",
               showCancelButton = T, closeOnEsc = F,
               confirmButtonText = "Continue",
               cancelButtonText = "Review results",
               callbackR = function(value){
                 if(value){
                   shinyjs::hide(id = "experimental_upload_box")
                   shinyjs::hide(id = "sample_upload_box")
                   shinyjs::show(id = "review_upload_box")

                   shinyjs::removeClass("show_exp_upload", "blueoutline")
                   shinyjs::removeClass("show_sample_upload", "blueoutline")
                   shinyjs::addClass("review_upload", "blueoutline")
                 }
               })
    
    updateProgressBar(session, "upload_exp_done", value = 100)
    updateProgressBar(session, "upload_samp_done", value = 100)
    
  }, error = function(e){
    
    shinyalert(title = "Something went wrong processing your data.",
               "Un-doing log transformation appears to generate very large numbers. Should these be raw abundance values?",
               showCancelButton = T, closeOnEsc = F,
               confirmButtonText = "Yes",
               cancelButtonText = "No",
               callbackR = function(value){
                 if(value){
                   attr(omicsData$obj, "data_info")$data_scale_orig <- "abundance"
                   updatePickerInput(session, "datascale", selected = "abundance")
                 } else {
                   shinyalert(title = "Please double check uploaded data for any anomalies.")
                 }
               })
    
    
    
    
  })
  
})

observeEvent(input$review_upload_done, ignoreInit = T, {
  
  if(!is.null(input$review_upload_done) && input$review_upload_done > 0){
    updateNavbarPage(session, "top_page", "Quality Control")
  }

})

