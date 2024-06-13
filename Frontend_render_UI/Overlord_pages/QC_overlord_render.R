
## upload overlord observers

observeEvent(input$show_refnorm, ignoreNULL = F, {
  
  shinyjs::show(id = "refnorm_box")
  shinyjs::hide(id = "low_ob_box")
  shinyjs::hide(id = "remove_outlier_box")
  shinyjs::hide(id = "missing_data_box")
  shinyjs::hide(id = "QC_review_selection_box")
  
  shinyjs::addClass("show_refnorm", "blueoutline")
  shinyjs::removeClass("show_low_obs", "blueoutline")
  shinyjs::removeClass("show_outlier_detect", "blueoutline")
  shinyjs::removeClass("show_missing_data", "blueoutline")
  shinyjs::removeClass("review_QC", "blueoutline")
  
})

observeEvent(input$show_low_obs, {
  
  shinyjs::hide(id = "refnorm_box")
  shinyjs::show(id = "low_ob_box")
  shinyjs::hide(id = "remove_outlier_box")
  shinyjs::hide(id = "missing_data_box")
  shinyjs::hide(id = "QC_review_selection_box")
  
  shinyjs::removeClass("show_refnorm", "blueoutline")
  shinyjs::addClass("show_low_obs", "blueoutline")
  shinyjs::removeClass("show_outlier_detect", "blueoutline")
  shinyjs::removeClass("show_missing_data", "blueoutline")
  shinyjs::removeClass("review_QC", "blueoutline")
  
})

observeEvent(input$show_outlier_detect, {
  
  shinyjs::hide(id = "refnorm_box")
  shinyjs::hide(id = "low_ob_box")
  shinyjs::show(id = "remove_outlier_box")
  shinyjs::hide(id = "missing_data_box")
  shinyjs::hide(id = "QC_review_selection_box")
  
  shinyjs::removeClass("show_refnorm", "blueoutline")
  shinyjs::removeClass("show_low_obs", "blueoutline")
  shinyjs::addClass("show_outlier_detect", "blueoutline")
  shinyjs::removeClass("show_missing_data", "blueoutline")
  shinyjs::removeClass("review_QC", "blueoutline")
  
})

observeEvent(input$show_missing_data, {
  
  shinyjs::hide(id = "refnorm_box")
  shinyjs::hide(id = "low_ob_box")
  shinyjs::hide(id = "remove_outlier_box")
  shinyjs::show(id = "missing_data_box")
  shinyjs::hide(id = "QC_review_selection_box")
  
  shinyjs::removeClass("show_refnorm", "blueoutline")
  shinyjs::removeClass("show_low_obs", "blueoutline")
  shinyjs::removeClass("show_outlier_detect", "blueoutline")
  shinyjs::addClass("show_missing_data", "blueoutline")
  shinyjs::removeClass("review_QC", "blueoutline")
  
})

observeEvent(input$review_QC, {
  
  shinyjs::hide(id = "refnorm_box")
  shinyjs::hide(id = "low_ob_box")
  shinyjs::hide(id = "remove_outlier_box")
  shinyjs::hide(id = "missing_data_box")
  shinyjs::show(id = "QC_review_selection_box")
  
  shinyjs::removeClass("show_refnorm", "blueoutline")
  shinyjs::removeClass("show_low_obs", "blueoutline")
  shinyjs::removeClass("show_outlier_detect", "blueoutline")
  shinyjs::removeClass("show_missing_data", "blueoutline")
  shinyjs::addClass("review_QC", "blueoutline")
  
})


###

observeEvent(input$refnorm_complete, ignoreInit = T, {
  
  if(!is.null(input$refnorm_complete) && input$refnorm_complete > 0){
    enable("show_low_obs")
    disable("show_outlier_detect")
    disable("show_missing_data")
    disable("review_QC")
  }
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 shinyjs::hide(id = "refnorm_box")
                 shinyjs::show(id = "low_ob_box")
                 shinyjs::hide(id = "remove_outlier_box")
                 shinyjs::hide(id = "missing_data_box")
                 shinyjs::hide(id = "QC_review_selection_box")
                 
                 shinyjs::removeClass("show_refnorm", "blueoutline")
                 shinyjs::addClass("show_low_obs", "blueoutline")
                 shinyjs::removeClass("show_outlier_detect", "blueoutline")
                 shinyjs::removeClass("show_missing_data", "blueoutline")
                 shinyjs::removeClass("review_QC", "blueoutline")
               }
             })
  
  updateProgressBar(session, "QC_refnorm_done", value = 100)
  updateProgressBar(session, "QC_lo_done", value = 0)
  updateProgressBar(session, "QC_outlier_done", value = 0)
  updateProgressBar(session, "QC_missing_data_done", value = 0)
  
})

observeEvent(input$LQ_done, ignoreInit = T, {

  if(!is.null(input$LQ_done) && input$LQ_done > 0){
    enable("show_outlier_detect")
    disable("show_missing_data")
    disable("review_QC")
  }
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 shinyjs::hide(id = "refnorm_box")
                 shinyjs::hide(id = "low_ob_box")
                 shinyjs::show(id = "remove_outlier_box")
                 shinyjs::hide(id = "missing_data_box")
                 shinyjs::hide(id = "QC_review_selection_box")
                 
                 shinyjs::removeClass("show_low_obs", "blueoutline")
                 shinyjs::addClass("show_outlier_detect", "blueoutline")
                 shinyjs::removeClass("show_missing_data", "blueoutline")
                 shinyjs::removeClass("review_QC", "blueoutline")
               }
             })

  updateProgressBar(session, "QC_lo_done", value = 100)
  updateProgressBar(session, "QC_outlier_done", value = 0)
  updateProgressBar(session, "QC_missing_data_done", value = 0)

})

observeEvent(input$outliers_done, ignoreInit = T, {
  
  if(!is.null(input$outliers_done) && input$outliers_done > 0){
    enable("show_missing_data")
    disable("review_QC")
  }
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 shinyjs::hide(id = "refnorm_box")
                 shinyjs::hide(id = "low_ob_box")
                 shinyjs::hide(id = "remove_outlier_box")
                 shinyjs::show(id = "missing_data_box")
                 shinyjs::hide(id = "QC_review_selection_box")
                 
                 shinyjs::removeClass("show_low_obs", "blueoutline")
                 shinyjs::removeClass("show_outlier_detect", "blueoutline")
                 shinyjs::addClass("show_missing_data", "blueoutline")
                 shinyjs::removeClass("review_QC", "blueoutline")
               }
             })
  
  updateProgressBar(session, "QC_outlier_done", value = 100)
  updateProgressBar(session, "QC_missing_data_done", value = 0)
  
})


observeEvent(input$done_md, ignoreInit = T, {
  
  if(!is.null(input$done_md) && input$done_md > 0){
    enable("review_QC")
  }
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 shinyjs::hide(id = "refnorm_box")
                 shinyjs::hide(id = "low_ob_box")
                 shinyjs::hide(id = "remove_outlier_box")
                 shinyjs::hide(id = "missing_data_box")
                 shinyjs::show(id = "QC_review_selection_box")
                 
                 shinyjs::removeClass("show_low_obs", "blueoutline")
                 shinyjs::removeClass("show_outlier_detect", "blueoutline")
                 shinyjs::removeClass("show_missing_data", "blueoutline")
                 shinyjs::addClass("review_QC", "blueoutline")
               }
             })
  
  updateProgressBar(session, "QC_missing_data_done", value = 100)
  
})

observeEvent(input$qc_review_done, ignoreInit = T, {
  
  if(!is.null(input$qc_review_done) && input$qc_review_done > 0){
    updateNavbarPage(session, "top_page", "Model Set-Up")
  }
  
})

