
## upload overlord observers

observeEvent(input$show_VS, ignoreNULL = F, {
  
  shinyjs::show(id = "VS_box")
  shinyjs::hide(id = "AG_box")
  shinyjs::hide(id = "EM_box")
  shinyjs::hide(id = "MSU_review_selection_box")
  
  shinyjs::addClass("show_VS", "blueoutline")
  shinyjs::removeClass("show_AGoals", "blueoutline")
  shinyjs::removeClass("show_EM", "blueoutline")
  shinyjs::removeClass("review_MSU", "blueoutline")
  
})

observeEvent(input$show_AGoals, {
  
  shinyjs::hide(id = "VS_box")
  shinyjs::show(id = "AG_box")
  shinyjs::hide(id = "EM_box")
  shinyjs::hide(id = "MSU_review_selection_box")
  
  shinyjs::removeClass("show_VS", "blueoutline")
  shinyjs::addClass("show_AGoals", "blueoutline")
  shinyjs::removeClass("show_EM", "blueoutline")
  shinyjs::removeClass("review_MSU", "blueoutline")
  
})

observeEvent(input$show_EM, {
  
  shinyjs::hide(id = "VS_box")
  shinyjs::hide(id = "AG_box")
  shinyjs::show(id = "EM_box")
  shinyjs::hide(id = "MSU_review_selection_box")
  
  shinyjs::removeClass("show_VS", "blueoutline")
  shinyjs::removeClass("show_AGoals", "blueoutline")
  shinyjs::addClass("show_EM", "blueoutline")
  shinyjs::removeClass("review_MSU", "blueoutline")
  
})

observeEvent(input$review_MSU, ignoreInit = T, {
  
  shinyjs::hide(id = "VS_box")
  shinyjs::hide(id = "AG_box")
  shinyjs::hide(id = "EM_box")
  shinyjs::show(id = "MSU_review_selection_box")
  
  shinyjs::removeClass("show_VS", "blueoutline")
  shinyjs::removeClass("show_AGoals", "blueoutline")
  shinyjs::removeClass("show_EM", "blueoutline")
  shinyjs::addClass("review_MSU", "blueoutline")
  
})

observeEvent(input$done_VS, ignoreInit = T, {

  if(!is.null(input$done_VS) && input$done_VS > 0){
    enable("show_AGoals")
    disable("show_EM")
    disable("review_MSU")
  }
  
  print("donevs")
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 shinyjs::hide(id = "VS_box")
                 shinyjs::show(id = "AG_box")
                 shinyjs::hide(id = "EM_box")
                 shinyjs::hide(id = "MSU_review_selection_box")
                 
                 
                 shinyjs::removeClass("show_VS", "blueoutline")
                 shinyjs::addClass("show_AGoals", "blueoutline")
                 shinyjs::removeClass("show_EM", "blueoutline")
                 shinyjs::removeClass("review_MSU", "blueoutline")
               }
             })
  
  updateProgressBar(session, "VS_done", value = 100)
  updateProgressBar(session, "AGoals_done", value = 0)
  updateProgressBar(session, "EM_done", value = 0)

})

observeEvent(input$ag_done, ignoreInit = T, {
  
  if(!is.null(input$ag_done) && input$ag_done > 0){
    enable("show_EM")
    disable("review_MSU")
  }
  
  print("doneag")
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 shinyjs::hide(id = "VS_box")
                 shinyjs::hide(id = "AG_box")
                 shinyjs::show(id = "EM_box")
                 shinyjs::hide(id = "MSU_review_selection_box")
                 
                 shinyjs::removeClass("show_VS", "blueoutline")
                 shinyjs::removeClass("show_AGoals", "blueoutline")
                 shinyjs::addClass("show_EM", "blueoutline")
                 shinyjs::removeClass("review_MSU", "blueoutline")
               }
             })
  
  updateProgressBar(session, "AGoals_done", value = 100)
  updateProgressBar(session, "EM_done", value = 0)
  
})

observeEvent(input$em_select, ignoreInit = T, {
  
  if(!is.null(input$em_select) && input$em_select > 0){
    enable("review_MSU")
  }
  
  print("doneem_select")
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 shinyjs::hide(id = "VS_box")
                 shinyjs::hide(id = "AG_box")
                 shinyjs::hide(id = "EM_box")
                 shinyjs::show(id = "MSU_review_selection_box")
                 
                 shinyjs::removeClass("show_VS", "blueoutline")
                 shinyjs::removeClass("show_AGoals", "blueoutline")
                 shinyjs::removeClass("show_EM", "blueoutline")
                 shinyjs::addClass("review_MSU", "blueoutline")
               }
             })

  updateProgressBar(session, "EM_done", value = 100)
  
  show("model_reccomendations")
  
})

observeEvent(input$msu_review_done, ignoreInit = T, {
  
  if(!is.null(input$msu_review_done) && input$msu_review_done > 0){
    updateNavbarPage(session, "top_page", "Pre-processing")
  }
  
})

