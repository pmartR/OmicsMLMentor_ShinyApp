
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

observeEvent(c(input$ag_prompts, input$ag_prompts_supervised, input$ag_prompts_unsupervised,
               input$skip_ag, input$pick_model_group_pick), {
  if (is.null(input$ag_prompts) && !isTruthy(input$skip_ag)) {
    shinyjs::hide("ag_done")
    return()
  }
  
  if (isTruthy(input$skip_ag) && is.null(input$pick_model_group_pick)) {
    shinyjs::hide("ag_done")
    return()
  }
  
  if (!is.null(input$ag_prompts) && input$ag_prompts == "supervised" && 
      (is.null(input$f_data_response_picker) || is.null(input$ag_prompts_supervised))) {
    shinyjs::hide("ag_done")
    return()
  }
  
  if (!is.null(input$ag_prompts) && input$ag_prompts == "unsupervised" && 
      is.null(input$ag_prompts_unsupervised)) {
    shinyjs::hide("ag_done")
    return()
  }
  
  shinyjs::show("ag_done")
}, ignoreNULL = FALSE)

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
  
  shinyjs::show("model_reccomendations")
  
})

observeEvent(input$msu_review_done, ignoreInit = T, {
  
  if(!is.null(input$msu_review_done) && input$msu_review_done > 0){
    updateNavbarPage(session, "top_page", "Pre-processing")
  }
  
})

