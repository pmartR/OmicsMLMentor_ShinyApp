
## Confirm triggers the make data
observeEvent(omicsData$obj_pp, once = T, {
  
  enable("confirm_preprocess")
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 updateNavbarPage(session = session, "top_page", selected = "Run Model")
               }
             })
  
})


observeEvent(input$confirm_preprocess, {
  
  req(input$confirm_preprocess > 0)
  
  updateNavbarPage(session = session, "top_page", selected = "Run Model")
})
