
## Confirm triggers the make data
observeEvent(omicsData$obj_predictions, once = T, {
  
  enable("complete_RM")
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 updateNavbarPage(session = session, "top_page", selected = "Downloads")
               }
             })
  
})

observeEvent(input$complete_RM, {
  updateNavbarPage(session = session, "top_page", selected = "Downloads")
})
