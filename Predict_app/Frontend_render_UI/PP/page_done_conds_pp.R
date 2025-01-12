
## Confirm triggers the make data
observeEvent(omicsData$obj_pp, once = T, {
  
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
