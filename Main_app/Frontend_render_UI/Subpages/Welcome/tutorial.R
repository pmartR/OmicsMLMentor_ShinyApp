
steps <- reactive(data.frame(
  
  step = 1:9,
  intro = c(
    HTML(paste0("Welcome to Omics ML Mentor v 1.0! This application allows users",
    " to upload 'omics data and develop statistical machine learning models ",
    "with expert guidance.<br/><br/> This tutorial will take you through ",
    "how to find additional information about the ",
    "data types, models, and statistical methods currently supported.")),
    
    HTML(paste0("Omics ML Mentor requires quantitative ‘omics data to build a model. ",
           "Proteomic, metabolomic, lipidomic, and transcriptomic data types are supported.",
           " To view data requirement specifics and/or download templates, click this button.")),
    
    HTML(paste0("Currently, Omics ML Mentor supports a variety of models and statistical",
           " analyses to manage your data with respect to the supported ",
           "models. Use this button to see all information ",
           "associated with the supported models and methods."
           )),
    
    HTML(paste0("No need to navigate back to this page as a reference either - this 'Glossary' ",
           "button will be available to users throughout the application.")),
    
    HTML("This work is still in development - major updates and changes will be posted here."),
    
    HTML(paste0("For additional assistance, this help button will provide ",
                "guidance and additional information for users ",
           "on each page.")),
    
    HTML(paste0("Pages are interspersed into these sections. You can use this",
                " menu to navigate back and forth, but the application will ",
                "automatically prompt the next page as you complete each step.")),
    
    HTML(paste0("Lastly, feel free to reach out to the maintainers with ",
                "requested features or potential bugs. We appreciate your ",
                "patience as we continue to develop!")),
    
    HTML("Need a second look? Read this tutorial again by clicking here.")
  ),
  element = c(
    NA,
    "#launch_data_requirements",
    "#launch_glossary_models",
    "#glossary_button",
    "#launch_app_info",
    "#help_button",
    "#top_page",
    "#contact",
    "#launch_totoro"
  ), 
  position = rep("auto", 9)
))

## Pop-up tutorial call
launch_tutorial <- function(){
  
  shinyalert("Welcome!",
             paste0(
               "Omics ML Mentor functions as a tool to assist analysts in developing",
               " high quality machine learning models.",
               " Would you like to view the ",
               "introduction tutorial?"),
             
             closeOnEsc = FALSE,
             showCancelButton = TRUE,
             confirmButtonText = "Yes",
             cancelButtonText = "No",
             callbackR = function(value){
               if(value){
                 print("launch intro sequence")
                 introjs(session,options = list(steps=steps()))
               }
               
             })
  
  
}
