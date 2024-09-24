glossary_popup <- function(){
  # Modal
  showModal(
    modalDialog(
      size = "l",
      title = "Glossary",
      
      fluidRow(
      tabsetPanel(
        id = "gloss_tabs",
        
        tabPanel("Current page",
                 uiOutput("current_page_glossary")
                 ),
        
        tabPanel("Model information",
                 wellPanel(
                   
                   strong("Click graph for more information"),
                   
                   hr(),
                   
                   div(
                     style = 'height:500px; overflow-y: scroll',
                     
                     br(),
                     
                     fluidRow(
                       map(models_long_name[!models_long_name %in% c("multilasso", "loglasso")], 
                           function(x) 
                         uiOutput(paste0("EM_", x)))
                     )
                     
                   )
                 )
        ),
        
        tabPanel("Statistical methods",
                 wellPanel(
                   strong("Click graph for more information"),
                   hr(),
                   div(
                     style = 'height:500px; overflow-y: scroll',
                     br(),
                     fluidRow(
                       map(statistical_methods, 
                           function(x) {
                             uiOutput(paste0("SM_", x))
                           })
                             
                     )
                     
                   )
                 )
        )
      ))
    )
  )
}