glossary_popup <- function(){
  # Modal
  showModal(
    modalDialog(
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
                       map(models, function(x) uiOutput(paste0("EM_", x)))
                     )
                     
                   )
                 )
        ),
        
        tabPanel("Statistical methods",
                 wellPanel(
                 )
        )
      )
    )
  )
}