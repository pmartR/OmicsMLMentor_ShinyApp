
destroy_advanced <- function(){
  
}

destroy_unguided <- function(){
  
}

observeEvent(input$welcome_confirm, {
  
  if(input$welcome_confirm == "beginner"){
    destroy_advanced()
  } else if (input$welcome_confirm == "familiar") {
    destroy_unguided()
  } else {
    NULL
  }
  
  updateNavbarPage(session, "top_page", "Upload")
  
})

observeEvent(input$welcome_confirm, {
  
  if(input$welcome_confirm == "beginner"){
    destroy_advanced()
  } else if (input$welcome_confirm == "familiar") {
    destroy_unguided()
  } else {
    NULL
  }
  
  updateNavbarPage(session, "top_page", "Upload")
  
})

## Button tutorial call
observeEvent(input$launch_totoro, introjs(session,options = list(steps=steps())))

observeEvent(input$launch_data_requirements, {
  
  # Modal
  showModal(
    modalDialog(
      size = "l",
      title = "Data requirements",
      tabsetPanel(
        tabPanel(
          "Supported Data Types",
          fluidRow(
            column(10,
                   align = "center", offset = 1,
                   requirements_function()
            )
          )
        ),
        tabPanel(
          "Data formating and templates",
          fluidRow(
            column(10,
                   align = "center", offset = 1,
                   
                   example_data_UI()
            )
          )
        )
        )
    )
  )
  })

patch_notes <- list(
  "v 1.0" = "Added functionality for introjs and user experience levels",
  "test" = "test yay"
)

subtext <- c(
  "12Dec2023",
  "test"
)


observeEvent(input$launch_app_info,{
  
  # Modal
  showModal(
    modalDialog(
      size = "l",
      title = "Current version and patch notes",
      
      br(),
      
      "SLOPE version: 1.0",
      br(),
      "Last updated: 12Dec2023",
      br(),
      br(),
      pickerInput("patch_picker", 
                  "Select version:",
                  choices = names(patch_notes),
                  choicesOpt = list(
                    subtext = subtext
                    )
                  ),
      
      br(),
      
      textOutput("patch_notes_text")
      
    )
  )
  
})

output$patch_notes_text <- renderText({
  patch_notes[[input$patch_picker]]
})


observeEvent(input$launch_glossary_models, {
  glossary_popup()
})

