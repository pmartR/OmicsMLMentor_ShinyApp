
patch_notes <- list(
  "v 0.1" = "Added functionality for introjs and user experience levels",
  "v 0.2" = "Additional features, quality of life support, and AWS deployment support",
  "v 0.3" = "Bugfix and progress UI release for MP prod env -- upcoming changes include reports, more bugfixes, and improved AWS functionality",
  "v 1.0" = "Deployment of SLOPE for MultiProbe enviornment, including many bigfixes, features, and updated README.md. Prediction on new data is"
)

subtext <- c(
  "12Dec2023",
  "27Mar2024",
  "19Aug2024",
  "06Mar2025"
)

observeEvent(input$launch_app_info,{
  
  # Modal
  showModal(
    modalDialog(
      size = "l",
      title = "Current version and patch notes",
      
      br(),
      
      strong("Currently running:"),
      br(),
      paste0("SLOPE app ", names(patch_notes)[length(patch_notes)]),
      br(),
      paste0("Release date: ", subtext[length(subtext)]),
      br(),
      br(),
      pickerInput("patch_picker", 
                  "Select version to view patch notes:",
                  choices = names(patch_notes),
                  selected = names(patch_notes)[length(patch_notes)],
                  choicesOpt = list(
                    subtext = subtext
                  )
      ),
      
      br(),
      
      strong("Patch notes:"),
      br(),
      
      textOutput("patch_notes_text")
      
    )
  )
  
})

output$patch_notes_text <- renderText({
  patch_notes[[input$patch_picker]]
})
