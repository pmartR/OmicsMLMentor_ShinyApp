
observeEvent(input$welcome_confirm, {
  
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
                   align = "left", 
                   offset = 1,
                   br(),
                   requirements_function()
            )
          )
        ),
        tabPanel(
          "Data formating and templates",
          fluidRow(
            column(10,
                   align = "left", 
                   offset = 1,
                   br(),
                   example_data_UI()
            )
          )
        )
        )
    )
  )
  })



observeEvent(input$launch_glossary_models, {
  glossary_popup()
})

