


observeEvent(input$glossary_button, {
  glossary_popup()
})

observeEvent(input$contact, {
  # Modal
  showModal(
    modalDialog(
      size = "m",
      title = "Contact Maintainer",
      
      br(),
      
      paste0("Please email us at Lisa.Bramer@pnnl.gov with any questions or ",
             "concerns regarding the Omics ML Mentor app. This includes, but is not ",
             "limited to, bugs, feature requests, and feedback.")
      
    )
  )
})