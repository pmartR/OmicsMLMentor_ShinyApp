## Helper_obs

# Observe any collapsible panels
observeEvent(input$collapseTitleClick, {
  req(input$collapseTitleClick)
  updateBoxCollapse(session, input$collapseTitleClick$p, toggle = input$collapseTitleClick$id)
})