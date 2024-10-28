## Dev buttons

output$developer_buttons <- renderUI({
  if(isTRUE(getOption("shiny.testmode"))){
  div(style="position:fixed;z-index:9999;bottom:10px;right:10px;",
      actionButton("Browser", "whats wrong!?!?", style = "background:deepskyblue"),
      actionButton("Reload", "reload resources", style = "background:deepskyblue")
  )
  }
  else return(NULL)
})

# Button to do browser things for debug
observeEvent(input$Browser, {
  browser()
})