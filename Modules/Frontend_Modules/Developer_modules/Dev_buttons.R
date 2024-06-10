## Dev buttons

output$developer_buttons <- renderUI({
  if(Sys.getenv("SHINY_TEST_MODE") == "1"){
    div(style="position:absolute;z-index:9999;bottom:10px;right:10px;",
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