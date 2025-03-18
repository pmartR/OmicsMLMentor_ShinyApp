## Dev buttons

# output$developer_buttons <- renderUI({
#   div(style="position:fixed;z-index:9999;bottom:10px;right:10px;",
#       actionButton("Browser", "whats wrong!?!?", style = "background:deepskyblue"),
#       actionButton("Reload", "reload resources", style = "background:deepskyblue")
#   )
# })


# Button to do browser things for debug
# observeEvent(input$Browser, {
#   browser()
# })