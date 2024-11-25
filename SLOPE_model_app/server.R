options(shiny.maxRequestSize = 250 * 1024^2, 
        ch.dir = TRUE, 
        expressions = 5e5,
        DT.TOJSON_ARGS = list(na = "string"))

shinyServer(function(session,input,output){
  observeEvent(input$Browser,{
    browser()
  })
})