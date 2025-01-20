# Main App UI
top_ui <- fluidPage(
  uiOutput("dynamic_ui")
)

# Main App Server
# Main Server
top_server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 250 * 1024^2,
          ch.dir = TRUE,
          expressions = 5e5,
          DT.TOJSON_ARGS = list(na = "string"),
          shiny.fullstacktrace=TRUE,
          renv.config.sandbox.enabled = FALSE
  )

  ggplot2::theme_set(ggplot2::theme_bw())
  
  observeEvent(session$clientData$url_search, once = T, {
    # Parse query string
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$model)) {
      
      # Dynamically source predict app
      source(file.path("./Predict_app/global.R"), local = F)
      source(file.path("./Predict_app/ui.R"), local = F)

      # Set the dynamic UI and call the server function
      output$dynamic_ui <- renderUI(ui)

    } else {
      
      # Dynamically source App 2
      source(file.path("./Main_app/global.R"), local = F)
      source(file.path("./Main_app/ui.R"), local = F)
      
      # Set the dynamic UI and call the server function
      output$dynamic_ui <- renderUI(ui)

    }
  })
  
  observeEvent(input$top_page, once = T, {
    
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$model)) {

      source(file.path("./Predict_app/server.R"), local = T)
      
    } else {
      
      # Dynamically source App 2
      source("./Main_app/server.R", local = T)

    }
    
  })
  
}



# Combine Main UI and Server
shinyApp(ui = top_ui, server = top_server)
