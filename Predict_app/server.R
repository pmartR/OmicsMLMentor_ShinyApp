formals(renderDT)$server <- FALSE
  
  source("./Predict_app/RV_all.R")
  
  file_loads <- c(
    list.files("./Predict_app/Frontend_render_UI", recursive = T, full.names = T)
  )
  
  for (f in grep(".R$", file_loads, value = T)) source(f, local = TRUE)
  
  # BROWSER
  observeEvent(input$Browser,{
    browser()
  })
  
  observeEvent(input$collapseTitleClick, {
    req(input$collapseTitleClick)
    updateBoxCollapse(session, input$collapseTitleClick$p, toggle = input$collapseTitleClick$id)
  })
  
  
  observeEvent(session$clientData$url_search, once = T, {
    
    # Parse the query string at the url header
    query <- parseQueryString(session$clientData$url_search)
    
    # Set a conditional test. We only care if the "s3" parameter exists.
    AWS_cond <- length(query) != 0 && "s3_bucket" %in% names(query)
    MAP_cond <- length(query) != 0 && 'map-data' %in% names(query)
    
    if(AWS_cond) AWS <<- T else if(MAP_cond) MAP_ACTIVE <<- T
    
    if (AWS) {
      
      message("AWS VERSION ENABLED")
      
      # Load AWS specific R library
      library(aws.s3)
      
      # Load AWS specific code
      source("./Predict_app/AWS_Functions.R", local = TRUE)
      
      ######## comment this out before push
      
      
      ## Temp fix for razor proteins
      observeEvent(AWSobj$e_data, {
        AWSobj$e_data <- AWSobj$e_data[apply(!is.na(AWSobj$e_data), 2, sum) > 2,]
        
      }, once = T)
      
      observeEvent(AWSobj$e_meta, {
        AWSobj$e_meta <- unique(AWSobj$e_meta[colnames(AWSobj$e_meta) != "Proteins"])
      }, once = T)

    } else if (MAP_ACTIVE){
      
      source("./Predict_app/MAP_Functions.R", local = TRUE)
      
    } else {
      hide("loading-gray-overlay")
    }
    
  })
