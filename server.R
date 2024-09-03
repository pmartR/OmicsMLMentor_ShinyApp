options(shiny.maxRequestSize = 250 * 1024^2, 
        ch.dir = TRUE, 
        expressions = 5e5,
        DT.TOJSON_ARGS = list(na = "string"))

shinyServer(function(session, input, output) {
  
  file_loads <- c(
    list.files("./Modules/", recursive = T, full.names = T),
    list.files("./Frontend_render_UI/", recursive = T, full.names = T)
  )
  
  file_loads <- file_loads[file_loads != "./Modules//Module_RV_all.R"]
  source("./Modules//Module_RV_all.R", local = T) ## must be first
  
  # file_loads <- grep("filter", file_loads, invert = T, value = T)
  
  for (f in grep(".R$", file_loads, value = T)){
    source(f, local = TRUE)
  }
  
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
      source("./AWS_Functions.R", local = TRUE)
      
      ######## comment this out before push
      
      # wd <-"../"
      # 
      # AWSobj$e_data <- read.csv(file.path(wd, "e_data_omni.csv"))
      # AWSobj$f_data <- read.csv(file.path(wd, "f_data_omni.csv"))
      # AWSobj$e_meta <- read.csv(file.path(wd, "e_meta_omni.csv"))
      #
      
      
      ## Temp fix for razor proteins
      observeEvent(AWSobj$e_data, {
        AWSobj$e_data <- AWSobj$e_data[apply(!is.na(AWSobj$e_data), 2, sum) > 2,]
        
        # write.csv(AWSobj$e_data, "e_data_short.csv", row.names = F)
        # write.csv(AWSobj$f_data, "f_data_short.csv", row.names = F)
        # write.csv(AWSobj$e_meta, "e_meta.csv", row.names = F)
      }, once = T)
      
      observeEvent(AWSobj$e_meta, {
        AWSobj$e_meta <- unique(AWSobj$e_meta[colnames(AWSobj$e_meta) != "Proteins"])
      }, once = T)
      
      # # Specify file type and disable input
      # updatePickerInput(session, "data_type", selected = "Label-free")
      # 
      launch_tutorial()
    } else if (MAP_ACTIVE){
      
      source("./MAP_Functions.R", local = TRUE)
      
    } else {
      hide("loading-gray-overlay")
      launch_tutorial()
    }
     
  })
  
  # Observe any collapsible panels
  observeEvent(input$collapseTitleClick, {
    req(input$collapseTitleClick)
    updateBoxCollapse(session, input$collapseTitleClick$p, toggle = input$collapseTitleClick$id)
  })
})
