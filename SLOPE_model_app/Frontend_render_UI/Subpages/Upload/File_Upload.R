# create RDS object for model
observeEvent(input$upload_model,ignoreInit = TRUE,{
  req(!is.null(input$upload_model$name))
  
  # read in RDS object
  rds_model <- readRDS(input$upload_model$datapath)
  omicsData$rds_model <- rds_model
})

# create data.frame for new edata that is uploaded
observeEvent(input$upload_edata,ignoreInit = TRUE,{
  req(input$upload_edata)
  req(!is.null(input$upload_edata$name))
  print("File upload event triggered")
  # read in RDS object
  new_edata <- tryCatch({
    read.csv(file_path)
  }, error = function(e) {
    showNotification(paste("Error in reading CSV file:", e$message), type = "error")
    NULL
  })
  # Check if data is read successfully
  if (!is.null(new_edata)) {
    omicsData$new_edata <- new_edata
    print("CSV data loaded successfully")
    print(head(omicsData$new_edata))  # Debugging line
  } else {
    print("Failed to load CSV data")  # Debugging line
  }
})

# create data.frame for new fdata that is uploaded
observeEvent(input$upload_fdata,ignoreInit = TRUE,{
  req(!is.null(input$upload_fdata$name))
  
  # read in RDS object
  new_fdata <- read.csv(input$upload_fdata$datapath)
  omicsData$new_fdata <- new_fdata
})