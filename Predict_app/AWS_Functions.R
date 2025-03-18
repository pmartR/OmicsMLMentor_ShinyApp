## Functionality that was added for AWS
## Last Updated: March 1st, 2022

observeEvent(input$`__startup__`, {

  # Parse the query string at the url header
  query <- parseQueryString(session$clientData$url_search)
  
  # Set a conditional test. We only care if the "s3" parameter exists.
  cond <- length(query) != 0 && "s3_bucket" %in% names(query)
  
  # If true, put data from the AWS bucket where it belongs
  if (cond) {
    
      # Create a loading screen
      html("loading-gray-overlay",
           paste0("<div class='fadein-out busy relative-centered',", 
                  "style='font-size:xx-large'>Pulling data from S3 bucket...</div>")
      )
      
      cat(file=stderr(), "Test 1")
      
      # Load into AWS obj
      csv_reader <- function(x) {read.csv(x, check.names = FALSE)}
      AWSobj$e_data <- unique(s3read_using(FUN = csv_reader, object = query$e_data, bucket=query$s3_bucket))
      if(!is.null(query$e_meta)){
        AWSobj$e_meta <- s3read_using(FUN = csv_reader, object = query$e_meta, bucket=query$s3_bucket)
        ## Temp fix for razor proteins
        AWSobj$e_meta <- unique(AWSobj$e_meta[colnames(AWSobj$e_meta) != "Proteins"])
      }
      if(!is.null(query$f_data)){
        AWSobj$f_data <- unique(s3read_using(FUN = csv_reader, object = query$f_data, bucket=query$s3_bucket))
      }
      
      ## Find the associated id (passed id in URL likely data specific and not model specific)
      model <- query$model
      new_folder <- "slope_models"
      
      og_df <- s3read_using(FUN = read.csv,
                            object = "SLOPE_model_tags.csv", 
                            bucket = gsub("merged_files.+", new_folder, query$s3_bucket))
      
      id_use <- og_df$id[og_df$filename == model][1]
      
      # fp <- "./Predict_app/example/data/demo/rf_new.RDS"
      # 
      # AWSobj$model <- readRDS(fp)
      
      AWSobj$model <- s3read_using(FUN = readRDS, object = file.path(id_use, model),
                                   bucket=gsub("merged_files.+", new_folder, query$s3_bucket))
      
      cat(file=stderr(), "Test 2")
      
  }
  
  
  
  cat(file=stderr(), "Test 5")

  # Exit loading screen
  on.exit({
    Sys.sleep(2)
    hide("loading-gray-overlay")
  })

}, priority = -10, ignoreNULL = FALSE, once = TRUE)


observeEvent(input$load_example, ignoreNULL = F, {
  if(!is.null(input$load_example)) disable(id = "load_example")
})


observeEvent(input$model_file, ignoreNULL = F, ignoreInit = F, {
  updateBoxCollapse(session, "", open = c("params_box", "data_props_fdata", "data_props"), 
                    close = c("model_upload", "data_upload_edata", "data_upload_fdata", "data_upload_emeta"))
})

observeEvent(input$specify_fdata_done, ignoreNULL = F, ignoreInit = F, {
  enable("specify_fdata_done")
})
      
      
