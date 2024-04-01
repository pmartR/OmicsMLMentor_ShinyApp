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
    AWSobj$e_data <- s3read_using(FUN = csv_reader, object = query$e_data, bucket=query$s3_bucket)
    if(!is.null(query$e_meta)){
      AWSobj$e_meta <- s3read_using(FUN = csv_reader, object = query$e_meta, bucket=query$s3_bucket)
      ## Temp fix for razor proteins
      AWSobj$e_meta <- unique(AWSobj$e_meta[colnames(AWSobj$e_meta) != "Proteins"])
    }
    if(!is.null(query$f_data)){
      AWSobj$f_data <- s3read_using(FUN = csv_reader, object = query$f_data, bucket=query$s3_bucket)
    }
    
    cat(file=stderr(), "Test 2")

    # Specify file type and disable input
    datatype <- query$datatype
    
    cat(file=stderr(), "Test 3")
    cat(file=stderr(), datatype)
    
    set_dt <- switch(datatype,
      proteomics = "Label-free",
      lipidomics = "Negative",
      transcriptomics= "RNA-seq",
      metabolomics = "GC-MS"
    )
    
    cat(file=stderr(), "Test 4")
    
    if(!is.null(AWSobj$e_meta)){
      
      updatePickerInput(session, "data_select", 
                        selected = c("e_data", "e_meta"))
    }
    
    updatePickerInput(session, "data_type", selected = set_dt)
  }
  
  
  
  cat(file=stderr(), "Test 5")

  # Exit loading screen
  on.exit({
    Sys.sleep(2)
    hide("loading-gray-overlay")
  })

}, priority = -10, ignoreNULL = FALSE, once = TRUE)


observeEvent(input$data_type, {
  if(!is.null(input$data_type)) disable(id = "data_type")
})

output$data_select_UI <- renderUI({
  
  req(!is.null(input$data_type))
  
  label_edata <- ifelse(input$data_type == "RNA-seq", 
                        "Expression data", "Abundance data")
  label_emeta <- ifelse(input$data_type == "RNA-seq", 
                        "Transcript information", "Biomolecule information")
  relevant_examples <- ifelse(input$data_type == "RNA-seq", 
                              "Optional (E.g., KEGG pathways, genes, proteins)",
                              "Optional (E.g., KEGG pathways, lipid class, molecular activity)"
  )
  
  
  choices <- c("e_data","e_meta")
  names(choices) <- c(label_edata, label_emeta)
  
  disabled(pickerInput(
    "data_select",
    "What experimental files do you have?",
    multiple = T,
    choices = choices,
    choicesOpt = list(
      disabled = c(1,0),
      subtext = c("Required",
                  relevant_examples
      )),
    selected = if(!is.null(AWSobj$e_meta)) c("e_data", "e_meta") else "e_data"
  ))
})

observeEvent(input$use_example, {
  if(!is.null(input$use_example)) disable(id = "use_example")
})


output$download_processed_data <- downloadHandler(
  filename = paste0("SLOPE_output_", gsub("( |:|-)", "_", Sys.time()), ".zip"),
  content = function(fname) {
    
    # this is necessary to zip up the entire trelliscope directory
    # specifically, we dont want to have to use the -j flag to get non-directory files..
    # .. so, we navigate to where everything is (tempdir()) and download with just -r
    
    orig_wd <- getwd()
    on.exit(setwd(orig_wd))
    setwd(tempdir())
    
    out <- zip(zipfile = fname, files = zipped_file$fs, flags = "-r")
    
    if (file.exists(paste0(fname, ".zip"))) {
      file.rename(paste0(fname, ".zip"), fname)
    }
    
    tryCatch({
      # # Parse the query string at the url header
      query <- parseQueryString(session$clientData$url_search)
      
      # Set a conditional test. We only care if the "s3" parameter exists.
      cond <- length(query) != 0 && "s3_bucket" %in% names(query)
      
    }, error = function(e){
      
      cond <- FALSE
      
      sendSweetAlert(
        session, 
        "AWS parseQueryString error", 
        e$message)
      
    })
    
    
    if(cond){
      
      tryCatch({
        
        new_folder <- "slope_output"
        
        aws.s3::put_object(
          out,
          bucket = gsub("merged_files.+", new_folder, query$s3_bucket),
          object = paste0(query$id,
                          "/",
                          paste0("SLOPE_output_", gsub("( |:|-)", "_",
                                                       Sys.time()), ".zip")))
        
      }, error = function(e){
        
        sendSweetAlert(
          session, 
          "AWS download Error", 
          e$message)
        
      })
      
    }
    
    out
    
  },
  contentType = "application/zip"
)
      
      
