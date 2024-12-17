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
      proteomicsTMT = "Isobaric",
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
                              "(E.g., KEGG pathways, genes, proteins)",
                              "(E.g., KEGG pathways, lipid class, molecular activity)"
  )
  
  
  choices <- c("e_data","e_meta")
  names(choices) <- c(label_edata, label_emeta)
  
  div(
    p(tags$b("What experimental files do you have?")),
    
    disabled(prettySwitch("have_edata", label_edata, value = T, fill = T, status = "primary")),
    p("Required", style = "margin-top: -10px;"),
    br(),
    if (input$data_type %in% c("Label-free", "Isobaric")) {
      tagList(
        disabled(prettySwitch("have_emeta", label_emeta, value = T, fill = T, status = "primary")),
        div("Required", relevant_examples, style = "margin-top: -10px;")
      )
    } else if (!is.null(AWSobj$e_meta)){
      tagList(
        disabled(prettySwitch("have_emeta", label_emeta, 
                              value = T, fill = T, status = "primary")),
        div("Optional", relevant_examples, style = "margin-top: -10px;")
      )
    } else {
      tagList(
        disabled(prettySwitch("have_emeta", label_emeta, 
                              value = F, fill = T, status = "primary")),
        div("Optional", relevant_examples, style = "margin-top: -10px;")
      )
    },
    br()
  )
  
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
        
        id <- query$id
        if(is.null(id)) id <- 9999
        
        file <- paste0("SLOPE_model_full_", gsub("( |:|-)", "_", Sys.time()), ".RDS")
        
        new_folder <- "slope_models"
        
        model_type <- attr(omicsData$objRM, "SLOPE_model_name")
        response_performance <- attr(omicsData$objRM, "response_performance")
        
        if(is.null(response_performance)){
          response_performance <- "Not applicable"
          response_type <- "Not applicable"
        } else if(response_types_ag() != "continuous"){
          
          response_performance <- toString(paste0(apply(response_performance, 1, paste, collapse = "-"), "%"))
          response_type <- "Categorical"
        } else {
          response_performance <- apply(response_performance, 1, paste, collapse = "-")
          response_type <- "Continuous"
        }
        
        update_main_df <- data.frame(
          id = id,
          filename = file,
          datatype = query$datatype,
          model_class = ifelse(supervised(), "supervised", "unsupervised"),
          model_type = model_type, 
          response_type = response_type,
          response_performance = response_performance
        )
        
        saveRDS(omicsData$objRM, file = "SLOPE_model.RDS")
        
        aws.s3::put_object(
          file = "SLOPE_model.RDS",
          bucket = gsub("merged_files.+", new_folder, query$s3_bucket),
          object = file.path(id, file)
          )
        file.remove("SLOPE_model.RDS")
        
        if(!is.null(omicsData$objRM_reduced)){
          
          
          file <- paste0("SLOPE_model_reduced_", gsub("( |:|-)", "_", Sys.time()), ".RDS")
          model_type <- attr(omicsData$objRM_reduced, "SLOPE_model_name")
          response_performance <- attr(omicsData$objRM_reduced, "response_performance")
          
          if(is.null(response_performance)){
            response_performance <- "Not applicable"
            response_type <- "Not applicable"
          } else if(nrow(response_performance) > 1){
            response_performance <- toString(paste0(apply(response_performance, 1, paste, sep = "-"), "%"))
            response_type <- "Categorical"
          } else {
            response_performance <- apply(response_performance, 1, paste, sep = "-")
            response_type <- "Continuous"
          }
          
          update_main_df2 <- data.frame(
            id = id,
            filename = file,
            datatype = query$datatype,
            model_class = ifelse(supervised(), "supervised", "unsupervised"),
            model_type = model_type, 
            response_type = response_type,
            response_performance = response_performance
          )
          update_main_df <- rbind(update_main_df, update_main_df2)
          
          saveRDS(omicsData$objRM_reduced, file = "SLOPE_model.RDS")
          
          aws.s3::put_object(
            file = "SLOPE_model.RDS",
            bucket = gsub("merged_files.+", new_folder, query$s3_bucket),
            object = file.path(id, file)
          )
          file.remove("SLOPE_model.RDS")
        }
        
        
        tryCatch({
          
          og_df <- s3read_using(FUN = read.csv,
                                 object = "SLOPE_model_tags.csv", 
                                 bucket = gsub("merged_files.+", new_folder, query$s3_bucket))
          
          update_main_df <- rbind(og_df, update_main_df)
          
        }, error = function(e){
          
          if(e$message != "Not Found (HTTP 404)."){
            sendSweetAlert(
              session, 
              "AWS upload Error", 
              e$message)
          }
          
        })
        
        s3write_using(update_main_df, 
                      FUN = write.csv, 
                      row.names = FALSE,
                      object = "SLOPE_model_tags.csv", 
                      bucket = gsub("merged_files.+", new_folder, 
                                    query$s3_bucket))
        
        
        
      }, error = function(e){
        
        sendSweetAlert(
          session, 
          "AWS upload Error", 
          e$message)
        
      })
      
    }
    
    out
    
  },
  contentType = "application/zip"
)
      
      
