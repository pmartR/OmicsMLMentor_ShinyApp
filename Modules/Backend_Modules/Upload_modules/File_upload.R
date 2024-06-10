# File upload module

# Includes:
## Uploading files
## Loading example files
## Disable clear for example data
## Clearing loaded files
## Generating preview datatables

## Parent input:
# input$use_example
# input$data_type
# input$data_select
# Preview tabset named "preview_data"

### Fdata seperation ###
# Altered preview tabset to "preview_data_f_data"
# Altered input$use_example to input$use_example_fdata

## Accessible UI via:
# fileinput_UI("e_data_upload", "e_data", is_RNA = F)
# (Basically, adding these to your UI page or a renderUI will generate appropriate UI)

## Please add a static tabset to FileInputServer for previewing data as well!
# preview_tabset <- "preview_data"

## Accessible resulting files via (check module reactives):
## reactive_dataholder

## Notes for use in future apps:

## Use or change name of reactive_dataholder for storing files before creating omics object
## Likewise for input$use_example for example data, input$data_type for specifying what kind of omics, and input$data_select for what data files are available
## data_select may need adjusting depending on what files are required for your application
## The name of the preview tabset is dependent on static UI and may need changing, currently set to "preview_data"
## Note: File handling is the only true module, where its own namespace is created. 
### This is due to the requirement of multiple UI pieces that rely on the result of this module, but don't do calculations

## Module Functions ##

## UI function for upload
fileinput_UI <- function(id, label = "e_data", is_RNA) {
  ns <- NS(id)
  
  tablabel <- switch(label,
                     e_data = ifelse(is_RNA, 
                                     "Expression data .csv",
                                     "Abundance data .csv"),
                     f_data = "Sample data .csv",
                     e_meta = "Biomolecule information .csv"
  )

  tagList(
    # wellPanel(
    splitLayout(cellWidths = c("75%", "25%"),
                fileInput(ns("file"), label = tablabel),
                div(br(), actionButton(ns("clear_file"), label = "Clear file"))
                )
    # )
  )
}

## Observer function for loading into RV and updating preview datatable tabs
FileInputServer <- function(id, 
                          use_example, ## Whether to load up example data or not
                          data_type, ## Determine what example data to load
                          data_select, ## For example data, only load if its actually needed
                          label, ## e_data, f_data, or e_meta 
                          use_AWS, ## AWS flag
                          preview_tabset) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      
      # parent session (for tabset appending)
      parentSession <- get("session", envir = parent.frame(2))
      
      store_data <- reactiveValues(filename = NULL, file = NULL)
      
      ## If a file is uploaded, load it up
      observeEvent(input$file, ignoreInit = TRUE, {
        
        req(!is.null(input$file$name))
        
        tablabel <- switch(label,
                           e_data = ifelse(data_type() == "RNA-seq", 
                                           "Expression data",
                                           "Abundance data"),
                           f_data = "Sample data",
                           e_meta = "Biomolecule information"
        )
        
        ## Remove prior
        removeTab(preview_tabset, tablabel, session = parentSession)
        
        ## make sure if dt changes remove prior
        if(tablabel == "Abundance data"){
          removeTab(preview_tabset, "Expression data", session = parentSession)
        } else if(tablabel == "Expression data"){
          removeTab(preview_tabset, "Abundance data", session = parentSession)
        }
        
        data_select_val <- c("e_data", data_select())
        if(label %in% data_select_val){
          
          store_data$filename <- input$file$name
          store_data$file <- default_factor(read.csv(input$file$datapath))
          
          appendTab(preview_tabset, 
                    select = T,
                    tabPanel(tablabel,
                             br(),
                             DTOutput(paste0("DT_", label)),
                             br()
                    ),
                    session = parentSession
          )
        } else {
          
          if(label == "f_data") 
          store_data$filename <- NULL
          store_data$file <- NULL
        }
        
      })
      
      ## If example files are used, load it up
      observeEvent(c(use_example(), data_type(), data_select()), ignoreInit = TRUE, {
        
        use_example_val <- use_example()
        data_type_val <- data_type()
        data_select_val <- c("e_data", data_select())
        
        req(use_example_val && !is.null(data_type_val) && !is.null(data_select_val))
        
        tablabel <- switch(label,
                           e_data = ifelse(data_type() == "RNA-seq", 
                                           "Expression data",
                                           "Abundance data"),
                           f_data = "Sample data",
                           e_meta = "Biomolecule information"
        )
        
        removeTab(preview_tabset, tablabel, session = parentSession)
        
        if(label %in% data_select_val){
          pmartRdata_prefix <- switch(data_type_val,
                                      "Protein" = "pro_", 
                                      "Label-free" = "pep_", 
                                      "Isobaric" = "isobaric_", 
                                      "Negative" = "lipid_neg_", 
                                      "Positive" = "lipid_pos_", 
                                      "NMR" = "nmr_identified_", 
                                      "GC-MS" = "metab_", 
                                      "RNA-seq" = "rnaseq_"
          )
          
          
          data_grab <- get(paste0(pmartRdata_prefix, sub("_", "", label)))
          
          store_data$filename <- "Example file"
          store_data$file <- default_factor(data_grab)
          
          appendTab(preview_tabset, 
                    select = T,
                    tabPanel(tablabel,
                             br(),
                             DTOutput(paste0("DT_", label)),
                             br()
                    ),
                    session = parentSession
          )
        } else {
          store_data$filename <- NULL
          store_data$file <- NULL
        }
      })
      
      ## If AWS, load it up
      observeEvent(c(use_AWS, data_type(), data_select()), ignoreInit = TRUE, {

        req(use_AWS)
        
        tablabel <- switch(label,
                           e_data = ifelse(data_type() == "RNA-seq",
                                           "Expression data",
                                           "Abundance data"),
                           f_data = "Sample data",
                           e_meta = "Biomolecule information"
        )

        removeTab(preview_tabset, tablabel, session = parentSession)

        use_example_val <- use_example()
        data_select_val <- c("e_data", data_select())

        if(label %in% data_select_val){

          store_data$filename <- "Loaded from S3 bucket"
          store_data$file <- default_factor(AWSobj[[label]])

          appendTab(preview_tabset,
                    select = T,
                    tabPanel(tablabel,
                             br(),
                             DTOutput(paste0("DT_", label)),
                             br()
                    ),
                    session = parentSession
          )
        } else {
          store_data$filename <- NULL
          store_data$file <- NULL
        }
      })
      
      #' @details Parse and store header parameters.  If we see the 'map-data' prefix, load that up!
      observe({
        query <- parseQueryString(session$clientData$url_search)
        
        # establish minio connection if we are pulling cloud resources
        if (any(names(query) %in% VALID_MINIO_HEADER_PARAMS)) {
          isolate({
            # store header params in a reactive variable
            for (key in names(query)) {
              header_params[[key]] <- query[[key]]
              message(sprintf("INFO: stored parameter %s: %s", key, query[[key]]))
            }
            
            if ('map-data' %in% names(query)) {
              # get the appropriate minio config for retrieving CoreMS files
              cfg_location = if (Sys.getenv("MINIO_CONFIG_PATH") == "") "./cfg/minio_config.yml" else Sys.getenv("MINIO_CONFIG_PATH")
              minio_con <<- mapDataAccess::map_data_connection(cfg_location)
              
              html(selector = "#loading-gray-overlay > div", html = "Loading MAP data...")
              
              # TODO:: here test if the thing is a folder, or a midpoint.  We'll probably check the tags to do this.
              
              uris <- reticulate::iterate(
                minio_con$client$list_objects(
                  minio_con$bucket,
                  prefix = header_params[['map-data']],
                  recursive = TRUE),
                function(x) x$object_name
              )
              
              if (length(uris) > 0) {
                minio_upload_success <- tryCatch({
                  fpaths <- lapply(uris, function(uri) {
                    mapDataAccess::get_file(
                      cms_minio_con, id = uri, filename = file.path(tempfile(), basename(uri)),
                      use_dir = FALSE
                    )
                  })
                  
                  modalmessage <- store_corems(fpaths)
                  TRUE
                  
                }, error = function(e) {
                  modalmessage <<- div(sprintf(info_text[["MINIO_UPLOAD_ERROR"]], e))
                  FALSE
                })
              } else {
                corems_upload_success <- FALSE
                modalmessage <- div(info_text[["COREMS_UPLOAD_NOSAMPS"]])
              }
              
              if (corems_upload_success) {
                # defined in srv_ui_elements/corems_UI.R
                showModal(corems_upload_success_modal(modalmessage)) 
              } else {
                showNotification(
                  modalmessage,
                  duration = NULL,
                  type = "error"
                )
              }
            } else if ('map-object' %in% names(query)) {
              # get the appropriate minio config for retrieving CoreMS files
              cfg_location = if (Sys.getenv("MAP_MINIO_CONFIG_PATH") == "") "./cfg/minio_config_map.yml" else Sys.getenv("MAP_MINIO_CONFIG_PATH")
              map_minio_con <<- mapDataAccess::map_data_connection(cfg_location)
              
              html(selector = "#loading-gray-overlay > div", html = "Loading data from MAP...")
              
              ftms_obj <- tryCatch({
                mapDataAccess::get_data(map_minio_con, header_params[['map-object']])
              }, error = function(e) {
                NULL
              })
              
              valid_obj_types = c("project omic")
              
              if (!inherits(ftms_obj, valid_obj_types)) {
                showNotification(
                  HTML(sprintf("Something went wrong retrieving your data from MAP.  Your MAP object was not of the appropriate type.  Please confirm that the object is of types:  [%s]", valid_obj_types)),
                  duration = NULL,
                  type = "error"
                )
              } else {
                # store in some reactive variable that e_data and e_meta....essentially check exists and use if so.
                revals$map_project <- ftms_obj 
              }
            }
          })
          
        } else if (length(names(query)) > 0) {
          showNotification(
            sprintf("No valid header parameters found in query string.  Found parameters: (%s).  Valid header parameters: (%s).  Presenting manual upload dialog.", paste(names(query), collapse = ", "), paste(VALID_MINIO_HEADER_PARAMS, collapse = ", ")),
            duration = NULL,
            type = 'error'
          ) 
        }
        
        on.exit({
          Sys.sleep(1)
          hide("loading-gray-overlay")
        })
        
        isolate({
          # if we're not coming from minio (CoreMS files or MAP project object), ask whether they have multiple CoreMS files to upload or a single unified file.
          if ((length(corems_revals[['combined_tables']]) == 0) && is.null(revals$map_project)) {
            showModal(upload_type_modal())
          } else if (!is.null(revals$map_project)) {
            insertTab(
              "top_page",
              target = "Welcome",
              tab = upload_tab(),
              position = "after"
            )
            showModal(map_upload_modal())
          } else {
            insertTab(
              "top_page",
              target = "Welcome",
              tab = upload_tab(from_corems = TRUE),
              position = "after"
            )
          }
        })
      })
      
      ## Disable clear file for example data
      observeEvent(c(use_example()), {
        use_example_val <- use_example() || use_AWS
        toggleState(id = "clear_file", condition = !use_example_val)
      })
      
      ## On clear file, reset holder
      observeEvent(input$clear_file, {
        
        if(label == "f_data") 
        store_data$filename <- NULL
        store_data$file <- NULL
        reset("file")
      })
      
      return(store_data)
    }
  )
}


## Module UI is located in UI of page of interest (upload) ##

## Module Servers and DTs ##
purrr::map(c("e_data", "f_data", "e_meta"), function(label){
  preview_tabset <- "preview_data"
  
  if(label == "f_data"){
    pt <- paste0(preview_tabset, "_f_data")
    UE <- reactive(input$use_example_fdata)
    ds <- reactive(input$use_fdata)
  } else{
    pt <- preview_tabset
    UE <- reactive(input$use_example)
    ds <- reactive(input$data_select)
  }
  
  req(!all(map_lgl(c(pt, UE, ds), is.null)))
  
  reactive_dataholder[[label]] <- FileInputServer(paste0(label, "_upload"), 
                                                  use_example= UE,
                                                  data_type = reactive(input$data_type),
                                                  data_select = ds,
                                                  label = label,
                                                  preview_tabset = pt,
                                                  use_AWS = AWS
  )
  
  ## Render from uploaded files
  output[[paste0("DT_", label)]] <- renderDT({

    reactive_dataholder[[label]]$file
  },
  selection = 'none',
  options = list(dom = 'tpi',
    pageLength = 10,
    scrollX = T

    ))
  
  outputOptions(output, paste0("DT_", label), suspendWhenHidden = T)
  ## Odd behavior -- if two files update simultaneously, sometimes the first table doesn't load if suspendWhenHidden = F
  ## E.g. Selecting both abundance data and biomolecule info, then hitting use example data
  
})

