# File upload module

# Includes:
## Uploading files
## Loading example files
## Disable clear for example data
## Clearing loaded files
## Generating preview datatables

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

#'@details Disable/modify all inputs on the Upload tab that are pre-determined if a user comes in with data from map:
#' - e_data file input 'e_data_file'
#' - e_meta file input 'e_meta_file'
#' - PickerInput 'data_type' (if DataType is present)
#' - Prettyswitch 'have_emeta' (if e_meta file is present)

## UI function for upload
fileinput_UI <- function(id, label = "e_data", is_RNA) {
  
  tablabel <- switch(label,
                     e_data = ifelse(is_RNA, 
                                     "Expression data .csv",
                                     "Abundance data .csv"),
                     f_data = "Sample Information .csv",
                     e_meta = "Biomolecule information .csv"
  )
  
  tagList(
    # wellPanel(
    splitLayout(cellWidths = c("75%", "25%"),
                div(
                  id = sprintf("js_%s_file", label),
                  fileInput(paste0(label, "_file"), label = tablabel) 
                ),
                div(br(), actionButton(paste0(label, "_clear_file"), 
                                       label = "Clear file"))
    )
    # )
  )
}

input_data_types <- reactive({
  
  req(!is.null(input$data_type))
  
  # e_data
  all_input_data_types <- "e_data"
  
  # f_data
  all_input_data_types <- c(
    all_input_data_types,
    if (!is.null(input$use_fdata) &&
        input$use_fdata == "f_data") 
      "f_data" 
    else 
      NULL
  )
  
  # e_meta
  all_input_data_types <- c(
    all_input_data_types,
    if (input$data_type %in% c("Label-free", "Isobaric") ||
        isTruthy(input$have_emeta)) 
      "e_meta" 
    else 
      NULL
  )
  
  all_input_data_types
})

purrr::map(c("e_data", "f_data", "e_meta"), function(label){
  
  preview_tabset <- "preview_data"
  
  if(label == "f_data"){
    preview_tabset <- paste0(preview_tabset, "_f_data")
  } else{
    preview_tabset <- preview_tabset
  }
  
  ## If a file is uploaded, load it up
  observeEvent(input[[paste0(label, "_file")]], ignoreInit = TRUE, {

    req(label %in% input_data_types())
    req(!is.null(input$data_type))
    
    req(!is.null(input[[paste0(label, "_file")]]$name))
    
    tablabel <- switch(label,
                       e_data = ifelse(input$data_type == "RNA-seq", 
                                       "Expression data",
                                       "Abundance data"),
                       f_data = "Sample Information",
                       e_meta = "Biomolecule information"
    )
    
    ## Remove prior
    removeTab(preview_tabset, tablabel, session = session)
    
    ## make sure if dt changes remove prior
    if(tablabel == "Abundance data"){
      removeTab(preview_tabset, "Expression data", session = session)
    } else if(tablabel == "Expression data"){
      removeTab(preview_tabset, "Abundance data", session = session)
    }
    
    data_select_val <- input_data_types()
    if(label %in% data_select_val){
      
      reactive_dataholder[[label]]$filename <- input[[paste0(label, "_file")]]$name
      reactive_dataholder[[label]]$file <- default_factor(
        read.csv(input[[paste0(label, "_file")]]$datapath))
      
      appendTab(preview_tabset, 
                select = T,
                tabPanel(tablabel,
                         br(),
                         DTOutput(paste0("DT_", label)),
                         br()
                ),
                session = session
      )
    } else {
      
      if(label == "f_data") 
        reactive_dataholder[[label]]$filename <- NULL
      reactive_dataholder[[label]]$file <- NULL
    }
    
  })
  
  ## If example files are used, load it up
  
  if(label == "f_data"){
    observeEvent(c(
      input$use_example_fdata, 
      input$data_type,
      input$data_select
    ), ignoreInit = TRUE, {
      
      req(!is.null(input$data_type))
      
      req(label %in% input_data_types())
      
      use_example_val <- input$use_example_fdata
      data_type_val <- input$data_type
      data_select_val <- input_data_types()
      
      req(use_example_val && !is.null(data_type_val))
      
      tablabel <- switch(label,
                         e_data = ifelse(input$data_type == "RNA-seq", 
                                         "Expression data",
                                         "Abundance data"),
                         f_data = "Sample Information",
                         e_meta = "Biomolecule information"
      )
      
      removeTab(preview_tabset, tablabel, session = session)
      
      ## make sure if dt changes remove prior
      if(tablabel == "Abundance data"){
        removeTab(preview_tabset, "Expression data", session = session)
      } else if(tablabel == "Expression data"){
        removeTab(preview_tabset, "Abundance data", session = session)
      }
      
      if(input$use_fdata == "f_data"){
        pmartRdata_prefix <- switch(data_type_val,
                                    "ProteinTMT" = "pro_", 
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
        
        reactive_dataholder[[label]]$filename <- "Example file"
        reactive_dataholder[[label]]$file <- default_factor(data_grab)
        
        appendTab(preview_tabset, 
                  select = T,
                  tabPanel(tablabel,
                           br(),
                           DTOutput(paste0("DT_", label)),
                           br()
                  ),
                  session = session
        )
      } else {
        reactive_dataholder[[label]]$filename <- NULL
        reactive_dataholder[[label]]$file <- NULL
      }
    })
    
    observeEvent(c(input$use_example_fdata), {
      use_example_val <- input$use_example_fdata || AWS
      toggleState(id = paste0(label, "_clear_file"), 
                  condition = !use_example_val)
    })
    
    
  } else {
    
    observeEvent(c(input$use_example, input$data_type, 
                   input$data_select), ignoreInit = TRUE, {
                     
                     req(!is.null(input$data_type))
                     
                     req(label %in% input_data_types())
                     
                     use_example_val <- input$use_example
                     data_type_val <- input$data_type
                     data_select_val <- input_data_types()
                     
                     req(use_example_val && !is.null(data_type_val))
                     
                     tablabel <- switch(label,
                                        e_data = ifelse(input$data_type == "RNA-seq", 
                                                        "Expression data",
                                                        "Abundance data"),
                                        f_data = "Sample Information",
                                        e_meta = "Biomolecule information"
                     )
                     
                     removeTab(preview_tabset, tablabel, session = session)
                     
                     ## make sure if dt changes remove prior
                     if(tablabel == "Abundance data"){
                       removeTab(preview_tabset, "Expression data", session = session)
                     } else if(tablabel == "Expression data"){
                       removeTab(preview_tabset, "Abundance data", session = session)
                     }
                     
                     if(label %in% data_select_val){
                       pmartRdata_prefix <- switch(data_type_val,
                                                   "Protein" = "pro_", 
                                                   "ProteinTMT" = "pro_", 
                                                   "Label-free" = "pep_", 
                                                   "Isobaric" = "isobaric_", 
                                                   "Negative" = "lipid_neg_", 
                                                   "Positive" = "lipid_pos_", 
                                                   "NMR" = "nmr_identified_", 
                                                   "GC-MS" = "metab_", 
                                                   "RNA-seq" = "rnaseq_"
                       )
                       
                       data_grab <- NULL
                       
                       tryCatch({
                         data_grab <- get(paste0(pmartRdata_prefix, sub("_", "", label)))
                       }, error = function(e) {
                         updatePrettySwitch(session, "have_emeta", value = FALSE)
                         shinyalert(title = "Data Unavailable", "Example biomolecule information for this data type is unavailable.")
                       })
                       
                       if (is.null(data_grab)) return()
                       
                       reactive_dataholder[[label]]$filename <- "Example file"
                       reactive_dataholder[[label]]$file <- default_factor(data_grab)
                       
                       appendTab(preview_tabset, 
                                 select = T,
                                 tabPanel(tablabel,
                                          br(),
                                          DTOutput(paste0("DT_", label)),
                                          br()
                                 ),
                                 session = session
                       )
                     } else {
                       reactive_dataholder[[label]]$filename <- NULL
                       reactive_dataholder[[label]]$file <- NULL
                     }
                   })
    
    
    observeEvent(c(input$use_example), {
      use_example_val <- input$use_example || AWS
      toggleState(id = paste0(label, "_clear_file"), 
                  condition = !use_example_val)
    })
  }
  
  ## If MAP, load it up! b-tsk-b-tsk-b-tsk
  observeEvent(minio_upload_data$project_omic, {
    req(!is.null(minio_upload_data$project_omic))
    dtype = minio_upload_data$project_omic$Project$DataType
    
    tablabel <- switch(label,
                       e_data = ifelse(dtype == "RNA-seq", 
                                       "Expression data",
                                       "Abundance data"),
                       f_data = "Sample Information",
                       e_meta = "Biomolecule information"
    )
    
    ## Remove prior
    removeTab(preview_tabset, tablabel, session = session)
    
    ## Shouldn't be needed in MAP or AWS, but for sure for example/upload
    # ## make sure if dt changes remove prior
    # if(tablabel == "Abundance data"){
    #   removeTab(preview_tabset, "Expression data", session = session)
    # } else if(tablabel == "Expression data"){
    #   removeTab(preview_tabset, "Abundance data", session = session)
    # }
    
    tmp_key = paste0(label, "_filename")
    reactive_dataholder[[label]]$filename <- minio_upload_data$project_omic$Data[[tmp_key]]
    
    reactive_dataholder[[label]]$file <- if (isTruthy(minio_upload_data$project_omic$Data[[label]])) {
      default_factor(minio_upload_data$project_omic$Data[[label]])
    } else {
      NULL
    }
    
    select_tab = if(label %in% c("e_data", "f_data")) TRUE else FALSE
    
    appendTab(preview_tabset, 
              select = select_tab,
              tabPanel(tablabel,
                       br(),
                       DTOutput(paste0("DT_", label)),
                       br()
              ),
              session = session
    )
  })
  
  ## If AWS, load it up
  observeEvent(c(AWS, input$data_type, input$data_select, input_data_types()), 
               ignoreInit = FALSE, {
                 
                 req(AWS)
                 req(!is.null(input$data_type))
                 
                 tablabel <- switch(label,
                                    e_data = ifelse(input$data_type == "RNA-seq",
                                                    "Expression data",
                                                    "Abundance data"),
                                    f_data = "Sample Information",
                                    e_meta = "Biomolecule information"
                 )
                 
                 removeTab(preview_tabset, tablabel, session = session)
                 
                 ## Shouldn't be needed in MAP or AWS, but for sure for example/upload
                 # ## make sure if dt changes remove prior
                 # if(tablabel == "Abundance data"){
                 #   removeTab(preview_tabset, "Expression data", session = session)
                 # } else if(tablabel == "Expression data"){
                 #   removeTab(preview_tabset, "Abundance data", session = session)
                 # }
                 
                 use_example_val <- input$use_example
                 data_select_val <- input_data_types()
                 
                 if(label %in% data_select_val){
                   
                   reactive_dataholder[[label]]$filename <- "Loaded from S3 bucket"
                   reactive_dataholder[[label]]$file <- default_factor(AWSobj[[label]])
                   
                   appendTab(preview_tabset,
                             select = T,
                             tabPanel(tablabel,
                                      br(),
                                      DTOutput(paste0("DT_", label)),
                                      br()
                             ),
                             session = session
                   )
                 } else {
                   reactive_dataholder[[label]]$filename <- NULL
                   reactive_dataholder[[label]]$file <- NULL
                 }
               })
  
  ## On clear file, reset holder
  observeEvent(input[[paste0(label, "_clear_file")]], {
    
    # if(label == "f_data") 
    reactive_dataholder[[label]]$filename <- NULL
    reactive_dataholder[[label]]$file <- NULL
    reset("file")
  })
  
  ## Render from uploaded files
  output[[paste0("DT_", label)]] <- renderDT({
    
    reactive_dataholder[[label]]$file
  },
  selection = 'none',
  options = list(dom = 'tpi',
                 pageLength = 10,
                 scrollX = T
                 
  ))
  
})

