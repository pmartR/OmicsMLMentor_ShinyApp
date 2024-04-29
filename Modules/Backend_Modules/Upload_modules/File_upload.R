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

