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

## Accessible UI via:
# fileinput_UI("e_data_upload", "e_data", is_RNA = F)
# (Basically, adding these to your UI page or a renderUI will generate appropriate UI)

## Please add a static tabset to FileInputServer for previewing data as well!
preview_tabset <- "preview_data"

## Accessible resulting files via:
## reactive_dataholder

## Notes for use in future apps:

## Use or change name of reactive_dataholder for storing files before creating omics object
## Likewise for input$use_example for example data, input$data_type for specifying what kind of omics, and input$data_select for what data files are available
## data_select may need adjusting depending on what files are required for your application
## The name of the preview tabset is dependent on static UI and may need changing, currently set to "preview_data"
## Note: File handling is the only true module, where its own namespace is created. 
### This is due to the requirement of multiple UI pieces that rely on the result of this module, but don't do calculations

## Data holder
reactive_dataholder <- reactiveValues(e_data = NULL,
                                      f_data = NULL,
                                      e_meta = NULL)

## Module Functions ##

## UI function for upload
fileinput_UI <- function(id, label = "e_data", is_RNA) {
  ns <- NS(id)
  
  tablabel <- switch(label,
                     e_data = ifelse(is_RNA, 
                                     "Expression data .csv",
                                     "Abundance data .csv"),
                     f_data = "Sample information .csv",
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
                          preview_tabset) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # parent session (for tabset appending)
      parentSession <- get("session", envir = parent.frame(2))
      
      store_data <- reactiveValues(filename = NULL, file = NULL)
      # use_example <- reactive(use_example())
      # data_type <- reactive(data_type())
      
      ## If a file is uploaded, load it up
      observeEvent(input$file, ignoreInit = TRUE, {
        
        req(!is.null(input$file$name))
        
        tablabel <- switch(label,
                           e_data = ifelse(data_type() == "RNA-seq", 
                                           "Expression data",
                                           "Abundance data"),
                           f_data = "Sample information",
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
          store_data$file <- read.csv(input$file$datapath)
          
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
          
          # removeTab(preview_tabset, tablabel, session = parentSession)
        }
        
      })
      
      ## If example files are used, load it up
      observeEvent(c(use_example(), data_type(), data_select()), ignoreInit = TRUE, {
        
        tablabel <- switch(label,
                           e_data = ifelse(data_type() == "RNA-seq", 
                                           "Expression data",
                                           "Abundance data"),
                           f_data = "Sample information",
                           e_meta = "Biomolecule information"
        )
        
        removeTab(preview_tabset, tablabel, session = parentSession)
        
        use_example_val <- use_example()
        data_type_val <- data_type()
        data_select_val <- c("e_data", data_select())
        
        req(use_example_val && !is.null(data_type_val))
        
        if(label %in% data_select_val){
          pmartRdata_prefix <- switch(data_type_val,
                                      "Protein" = "pro_", 
                                      "Label-free" = "pep_", 
                                      "Isobaric" = "isobaric_", 
                                      "Negative" = "lipid_neg_", 
                                      "Positive" = "lipid_pos_", 
                                      "NMR" = "nmr_", 
                                      "GC-MS" = "metab_", 
                                      "RNA-seq" = "rnaseq_"
          )
          
          data_grab <- get(paste0(pmartRdata_prefix, sub("_", "", label)))
          
          store_data$filename <- "Example file"
          store_data$file <- data_grab
          
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
          
          # removeTab(preview_tabset, tablabel, session = parentSession)
        }
        
      })
      
      ## Disable clear file for example data
      observeEvent(use_example(), {
        use_example_val <- use_example()
        toggleState(id = "clear_file", condition = !use_example_val)
      })
      
      ## On clear file, reset holder
      observeEvent(input$clear_file, {
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
  
  pt <- if(label == "f_data") paste0(preview_tabset, "_f_data") else preview_tabset
  
  reactive_dataholder[[label]] <- FileInputServer(paste0(label, "_upload"), 
                                                reactive(input$use_example),
                                                reactive(input$data_type),
                                                reactive(input$data_select),
                                                label = label,
                                                preview_tabset = preview_tabset
  )
  
  ## Render from uploaded files
  output[[paste0("DT_", label)]] <- renderDT({
    
    reactive_dataholder[[label]]$file
  },

  options = list(dom = 't', 
    pageLength = 10, 
    scrollY = T
    )#, fillContainer = TRUE ### Fix for headers not scrolling in tabsets, but generates unstable loading of data (no rows appear)
  )
  
  outputOptions(output, paste0("DT_", label), suspendWhenHidden = F)
  
})

