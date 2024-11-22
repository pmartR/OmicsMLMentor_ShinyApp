# File upload module

# Includes:
## Uploading files
## Loading example files
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
## Likewise for input$use_example for example data, reactive_dataholder$model$datatype for specifying what kind of omics, and input$data_select for what data files are available
## data_select may need adjusting depending on what files are required for your application
## The name of the preview tabset is dependent on static UI and may need changing, currently set to "preview_data"
## Note: File handling is the only true module, where its own namespace is created. 
### This is due to the requirement of multiple UI pieces that rely on the result of this module, but don't do calculations

## Module Functions ##

## UI function for upload
fileinput_UI <- function(id, label = "e_data", is_RNA) {
  
  tablabel <- switch(label,
                     e_data = ifelse(is_RNA, 
                                     "Expression data .csv",
                                     "Abundance data .csv"),
                     f_data = "Sample Information .csv",
                     e_meta = "Biomolecule information .csv",
                     model = "Model .RDS"
  )
  
  tagList(
    # wellPanel(
    splitLayout(cellWidths = c("75%", "25%"),
                fileInput(paste0(label, "_file"), label = tablabel),
                div(br(), actionButton(paste0(label, "_clear_file"), 
                                       label = "Clear file"))
    )
    # )
  )
}


output[["model_upload_UI"]] <- renderUI({
  
  fileinput_UI("model_upload", "model", 
               is_RNA = F ## Doesn't matter yet
               )
})

output[["e_data_upload_UI"]] <- renderUI({
  
  dt <- class(reactive_dataholder$model$model$norm_omics)
  
  fileinput_UI("e_data_upload", "e_data", is_RNA = "RNA-seq" %in% dt)
})

output[["f_data_upload_UI"]] <- renderUI({
  
  req(!is.null(input$use_fdata) &&
        input$use_fdata == "Yes"
      )
  
  dt <- class(reactive_dataholder$model$model$norm_omics)
  
  fileinput_UI("f_data_upload", "f_data", is_RNA = dt == "RNA-seq")
})

output[["e_meta_upload_UI"]] <- renderUI({
  
  dt <- class(reactive_dataholder$model$model$norm_omics)
  
  fileinput_UI("e_meta_upload", "e_meta", is_RNA = dt == "RNA-seq")
})


input_data_types <- reactive({
  
  # e_data
  all_input_data_types <- c("e_data", "model")
  
  # f_data
  all_input_data_types <- c(
    all_input_data_types,
    if (!is.null(input$use_fdata) &&
        input$use_fdata == "Yes") 
      "f_data" 
    else 
      NULL
  )
  
  # e_meta
  all_input_data_types <- c(
    all_input_data_types,
    if (isTruthy(input$have_emeta)) 
      "e_meta" 
    else 
      NULL
  )
  
  all_input_data_types
})



purrr::map(c("e_data", "f_data", "e_meta", "model"), function(label){
  
  preview_tabset <- "preview_data"
  
  ## If a file is uploaded, load it up
  observeEvent(input[[paste0(label, "_file")]], ignoreInit = TRUE, {
    
    
    req(label %in% input_data_types())
    req(!is.null(input[[paste0(label, "_file")]]$name))
    
    dt <- class(reactive_dataholder$model$model$norm_omics)
    
    tablabel <- switch(label,
                       e_data = ifelse(
                         dt == "seqData", 
                                       "Expression data",
                                       "Abundance data"),
                       f_data = "Sample Information",
                       e_meta = "Biomolecule information",
                       model = "Predictive model"
    )
    
    ## Remove prior
    removeTab(preview_tabset, tablabel, session = session)
    
    ## make sure if dt changes remove prior
    if(tablabel == "Abundance data"){
      removeTab(preview_tabset, "Expression data", session = session)
    } else if(tablabel == "Expression data"){
      removeTab(preview_tabset, "Abundance data", session = session)
    }  else if(tablabel == "Predictive model"){
      removeTab(preview_tabset, "Predictive model", session = session)
    }
    
    data_select_val <- input_data_types()
    if(label %in% data_select_val){
      
      reactive_dataholder[[label]]$filename <- input[[paste0(label, "_file")]]$name
      
      if(label == "model"){
        
        tryCatch({
          
          reactive_dataholder[[label]]$model <- readRDS(
            input[[paste0(label, "_file")]]$datapath)
        
          model <- reactive_dataholder[[label]]$model$model
          
        responses <- unique(model$pre$mold$outcomes[[1]])
        og_train_size <- model$pre$mold$blueprint$recipe$tr_info[[1]]
        n_predictors <- ncol(model$pre$mold$blueprint$ptypes$predictors)
        
        
        hyperparams <- model$fit$actions$model$spec$args
        # this originally was map_int but issues when not dealing with integers
        hyperparams <- map_dbl(hyperparams, rlang::quo_get_expr)
        hp_df <- as.data.frame(hyperparams)
        colnames(hp_df) <- "Metrics"
        row.names(hp_df) <- paste0("Hyperparameter ", row.names(hp_df))
        
        # update this once we know if this is the problem or not
        performance <- as.data.frame(model$fit$fit$fit$confusion)
        per_df <- signif(performance[ncol(performance)])
        colnames(per_df) <- "Metrics"
        row.names(per_df) <- paste0(row.names(per_df), " class error")
        #per_df <- NULL
        
        df <- data.frame(
          "Metrics" = c(
            "Protein", ####### TEMP
            toString(responses),
            og_train_size,
            n_predictors
          ) )
        
        row.names(df) <- c(
          "Datatype scope",
          "Predicted outcomes", 
          "Number of samples trained on",
          "Number of predictors used")
        
        
        
        reactive_dataholder[[label]]$file <- rbind(df, hp_df, per_df)
        
        }, error = function(e){
          shinyalert(title = "Something went wrong!", text = e$message)
        })
        
        
      } else {
        tryCatch({
          reactive_dataholder[[label]]$file <- default_factor(
            read.csv(input[[paste0(label, "_file")]]$datapath))
        }, error = function(e){
          shinyalert(title = "Something went wrong!", text = e$message)
        })
      }
      
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
  
  ## If AWS, load it up
  observeEvent(c(AWS, 
                 reactive_dataholder$model$model, 
                 input$data_select, 
                 input_data_types()
                 ), 
               ignoreInit = FALSE, {
                 
                 req(AWS)
                 req(!is.null(reactive_dataholder$model$model))
                 
                 dt <- class(reactive_dataholder$model$model$norm_omics)
                 
                 tablabel <- switch(label,
                                    e_data = ifelse(dt == "seqData",
                                                    "Expression data",
                                                    "Abundance data"),
                                    f_data = "Sample Information",
                                    e_meta = "Biomolecule information",
                                    model = "Predictive model (RDS file)"
                 )
                 
                 removeTab(preview_tabset, tablabel, session = session)
                 
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
    reset(paste0(label, "_file"))
    
    tablabel <- switch(label,
                       e_data = ifelse(
                         reactive_dataholder$model$datatype == "RNA-seq", 
                                       "Expression data",
                                       "Abundance data"),
                       f_data = "Sample Information",
                       e_meta = "Biomolecule information",
                       model = "Predictive model (RDS file)"
    )
    
    ## Remove prior
    removeTab(preview_tabset, tablabel, session = session)
    
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