
## UI function for upload
FileSpecificationsModal <- function(id) {
  ns <- NS(id)
  
  modalDialog(
    title = "File Specifications",
    size = "xl",
    br(),
    DTOutput(ns("file_DT")),
    br(),
    strong("Expected input: Predictors/Response as columns, instances/observations as rows"),
    br(),
    br(),
    fluidRow(
      column(6,
        uiOutput(ns("factor_col_select_UI"))
      ),
      column(6,
             actionButton(ns("flip_button"), "Flip columns and rows")
      )
      
    ),
    br(),
    textOutput(ns("warning_text")),
    br(),
    footer = tagList(
      # modalButton(ns("Cancel")),
      actionButton(ns("ok"), "OK")
    )
  )
}

FileSpecificationsServer <- function(id, file) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      file_reactive <- reactiveValues(file = NULL)
      warning_reactive <- reactiveValues(factor_warning = NULL)
      
      observeEvent(file(), {
        val <- file()
        file_reactive$file <- val
      })
      
      ## Flip file orientation
      observeEvent(input$flip_button, ignoreInit = T, {
        
        og <- file_reactive$file
        colnames_og <- colnames(og)
        rownames_og <- row.names(og)
        df <- as.data.frame(t(og))
        row.names(df) <- colnames_og
        colnames(df) <- rownames_og
        file_reactive$file <- df
        
      })
      
      ## Selection of factor columns dependent on file orientations
      output$factor_col_select_UI <- renderUI({
        
        req(!is.null(file_reactive$file))
        
        ns <- session$ns
        
        # file_reactive$file <- file()
        use_file <- file_reactive$file
        
        current_selected <- input$factor_col_select
        
        class_cols <- purrr::map_chr(1:ncol(use_file), function(n) class(use_file[[n]]))
        
        selected <- if(is.null(current_selected)){
          colnames(use_file)[class_cols %in% c("factor", "character")]
        } else current_selected
        
        pickerInput(ns("factor_col_select"),
                    "Select all columns that are categorical (i.e., character or factor levels)",
                    choices = colnames(use_file),
                    selected = selected,
                    multiple = T
        )
        
      })
      
      ## Based on factor selection, change designation in file
      observeEvent(input$factor_col_select, {
        
        req(!is.null(file_reactive$file))
        
        use_file <- file_reactive$file
        factors <- input$factor_col_select
        factor_cols <- which(colnames(use_file) %in% factors)
        
        warning_reactive$factor_warning <- NULL
        
        for(i in 1:ncol(use_file)){
          if(i %in% factor_cols){
            use_file[[i]] <- as.factor(use_file[[i]])
          } else {
            tryCatch({
              use_file[[i]] <- as.integer(use_file[[i]])
            }, error = function(e){
              tryCatch({
                use_file[[i]] <- as.numeric(use_file[[i]])
              }, error = function(e){
                warning_reactive$factor_warning <- 
                  c(warning_reactive$factor_warning, colnames(use_file)[[i]])
              })
            })
          }
        }
        
        file_reactive$file <- use_file
      })
      
      ## close modal
      observeEvent(input$ok, removeModal())
      
      ## If anything is specified but ng
      output$warning_text <- renderText({
        
        bad_factors <- warning_reactive$factor_warning
        
        out <- if(length(bad_factors) > 1){
          paste0("The following columns could not be converted to numeric representation:",
                 toString(bad_factors))
        } else NULL
        
        out
      })
      
      ## Display DT with columns color by factor/numeric
      output$file_DT <- renderDT({
        
        req(!is.null(file_reactive$file))
        use_file <- file_reactive$file[1:5,]
        input$factor_col_select
        input$flip
        
        class_cols <- purrr::map_chr(1:ncol(use_file), function(n) class(use_file[[n]]))
        
        dt <- datatable(use_file,
                  selection = 'none',
                  options = list(dom = 't', 
                                 pageLength = 5, 
                                 scrollY = T),
                  fillContainer = TRUE
                  )
        
        dt <- formatStyle(dt, columns = which(class_cols %in% c("factor", "character")), 
                      backgroundColor = "#fcf4d9")
        
        if(nrow(dt$x$data) < 5) browser()
        
        dt
      })
      
      
      return(file_reactive$file)
      
    }
  )
}


## only activate after initialized
# observeEvent(reactive_dataholder, {
#   purrr::map(c("e_data", "f_data", "e_meta"), function(label){
# 
#     modal <- FileSpecificationsModal(paste0(label, "_filespec"))
#     
#     reactive_dataholder[[label]]$file <- FileSpecificationsServer(
#       paste0(label, "_filespec"),
#       reactive(reactive_dataholder[[label]]$file)
#     )
# 
#     observeEvent(reactive_dataholder[[label]]$file, {
#       req(!input$use_example && !is.null(reactive_dataholder[[label]]$file))
#       showModal(modal)
#     }, ignoreInit = T)
# 
#     # ## Render from uploaded files
#     # output[[paste0("DT_", label)]] <- renderDT({
#     #   reactive_dataholder[[label]]$file
#     # })
# 
#   })
# }, once = TRUE)

