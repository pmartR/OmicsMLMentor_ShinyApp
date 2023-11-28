

FactorColsModule <- function(id) {
  ns <- NS(id)
  uiOutput(ns("picker_factor"))
}

## Observer function for loading into RV and updating preview datatable tabs
FactorColsServer <- function(id,
                           f_data_file,
                           f_data_cname,
                           e_data_cname,
                           e_data_file,
                           e_meta_cname,
                           e_meta_file,
                           data_type,
                           vscols_options_done
) {
  moduleServer(
    id,
    function(input, output, session) {
      
      file_reactive <- reactiveValues(
        e_data_file = NULL,
        f_data_file = NULL,
        e_meta_file = NULL,
        f_data_cname = NULL,
        e_data_cname = NULL,
        e_meta_cname = NULL
      )
      
      warning_reactive <- reactiveValues(bad_regex = NULL)
      
      # parent session (for tabset appending)
      parentSession <- get("session", envir = parent.frame(2))
      
      ## Observers reactive val ##
      
      observeEvent(e_data_file(), {
        val <- e_data_file()
        file_reactive$e_data_file <- val
      })
      
      observeEvent(e_data_cname(), {
        val <- e_data_cname()
        file_reactive$e_data_cname <- val
      })
      
      observeEvent(f_data_file(), {
        val <- f_data_file()
        file_reactive$f_data_file <- val
      })
      
      observeEvent(f_data_cname(), {
        val <- f_data_cname()
        file_reactive$f_data_cname <- val
      })
      
      observeEvent(e_meta_file(), {
        val <- e_meta_file()
        file_reactive$e_meta_file <- val
      })
      
      observeEvent(e_meta_cname(), {
        val <- e_meta_cname()
        file_reactive$e_meta_cname <- val
      })
      
      ## Categorical/factor adjust
      output$picker_factor <- renderUI({

        req((!is.null(file_reactive$f_data_cname) || !
               is.null(file_reactive$e_meta_cname)) &&
              vscols_options_done() > 0)
        
        cols_f_data <- colnames(file_reactive$f_data_file)
        cols_e_meta <- colnames(file_reactive$e_meta_file)
        
        pickers <- map2(
          list(cols_f_data, cols_e_meta),
          c("f_data", "e_meta"),
          function(x, lab){
            if(length(x) != 0){
              dt <- data_type()
              
              tablabel <- switch(lab,
                                 e_data = ifelse(dt == "RNA-seq",
                                                 "Expression data",
                                                 "Abundance data"),
                                 f_data = "Sample data",
                                 e_meta = "Biomolecule information"
              )
              
              df <- file_reactive[[paste0(lab, "_file")]][x]
              
              factor_cols <- apply(df, 2, is.factor) ## factor overrides numeric
              cat_cols <- apply(is.na(apply(df, 2, as.numeric)), 2, all)
              id_col <- colnames(df) == file_reactive[[paste0(lab, "_cname")]]
              
              
              if(is.null(isolate(input[[paste0(lab, "_cats")]]))){
                selected <- colnames(df)[Reduce("|", list(factor_cols, cat_cols, id_col))]
              } else selected <- c(colnames(df)[id_col], isolate(input[[paste0(lab, "_cats")]]))
              
              pickerInput(session$ns(paste0(lab, "_cats")),
                          paste0("Select/deselect all categorical columns in ", tablabel),
                          multiple = T,
                          choices = x,
                          selected = selected,
                          options = list( `live-search` = TRUE, `actions-box` = TRUE),
                          choicesOpt = list(disabled = x == colnames(df)[id_col])
                          )
              }
            })
      
          
          ## categorical column picker UI based on omicsData$obj and vs_cols_select_UI
          div(
            pickers
          )
      })
      
      
      map(c("e_meta", "f_data"), function(lab){
        observeEvent(input[[paste0(lab, "_cats")]], {
          
          use_file <- file_reactive[[paste0(lab, "_file")]]
          factors <- input[[paste0(lab, "_cats")]]
          
          factor_cols <- which(colnames(use_file) %in% c(
            file_reactive[[paste0(lab, "_cname")]], factors))
          
          factor_warning <- c()
          
          for(i in 1:ncol(use_file)){
            to_string <- as.character(use_file[[i]])
            if(i %in% factor_cols){
              use_file[[i]] <- as.factor(to_string)
            } else {
                if(!all(is.na(as.numeric(to_string)))){
                  use_file[[i]] <- as.numeric(to_string)
                } else factor_warning <- c(factor_warning, colnames(use_file)[[i]])
            }
          }
          
          if(length(factor_warning) > 0){
            shinyalert("Cannot convert value", 
                       paste0("Column(s) ", toString(factor_warning), 
                              " cannot be converted to numeric values and will", 
                              " remain marked as categorical.")
                       )
            
            updatePickerInput(session, paste0(lab, "_cats"), 
                              selected = unique(
                                c(file_reactive[[paste0(lab, "_cname")]], 
                                  factors, factor_warning)))
          }
          
          file_reactive[[paste0(lab, "_file")]] <- use_file
          
        })
      })
      
      return(file_reactive)
      
    })
}

factor_cols <- reactiveValues(result = NULL)

observeEvent(omicsData$objMSU, {
  req(!is.null(omicsData$objMSU))
  
  factor_cols$result <- FactorColsServer(
    id = "factor_cols",
    e_data_file = reactive(preview_keep_cols$result$e_data_file),
    f_data_file = reactive(preview_keep_cols$result$f_data_file),
    e_meta_file = reactive(preview_keep_cols$result$e_meta_file),
    f_data_cname = reactive(input$f_data_id_col),
    e_data_cname = reactive(input$e_data_id_col),
    e_meta_cname = reactive(input$e_meta_id_col),
    data_type = reactive(input$data_type),
    vscols_options_done = reactive(input$vscols_options_done)
  )
  
}, once = T)


## Confer factors
observeEvent(c(factor_cols$result$f_data_file), {
  req(!is.null(factor_cols$result$f_data_file))

  df <- factor_cols$result$f_data_file

  for(col in colnames(df)){
    omicsData$objMSU[["f_data"]][col] <- df[col]
    preview_keep_cols$result$f_data_file[col] <- df[col]
  }
})

observeEvent(c(factor_cols$result$e_meta_file), {
  req(!is.null(factor_cols$result$e_meta_file))
  
  df <- factor_cols$result$e_meta_file
  
  for(col in colnames(df)){
    omicsData$objMSU[["e_meta"]][col] <- df[col]
    preview_keep_cols$result$e_meta_file[col] <- df[col]
  }
})


