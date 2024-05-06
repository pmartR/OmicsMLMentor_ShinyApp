

FactorColsModule <- function() {
  uiOutput("picker_factor")
}
      
      factor_cols <- reactiveValues(
        e_data_file = NULL,
        f_data_file = NULL,
        e_meta_file = NULL,
        f_data_cname = NULL,
        e_data_cname = NULL,
        e_meta_cname = NULL
      )
      
      ## Observers reactive val ##
      
      observeEvent(preview_keep_cols$result$e_data_file, {
        val <- preview_keep_cols$result$e_data_file
        factor_cols$e_data_file <- val
      })
      
      observeEvent(input$e_data_id_col, {
        val <- input$e_data_id_col
        factor_cols$e_data_cname <- val
      })
      
      observeEvent(preview_keep_cols$result$f_data_file, {
        val <- preview_keep_cols$result$f_data_file
        factor_cols$f_data_file <- val
      })
      
      observeEvent(input$f_data_id_col, {
        val <- input$f_data_id_col
        factor_cols$f_data_cname <- val
      })
      
      observeEvent(preview_keep_cols$result$e_meta_file, {
        val <- preview_keep_cols$result$e_meta_file
        factor_cols$e_meta_file <- val
      })
      
      observeEvent(input$e_meta_id_col, {
        val <- input$e_meta_id_col
        factor_cols$e_meta_cname <- val
      })
      
      ## Categorical/factor adjust
      output$picker_factor <- renderUI({

        req((!is.null(factor_cols$f_data_cname) || 
               !is.null(factor_cols$e_meta_cname)) &&
              input$vscols_options_done > 0)
        
        cols_f_data <- colnames(factor_cols$f_data_file)
        cols_e_meta <- colnames(factor_cols$e_meta_file)
        
        list_cols <- list(cols_f_data, cols_e_meta)
        
        pickers <- map2(
          list_cols[!map_lgl(list_cols, is.null)],
          c("f_data", "e_meta")[!map_lgl(list_cols, is.null)],
          function(x, lab){
            if(length(x) != 0){
              dt <- input$data_type
              
              tablabel <- switch(lab,
                                 e_data = ifelse(dt == "RNA-seq",
                                                 "Expression data",
                                                 "Abundance data"),
                                 f_data = "Sample Information",
                                 e_meta = "Biomolecule information"
              )
              
              req(!is.null(factor_cols[[paste0(lab, "_file")]]))
              
              df <- factor_cols[[paste0(lab, "_file")]][x]
              
              fctr_cols <- apply(df, 2, is.factor) ## factor overrides numeric
              cat_cols <- apply(is.na(apply(df, 2, as.numeric)), 2, all)
              id_col <- colnames(df) == factor_cols[[paste0(lab, "_cname")]]
              
              
              if(is.null(isolate(input[[paste0(lab, "_cats")]]))){
                selected <- colnames(df)[Reduce("|", list(fctr_cols, cat_cols, id_col))]
              } else selected <- c(colnames(df)[id_col], isolate(input[[paste0(lab, "_cats")]]))
              
              pickerInput((paste0(lab, "_cats")),
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
          
          use_file <- factor_cols[[paste0(lab, "_file")]]
          factors <- input[[paste0(lab, "_cats")]]
          
          fctr_cols <- which(colnames(use_file) %in% c(
            factor_cols[[paste0(lab, "_cname")]], factors))
          
          factor_warning <- c()
          
          for(i in 1:ncol(use_file)){
            to_string <- as.character(use_file[[i]])
            if(i %in% fctr_cols){
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
                                c(factor_cols[[paste0(lab, "_cname")]], 
                                  factors, factor_warning)))
          }
          
          factor_cols[[paste0(lab, "_file")]] <- use_file
          
        })
      })

# observeEvent(omicsData$objMSU, {
#   req(!is.null(omicsData$objMSU))
#   
#   factor_cols <- FactorColsServer(
#     id = "factor_cols",
#     e_data_file = reactive(preview_keep_cols$result$e_data_file),
#     f_data_file = reactive(preview_keep_cols$result$f_data_file),
#     e_meta_file = reactive(preview_keep_cols$result$e_meta_file),
#     f_data_cname = reactive(input$f_data_id_col),
#     e_data_cname = reactive(input$e_data_id_col),
#     e_meta_cname = reactive(input$e_meta_id_col),
#     data_type = reactive(input$data_type),
#     vscols_options_done = reactive(input$vscols_options_done)
#   )
#   
# }, once = T)


## Confer factors
observeEvent(c(factor_cols$f_data_file), {
  req(!is.null(factor_cols$f_data_file))

  df <- factor_cols$f_data_file

  for(col in colnames(df)){
    omicsData$objMSU[["f_data"]][col] <- df[col]
    preview_keep_cols$result$f_data_file[col] <- df[col]
  }
})

observeEvent(c(factor_cols$e_meta_file), {
  req(!is.null(factor_cols$e_meta_file))
  
  df <- factor_cols$e_meta_file
  
  for(col in colnames(df)){
    omicsData$objMSU[["e_meta"]][col] <- df[col]
    preview_keep_cols$result$e_meta_file[col] <- df[col]
  }
})


