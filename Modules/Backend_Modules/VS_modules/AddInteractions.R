

AddInteractionModule <- function() {
  uiOutput("vs_cols_factor_UI")
}
      
file_reactive_add_int <- reactiveValues(
  f_data_file = NULL
)
## Observers reactive val ##

observeEvent(preview_keep_cols$result$f_data_file, {
  val <- preview_keep_cols$result$f_data_file
  file_reactive_add_int$f_data_file <- val
})


## Categorical/factor adjust
output$vs_cols_factor_UI <- renderUI({
  
  req(!is.null(file_reactive_add_int$f_data_file))
    
    div(
    div(
      uiOutput("vs_cols_factor_1"),
      uiOutput("vs_cols_factor_2")
    ),
    
    actionButton("add_int_col", "Add interaction")
    )

})

output$vs_cols_factor_1 <- renderUI({
  
  use_file <- file_reactive_add_int$f_data_file
  class_cols <- purrr::map_chr(1:ncol(use_file), function(n) class(use_file[[n]]))
  
  cats_f_data <- colnames(use_file)[class_cols == "factor"]
  
  disabled <- cats_f_data %in% input$col_2_int
  selected <- isolate(if(is.null(input$col_1_int)) NULL else input$col_1_int)
  
  pickerInput("col_1_int",
              "Specify first column for interaction effect:",
              multiple = T,
              choices = cats_f_data,
              selected = selected,
              choicesOpt = list(disabled = disabled),
              options = list( `live-search` = TRUE, "max-options" = 1)
  )
  
})

output$vs_cols_factor_2 <- renderUI({
  
  use_file <- file_reactive_add_int$f_data_file
  class_cols <- purrr::map_chr(1:ncol(use_file), function(n) class(use_file[[n]]))
  
  cats_f_data <- colnames(use_file)[class_cols == "factor"]
  
  disabled <- cats_f_data %in% input$col_1_int
  selected <- isolate(if(is.null(input$col_2_int)) NULL else input$col_2_int)
  
  pickerInput("col_2_int",
              "Specify first column for interaction effect:",
              multiple = T,
              choices = cats_f_data,
              selected = selected,
              choicesOpt = list(disabled = disabled),
              options = list( `live-search` = TRUE, "max-options" = 1)
  )
  
})

observeEvent(input$add_int_col, {
  req(!is.null(input$col_1_int) && !is.null(input$col_2_int))
  
  new_colname <- paste0(input$col_1_int, "_", input$col_2_int)
  
  file_reactive_add_int$f_data_file[[new_colname]] <- paste(
    file_reactive_add_int$f_data_file[[input$col_1_int]], 
    file_reactive_add_int$f_data_file[[input$col_2_int]], sep = "_")
  
})


## Confer extra columns
observeEvent(c(file_reactive_add_int$f_data_file), {
  req(!is.null(file_reactive_add_int$f_data_file))
  req(input$vscols_cats_done > 0)
  
  df <- file_reactive_add_int$f_data_file
  
  old_cols <- colnames(omicsData$objMSU[["f_data"]])
  
  new_col <- setdiff(colnames(df), old_cols)
  
  omicsData$objMSU[["f_data"]][new_col] <- df[new_col]
  preview_keep_cols$result$f_data_file[new_col] <- df[new_col]
  
  ## Need this? yeahhh
  updatePickerInput(session, "keepcols-f_data_track",
    choices = colnames(omicsData$objMSU[["f_data"]]),
    selected = c(new_col, input[["keepcols-f_data_track"]]))
  
  updatePickerInput(session, "factor_cols-f_data_cats",
                    choices = colnames(preview_keep_cols$result$f_data_file),
                    selected = c(new_col, input[["factor_cols-f_data_cats"]]))
  
})

