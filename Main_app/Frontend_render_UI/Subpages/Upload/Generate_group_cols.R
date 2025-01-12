## Module for adding and generating new group cols 

# e_data_file = reactive(reactive_dataholder$e_data$file),
# e_data_cname = reactive(input$e_data_id_col),
# f_data_file = reactive(reactive_dataholder$f_data$file)

## Call in Ui using generate_fdata_UI("id")

## Assumes reactive value reactive_dataholder to populate

####################

## UI function for new group col generation
generate_fdata_UI <- function(id) {
  
  collapseBox(
    "Add Columns to Sample Information", 
    icon_id = "groups-create-columns",
    icon = icon("exclamation-sign", lib = "glyphicon"),
    
    value = "fdata_create_columns",
    radioGroupButtons("how_make_new_columns", "Create new columns:",
                      choices = c("Manually" = "manually", 
                                  "From existing columns" = "colvalues")
    ),
    hr(),
    textInput("new_column_name", "Pick a name for this new column"),
    uiOutput("make_fdata_columns"),
    uiOutput("extract_levels"),
    hr(),
    uiOutput("new_column_preview"),
    br(),
    actionButton("add_fmeta_col_submit", "Add this column"),
    
    fluidRow(
      column(10, ""),
      column(2, actionButton("add_cols_done", "Done", style="float:right"))
    )
  )
  
}

      
updated_f_data <- reactiveValues(f_data_file = NULL,
                                e_data_file = NULL)

## reval observers $$

observeEvent(reactive_dataholder$e_data$file, {
  val <- reactive_dataholder$e_data$file
  updated_f_data$e_data_file <- val
})

observeEvent(input$e_data_id_col, {
  val <- input$e_data_id_col
  updated_f_data$e_data_cname <- val
})

observeEvent(reactive_dataholder$f_data$file, {
  val <- reactive_dataholder$f_data$file
  updated_f_data$f_data_file <- val
})

# stores the to-be-added values of new columns
new_fmeta_column_values <- reactive({
  req(input$how_make_new_columns, updated_f_data$f_data_file)
  # manual returns the result of ';'-splitting the text input
  if (input$how_make_new_columns == "manually") {
    req(input$group_values)
    splitvals <- str_split(input$group_values, ";")[[1]]
    length(splitvals) <- nrow(updated_f_data$f_data_file)
    return(splitvals)
  }
  # from column values either...
  else if (input$how_make_new_columns == "colvalues") {
    req(input$groups_from_column)
    
    if (input$how_extract_levels == "split") {
      req(input$fmeta_col_delimiter)
      column_values <- as.character(updated_f_data$f_data_file[[input$groups_from_column]])
      split_result <- str_split(column_values, input$fmeta_col_delimiter)
      
      keep_which <- sapply(split_result, function(x) {
        if (!isTruthy(x)) {
          return(NA)
        }
        paste(x[as.numeric(input$groups_split_keep_which)],
              collapse = input$fmeta_col_delimiter
        )
      })
      return(keep_which)
    }
    # ... extracts the text corresponding to a regex input
    else if (input$how_extract_levels == "regex") {
      req(input$fmeta_col_regex)
      column_values <- as.character(updated_f_data$f_data_file[[input$groups_from_column]])
      extracted_values <- str_extract(column_values, input$fmeta_col_regex)
      return(extracted_values)
    }
  }
  
  return(NULL)
})

## UI excitement ##
output$make_fdata_columns <- renderUI({
  if (input$how_make_new_columns == "manually") {
    textInput("group_values", "Enter values separated by ';'")
  }
  else if (input$how_make_new_columns == "colvalues") {
    req(updated_f_data$f_data_file)
    tagList(
      pickerInput("groups_from_column", "Extract group info from column:",
                  choices = colnames(updated_f_data$f_data_file)),
      radioGroupButtons("how_extract_levels", "Extract groups by:",
                        choices = c("Delimiter split" = "split", "Regex search" = "regex")
      )
    )
  }
})

# making new column values by regex or delimiter
output$extract_levels <- renderUI({
  
  req(input$how_make_new_columns == "colvalues", input$how_extract_levels)
  if (input$how_extract_levels == "split") {
    tagList(
      textInput("fmeta_col_delimiter", "Split values by:"),
      uiOutput("split_position_picker")
    )
  }
  else if (input$how_extract_levels == "regex") {
    textInput("fmeta_col_regex", "Extract values by regex string:")
  }
})

# conditional picker depending on what column and delimiter is used
output$split_position_picker <- renderUI({
  req(!is.null(input$groups_from_column) &&  input$fmeta_col_delimiter != "")
  column_values <- as.character(updated_f_data$f_data_file[[input$groups_from_column]])
  
  split_result <- str_split(column_values, input$fmeta_col_delimiter)
  truncate_length <- max(sapply(split_result, length))
  for (i in which(map_int(split_result, length) < truncate_length)) {
    split_result[[i]] <- c(split_result[[i]], rep("NA", truncate_length - length(split_result[[i]])))
  }
  
  # let them split up to the maximum number of split elements.
  # If a value is selected that exceeds the length of one of the split results, that result will become NA in the new column
  subtext <- map_chr(1:truncate_length, function(n) toString(unique(map_chr(split_result, n))))
  
  if (isTruthy(input$fmeta_col_delimiter)) {
    return(
      pickerInput("groups_split_keep_which", "Keep value in which positions",
                  choices = 1:truncate_length, multiple = TRUE,
                  choicesOpt = list(subtext = subtext)
      )
    )
  } else {
    add_prompt(
      disabled(pickerInput("groups_split_keep_which", "Keep value in which positions",
                           choices = 1:truncate_length, multiple = TRUE
      )),
      message = paste0("In the field above this one, pick a value that",
                       " each entry in the chosen column will be split",
                       " by, then choose which parts of the result to keep")
    )
  }
})

output$new_column_preview <- renderUI({
  input$how_make_new_columns
  div(
    tags$b("New column values preview:"),
    paste(new_fmeta_column_values(), collapse = " | ")
  )
})


## Observers ##

# add new column on button click
observeEvent(input$add_fmeta_col_submit, {
  req(updated_f_data$f_data_file, new_fmeta_column_values())
  
  if (input$new_column_name %in% colnames(updated_f_data$f_data_file)) {
    popup$status <- "Fdata new column"
    
    shinyalert(
      text = paste0("Something went wrong: This column already exists, please rename new column."),
      type = "error",
      callbackR = function(val) {
        popup$status <- "None"
        return(NULL)
      }
    )
    return()
  }
  
  updated_f_data$f_data_file[[input$new_column_name]] <- new_fmeta_column_values()
})

observeEvent(input$add_cols_done, {
  updateBoxCollapse(session = session, 
                    id = "groups_collapse_left",
                    close = "fdata_create_columns")
})

observeEvent(updated_f_data$f_data_file, {
  req(!is.null(updated_f_data$f_data_file))
  reactive_dataholder[["f_data"]]$file <- updated_f_data$f_data_file
})



