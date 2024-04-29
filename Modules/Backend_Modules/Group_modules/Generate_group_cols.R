## Module for adding and generating new group cols 


## Assumes inputs of 

# e_data_file = reactive(reactive_dataholder$e_data$file),
# e_data_cname = reactive(input$e_data_id_col),
# f_data_file = reactive(reactive_dataholder$f_data$file)

## Call in Ui using generate_fdata_UI("id")

## Assumes reactive value reactive_dataholder to populate

####################

## UI function for new group col generation
generate_fdata_UI <- function(id) {
  ns <- NS(id)

  collapseBox(
    "Add Columns to Sample Data", 
    icon_id = "groups-create-columns",
    icon = icon("exclamation-sign", lib = "glyphicon"),
    
    value = "fdata_create_columns",
    radioGroupButtons(ns("how_make_new_columns"), "Create new columns:",
                      choices = c("Manually" = "manually", 
                                  "From existing columns" = "colvalues")
    ),
    hr(),
    textInput(ns("new_column_name"), "Pick a name for this new column"),
    uiOutput(ns("make_fdata_columns")),
    uiOutput(ns("extract_levels")),
    hr(),
    uiOutput(ns("new_column_preview")),
    br(),
    actionButton(ns("add_fmeta_col_submit"), "Add this column"),
    
    # div(style="display:inline-block",actionButton(ns("add_cols_done"), "Done", style="float:right"))
    
    fluidRow(
      column(10, ""),
      column(2, actionButton(ns("add_cols_done"), "Done", style="float:right"))
    )
  )
  
}

## Observer function for loading into RV and updating preview datatable tabs
GenerateFdataServer <- function(id, e_data_file,
                            e_data_cname,
                            f_data_file
                            ) {

  moduleServer(
    id,
    function(input, output, session) {

      
      parentSession <- get("session", envir = parent.frame(2))
      
      file_reactive <- reactiveValues(f_data_file = NULL,
                                      e_data_file = NULL)
      
      warning_reactive <- reactiveValues(bad_regex = NULL)
      
      ## reval observers $$
      
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
      
      # stores the to-be-added values of new columns
      new_fmeta_column_values <- reactive({
        req(input$how_make_new_columns, file_reactive$f_data_file)
        # manual returns the result of ';'-splitting the text input
        if (input$how_make_new_columns == "manually") {
          req(input$group_values)
          splitvals <- str_split(input$group_values, ";")[[1]]
          length(splitvals) <- nrow(file_reactive$f_data_file)
          return(splitvals)
        }
        # from column values either...
        else if (input$how_make_new_columns == "colvalues") {
          req(input$groups_from_column)
          
          if (input$how_extract_levels == "split") {
            req(input$fmeta_col_delimiter)
            column_values <- as.character(file_reactive$f_data_file[[input$groups_from_column]])
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
            column_values <- as.character(file_reactive$f_data_file[[input$groups_from_column]])
            extracted_values <- str_extract(column_values, input$fmeta_col_regex)
            return(extracted_values)
          }
        }
        
        return(NULL)
      })

      ## UI excitement ##
      output$make_fdata_columns <- renderUI({
        if (input$how_make_new_columns == "manually") {
          textInput(session$ns("group_values"), "Enter values separated by ';'")
        }
        else if (input$how_make_new_columns == "colvalues") {
          req(file_reactive$f_data_file)
          tagList(
            pickerInput(session$ns("groups_from_column"), "Extract group info from column:",
                        choices = colnames(file_reactive$f_data_file)),
            radioGroupButtons(session$ns("how_extract_levels"), "Extract groups by:",
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
            textInput(session$ns("fmeta_col_delimiter"), "Split values by:"),
            uiOutput(session$ns("split_position_picker"))
          )
        }
        else if (input$how_extract_levels == "regex") {
          textInput(session$ns("fmeta_col_regex"), "Extract values by regex string:")
        }
      })

      # conditional picker depending on what column and delimiter is used
      output$split_position_picker <- renderUI({
        req(!is.null(input$groups_from_column) &&  input$fmeta_col_delimiter != "")
        column_values <- as.character(file_reactive$f_data_file[[input$groups_from_column]])

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
            pickerInput(session$ns("groups_split_keep_which"), "Keep value in which positions",
                        choices = 1:truncate_length, multiple = TRUE,
                        choicesOpt = list(subtext = subtext)
            )
          )
        } else {
          add_prompt(
            disabled(pickerInput(session$ns("groups_split_keep_which"), "Keep value in which positions",
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
        req(file_reactive$f_data_file, new_fmeta_column_values())
        
        if (input$new_column_name %in% colnames(file_reactive$f_data_file)) {
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

        file_reactive$f_data_file[[input$new_column_name]] <- new_fmeta_column_values()
      })
      
      observeEvent(input$add_cols_done, {
        updateBoxCollapse(session = parentSession, 
                          id = "groups_collapse_left",
                          close = "fdata_create_columns")
      })

      return(file_reactive)
    }
  )
}

## set to update reactive_dataholder
updated_f_data <- reactiveValues(result = NULL)

observeEvent(reactive_dataholder, {
  req(!is.null(reactive_dataholder))

  updated_f_data$result <- GenerateFdataServer(
    id = "f_data",
    e_data_file = reactive(reactive_dataholder$e_data$file),
    e_data_cname = reactive(input$e_data_id_col),
    f_data_file = reactive(reactive_dataholder$f_data$file)
  )

}, once = T)

observeEvent(updated_f_data$result$f_data_file, {
  req(!is.null(updated_f_data$result$f_data_file))
  reactive_dataholder[["f_data"]]$file <- updated_f_data$result$f_data_file
})



