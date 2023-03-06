## Module for adding and generating new group cols 

## UI function for new group col generation
generate_fdata_UI <- function(id) {
  ns <- NS(id)

  collapseBox(
    "Create grouping columns", 
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
    appButton(inputId = ns("add_fmeta_col_submit"), label = "Add this column")
  )
  
}

## Observer function for loading into RV and updating preview datatable tabs
# GenerateFdataServer <- function(id, e_data_file, 
#                             e_data_cname, 
#                             f_data_file
#                             ) {
# 
#   moduleServer(
#     id,
#     function(input, output, session) {
#       
#       
#       browser()
#       
#       file_reactive <- reactiveValues(f_data_file = NULL,
#                                       e_data_file= NULL)
#       warning_reactive <- reactiveValues(bad_regex = NULL)
#       
#       observeEvent(e_data_file(), {
#         val <- e_data_file()
#         file_reactive$e_data_file <- val
#       })
#       
#       observeEvent(e_data_cname(), {
#         val <- e_data_cname()
#         file_reactive$e_data_cname <- val
#       })
#       
#       observeEvent(f_data_file(), {
#         val <- f_data_file()
#         file_reactive$f_data_file <- val
#       })
#       
#       ## UI excitement ##
#       output$make_fdata_columns <- renderUI({
#         if (input$how_make_new_columns == "manually") {
#           textInput("group_values", "Enter values separated by ';'")
#         }
#         else if (input$how_make_new_columns == "colvalues") {
#           req(file_reactive$f_data_file)
#           tagList(
#             pickerInput("groups_from_column", "Extract group info from column:", 
#                         choices = colnames(file_reactive$f_data_file)),
#             radioGroupButtons("how_extract_levels", "Extract groups by:",
#                               choices = c("Delimiter split" = "split", "Regex search" = "regex")
#             )
#           )
#         }
#       })
#       
#       # making new column values by regex or delimiter
#       output$extract_levels <- renderUI({
#         req(input$how_make_new_columns == "colvalues", input$how_extract_levels)
#         if (input$how_extract_levels == "split") {
#           tagList(
#             textInput("fmeta_col_delimiter", "Split values by:"),
#             uiOutput("split_position_picker")
#           )
#         }
#         else if (input$how_extract_levels == "regex") {
#           textInput("fmeta_col_regex", "Extract values by regex string:")
#         }
#       })
#       
#       # conditional picker depending on what column and delimiter is used
#       output$split_position_picker <- renderUI({
#         req(!is.null(input$groups_from_column) &&  input$fmeta_col_delimiter != "")
#         column_values <- as.character(file_reactive$f_data_file)
#         
#         split_result <- str_split(column_values, input$fmeta_col_delimiter)
#         truncate_length <- max(sapply(split_result, length))
#         for (i in which(map_int(split_result, length) < truncate_length)) {
#           split_result[[i]] <- c(split_result[[i]], rep("NA", truncate_length - length(split_result[[i]])))
#         }
#         
#         # let them split up to the maximum number of split elements.
#         # If a value is selected that exceeds the length of one of the split results, that result will become NA in the new column
#         subtext <- map_chr(1:truncate_length, function(n) toString(unique(map_chr(split_result, n))))
#         
#         if (isTruthy(input$fmeta_col_delimiter)) {
#           return(
#             pickerInput("groups_split_keep_which", "Keep value in which positions",
#                         choices = 1:truncate_length, multiple = TRUE,
#                         choicesOpt = list(subtext = subtext)
#             )
#           )
#         } else {
#           add_prompt(
#             disabled(pickerInput("groups_split_keep_which", "Keep value in which positions",
#                                  choices = 1:truncate_length, multiple = TRUE
#             )),
#             message = paste0("In the field above this one, pick a value that",
#                              " each entry in the chosen column will be split",
#                              " by, then choose which parts of the result to keep")
#           )
#         }
#       })
#       
#       output$new_column_preview <- renderUI({
#         input$how_make_new_columns
#         div(
#           tags$b("New column values preview:"),
#           paste(new_fmeta_column_values(), collapse = " | ")
#         )
#       })
#       
#       
#       ## Observers ##
#       observeEvent(input$how_make_new_columns, {
#         
#         if (input$how_make_new_columns == "manually") {
#           
#           cnames <- colnames(file_reactive$e_data_file)
#           cnames <- cnames[cnames != file_reactive$e_data_cname]
#           
#           file_reactive$f_data_file <- data.frame("SampleID" = cnames)
#           
#         } else {
#           file_reactive$f_data_file
#         }
#         
#       })
#       
#       
#       return(file_reactive$f_data_file)
#       
#     }
#   )
# }
# 
# 
# 
# observeEvent(reactive_dataholder, {
#   req(!is.null(reactive_dataholder))
#   
#   reactive_dataholder[["f_data"]] <- GenerateFdataServer(
#     id = "f_data",
#     e_data_file = reactive(reactive_dataholder$e_data$file),
#     e_data_cname = reactive(input$e_data_id_col),
#     f_data_file = reactive(reactive_dataholder$f_data$file),
#   )
#   
# }, once = T)
