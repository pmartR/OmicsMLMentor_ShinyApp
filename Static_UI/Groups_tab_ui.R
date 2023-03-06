
groups_tab <- function() {
  tabPanel("Upload Sample Data",
           class = "collapse_page",
           fluidRow(
             column(
               4,
               collapseBoxGroup(
                 id = "groups_collapse_left",
                 # file upload collapse sub-div
                 collapseBox(
                   "Sample data file options", 
                   icon_id = "groups-upload-fmeta",
                   icon = icon("exclamation-sign", lib = "glyphicon"),
                   value = "fmeta_options",
                   collapsed = F,
                   
                   
                   radioGroupButtons(
                     inputId = "use_fdata", label = "Include sample data in workflow?",
                     choices = c("Yes" = "TRUE", "No" = "FALSE"), selected = character(0)
                   ),
                   
                   "(Allows for response or coloring designation on experimental factors or other sample data)",
                   
                   br(), br(),
                   
                   conditionalPanel(
                     condition = "input.use_fdata == 'TRUE'",
                     div(
                       radioGroupButtons(
                         inputId = "how_make_fdata", label = "Create a grouping structure from:",
                         choices = c("Uploaded file" = "upload", 
                                     "Experimental data column names" = "colnames"),
                         selected = character(0)
                       )
                     )
                   ),
                   
                   br(),
                   
                   conditionalPanel(
                     condition = "input.use_example == true && input.use_fdata == 'TRUE'",
                     div(
                       checkboxInput("use_example_fdata", "Use example sample data?", value = F)
                     )
                   ),
                   
                   br(),
                   
                   actionButton("fdata_options_done", "Done")
                 ),
                 
                 uiOutput("f_data_upload_UI"),
                 
                 uiOutput("f_data_generate_UI"),
                 
                 uiOutput("f_meta_spec_UI")#,
                 
                 # collapseBox(
                 #   "Select sample ID columns", 
                 #   icon_id = "groups-id-columns",
                 #   icon = icon("exclamation-sign", lib = "glyphicon"),
                 #   
                 #   value = "fmeta_identify_and_align",
                 #   uiOutput("fmeta_id_cols"),
                 #   div(
                 #     class = "inline-wrapper-1",
                 #     div(
                 #       id = "align_fmeta_wrapper", class = "tooltip-wrapper",
                 #       appButton(inputId = "align_fmeta", label = "I need to align the sample names"),  
                 #     ),
                 #     div(
                 #       id = "align_fmeta_tooltip",
                 #       add_prompt(blueq, message = ttext[["ALIGNED_COLUMNS"]])
                 #     )
                 #   )
                 # ),
                 # collapseBox(
                 #   "Create grouping columns", 
                 #   icon_id = "groups-create-columns",
                 #   icon = icon("exclamation-sign", lib = "glyphicon"),
                 #   
                 #   value = "fmeta_create_columns",
                 #   radioGroupButtons("how_make_new_columns", "Create new columns:",
                 #                     choices = c("Manually" = "manually", "From existing columns" = "colvalues")
                 #   ),
                 #   hr(),
                 #   textInput("new_column_name", "Pick a name for this new column"),
                 #   uiOutput("make_fmeta_columns"),
                 #   uiOutput("extract_levels"),
                 #   hr(),
                 #   uiOutput("new_column_preview"),
                 #   br(),
                 #   appButton(inputId = "add_fmeta_col_submit", label = "Add this column")
                 # ),
                 # ID column collapse sub-div
                 # collapseBox(
                 #   "Main Effects, Covariates, Pairing",
                 #   icon_id = "grouping-columns",
                 #   icon = icon("exclamation-sign", lib = "glyphicon"),
                 #   value = "fmeta_groups_covariates",
                 #   
                 #   wellPanel(
                 #     uiOutput("grouping_cols"),
                 #     uiOutput("covariates_type_picker_UI")
                 #   )
                 # )
               ), # parent collapse
               div(id = "check_group_cols_wrapper", class = "tooltip-wrapper",
                   actionButton(inputId = "check_group_cols", 
                                label = "Verify and use these values", class = "btn-primary")
               )
             ), # column 4
             column(
               8,
               collapseBoxGroup(
                 id = "groups_collapse_right", multiple = TRUE,
                 collapseBox("Sample Data Preview",
                             value = "data_preview_fdata",
                             DTOutput("data_preview_fdata")
                 ),
                 collapseBox("Detected Properties",
                             value = "fdata_plots",
                             uiOutput("group_tab_boxplots")
                 )
               )
             ) # column 8
           ) # fluidrow
  )
}
