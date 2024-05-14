
groups_tab <- function() {
  div(
           fluidRow(
             column(
               4,
               collapseBoxGroup(
                 id = "groups_collapse_left",
                 # file upload collapse sub-div
                 collapseBox(
                   "Sample Information File Options", 
                   icon_id = "groups-upload-fmeta",
                   icon = icon("exclamation-sign", lib = "glyphicon"),
                   value = "fdata_options",
                   collapsed = F,
                   
                   
                   radioGroupButtons(
                     inputId = "use_fdata", label = "Include Sample Information in workflow?",
                     choices = c("Yes" = "f_data", "No" = ""), selected = character(0)
                   ),
                   
                   "(Allows for response or coloring designation on experimental factors or other Sample Information)",
                   
                   conditionalPanel(
                     condition = "input.use_example == true && input.use_fdata == 'f_data'",
                     div(
                       br(),
                       checkboxInput("use_example_fdata", "Use example Sample Information?", value = F),
                     )
                   ),
                   
                   uiOutput("how_make_fdata_UI"),
                   
                   br(),
                   br(),
                   
                   fluidRow(
                     column(10, ""),
                     column(2, actionButton("fdata_options_done", "Done", style="float:right"))
                   )
                 ),
                 
                 uiOutput("f_data_upload_UI"),
                 
                 uiOutput("f_data_generate_UI"),
                 
                 uiOutput("f_meta_spec_UI")
                 
               ), # parent collapse
               
               
               div(id = "check_group_cols_wrapper", class = "tooltip-wrapper",
                   actionButton(inputId = "check_group_cols", 
                                label = "Confirm selections"
                                )
               )
             ), # column 4
             column(
               8,
               collapseBoxGroup(
                 id = "groups_collapse_right", multiple = TRUE,
                 collapseBox("Sample Information Preview",
                             value = "data_preview_fdata",
                             
                             tabsetPanel(id = "preview_data_f_data")
                             # DTOutput("DT_f_data")
                 ),
                 
                 uiOutput("detected_box_group")
                 # collapseBox("Data Properties",
                 #             value = "fdata_plots",
                 #             uiOutput("Group_plot_picker"),
                 #             plotlyOutput("Group_tab_plots")
                 #             # uiOutput("group_tab_boxplots")
                 # )
               )
             ) # column 8
           ) # fluidrow
  )
}
