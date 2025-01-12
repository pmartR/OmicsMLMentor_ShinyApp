
var_select_tab <- function() {
  div(
           fluidRow(
             column(
               4,
               collapseBoxGroup(
                 id = "vs_collapse_left",
                 # file upload collapse sub-div
                 collapseBox(
                   "Select columns to use",
                   icon_id = "vs-usecols",
                   icon = icon("exclamation-sign", lib = "glyphicon"),
                   value = "use_cols_vs",
                   collapsed = F,

                   ## column picker UI based on reactive_dataholder
                   uiOutput("vs_cols_select_UI"),

                   br(),
                   br(),

                   # div(style="display:inline-block",actionButton("vscols_options_done", "Done", style="float:right"))
                   
                   
                   fluidRow(
                     column(10, ""),
                     column(2, actionButton("vscols_options_done", "Done", style="float:right"))
                   )
                 ),
                 
                 conditionalPanel("input.vscols_options_done > 0 && input.use_fdata == 'f_data'", {
                   # div(id = "factor_col_collapse",
                   collapseBox(
                     "Check numeric and categorical columns",
                     icon_id = "vs-factorcols",
                     icon = icon("exclamation-sign", lib = "glyphicon"),
                     value = "factor_cols_vs",
                     collapsed = F,

                     uiOutput("vs_cols_categorical_UI"),
                     # div(style="display:inline-block",actionButton("vscols_cats_done", "Done", style="float:right"))
                     
                     fluidRow(
                       column(10, ""),
                       column(2, actionButton("vscols_cats_done", "Done", style="float:right"))
                     )
                     
                   )#)
                 }),
                 
                 conditionalPanel
                 ("input.vscols_cats_done > 0 & input.user_level_pick != 'beginner'", {
                 # div( id = "int_col_collapse",
                   collapseBox(
                     "Specify Sample Information interaction effects",
                     icon_id = "vs-intcols",
                     icon = icon("exclamation-sign", lib = "glyphicon"),
                     value = "int_cols_vs",
                     collapsed = F,

                     ## categorical column picker UI based on fdata reactive_dataholder and vs_cols_select_UI
                     uiOutput("vs_cols_addint_UI"),
                     # div(style="display:inline-block",actionButton("vscols_ints_done", "Done", style="float:right"))
                     
                     fluidRow(
                       column(10, ""),
                       column(2, actionButton("vscols_ints_done", "Done", style="float:right"))
                     )
                   )
                 # )
                 })

               ), # parent collapse
               
               hidden(actionButton("done_VS", "Confirm Selections"))

             ), # column 4
             column(
               8,
               collapseBoxGroup(
                 id = "vs_collapse_right", multiple = TRUE,
                 collapseBox("Data Preview",
                             collapsed = FALSE,
                             value = "data_preview_all",
                             fluidRow(
                               column(12, uiOutput("preview_all_data_UI"))
                             ),
                             
                             br(),
                             strong("Note: Data preview will only show first 500 rows."),
                             br()
                 ),
                 
                 uiOutput("detected_box_varsel")
                 
                 # collapseBox("Data Properties",
                 #             value = "detected_plots",
                 #             uiOutput("vs_tab_boxplots")
                 # )
               )
             ) # column 8
           ) # fluidrow
  )
}
