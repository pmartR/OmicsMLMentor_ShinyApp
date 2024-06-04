
# Template for different "stats" tabs for each data type
rollup_tab <- function(tabname) {
  div(
           fluidRow( # begin fluidrow
             column( # sidebarpanel
               4,
               
               collapseBoxGroup(
                 id = paste0(tabname, "_rollup_sidebar"),
                 
                 collapseBox(
                   "Protein Rollup Options",
                   icon_id = paste0(tabname, "_rollup_picker_icon"),
                   icon = icon("exclamation-sign", lib = "glyphicon"),
                   value = "rollup_opts",
                   collapsed = F,
                   
                   br(), br(),
                   tags$b("Rollup Method: "),
                   br(),
                   textOutput(paste0(tabname, "_which_rollup")),
                   br(), br(),
                   tags$b("Center By: "),
                   br(),
                   textOutput(paste0(tabname, "_which_combine_fn")),
                   
                   hr(),
                   
                   actionButton(paste0(tabname, "_apply_rollup"), "Roll-up peptides"),
                   
                   hidden(div(
                     br(),
                     "Applying rollup, please wait...",
                              id = paste0(tabname, "_rollup_busy"),
                              class = "fadein-out",
                              style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
                   ))
                 )
               ),
               
               
               
              hidden(actionButton("complete_rollup", "Confirm Selections"))
             ), # column 4
             
             column(
               8,
               
               
               collapseBoxGroup(
                 id = paste0(tabname, "_rollup_main"),
                 
                 collapseBox(
                   "Protein Roll-up Results",
                   value = "rollup_res",
                   collapsed = F,
                   
                   tabsetPanel(
                     id = paste0(tabname, "_rollup_res_tabpanel"),
                     tabPanel(
                       "Roll-up Visualization",
                       br(),
                       withSpinner(uiOutput(paste0(tabname, "_rollup_res_UI")))
                     ),
                     
                     tabPanel(
                       "Summary",
                       br(),
                       withSpinner(uiOutput(paste0(tabname, "_rollup_data_summary_UI")))
                     )
                   )
                 )
               )
             ) # main_column
           ) # fluidrow
  ) # tabpanel
}

# rollup_tab <- function() {
#   rollup_tab_temp("")
# }
