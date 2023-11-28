
# Template for different "stats" tabs for each data type
rollup_tab <- function(tabname) {
  tabPanel("Protein roll-up",
           value = paste0(tabname, "_rollup"),
           class = "collapse_page",
           fluidRow( # begin fluidrow
             column( # sidebarpanel
               4,
               
               collapseBoxGroup(
                 id = paste0(tabname, "_rollup_sidebar"),
                 
                 collapseBox(
                   "Isoform Identification",
                   value = "isoform_ident",
                   
                   uiOutput(paste0(tabname, "_bpquant_options")),
                   uiOutput(paste0(tabname, "_bpquant_filter_anova_UI")),
                   hr(),
                   
                   splitLayout(
                     actionButton(
                       paste0(tabname, "_bpquant"),
                       "Compute Isoforms"
                     ),
                     prettySwitch(
                       inputId = paste0(tabname, "_bpquant_lock"),
                       label = "Unlock/Lock",
                       width = "100%"
                     )
                   )
                 ),
                 
                 collapseBox(
                   "Protein Rollup Options",
                   icon_id = paste0(tabname, "_rollup_picker_icon"),
                   icon = icon("exclamation-sign", lib = "glyphicon"),
                   value = "rollup_opts",
                   collapsed = F,
                   
                   radioButtons(
                     paste0(tabname, "_which_rollup"),
                     div(
                       "Rollup Method",
                       div(
                         style = "color:deepskyblue;display:inline-block",
                         add_prompt(
                           icon("question-sign", lib = "glyphicon"),
                           message = paste(
                             "Reference: Peptides are scaled based on a reference peptide and protein abundance is set as the mean of these scaled peptides.",
                             "Z-Score: Peptides are scaled with z-score transformation and protein abundance is set as the mean of these scaled peptides.",
                             "Quantile: Peptides are selected according to a user selected abundance cutoff value and protein abundance is set as the mean of these selected peptides.",
                             "Centering only: No scaling and protein abundance is set as the mean or median of peptides.",
                             sep = "<br/><br/>"
                           )
                         )
                       )
                     ),
                     c(
                       "Reference" = "rrollup",
                       "Z-Score" = "zrollup",
                       "Quantile" = "qrollup",
                       "Centering only" = "rollup"
                     ),
                     selected = character(0)
                   ),
                   
                   radioGroupButtons(
                     paste0(tabname, "_which_combine_fn"),
                     "Center By:",
                     c("Median" = "median", "Mean" = "mean"),
                     selected = character(0)
                   ),
                   
                   hidden(numericInput(paste0(tabname, "_qrollup_thresh"),
                                       "Quantile cutoff percent:",
                                       value = 5, step = 5, min = 1e-308, max = (100 - 1e-308)
                   )),
                   
                   
                   uiOutput(paste0(tabname, "_bpquant_apply_icon_UI")),
                   
                   hr(),
                   
                   actionButton(paste0(tabname, "_apply_rollup"), "Roll-up peptides"),
                   
                   hidden(div("Applying rollup, please wait...",
                              id = paste0(tabname, "_rollup_busy"),
                              class = "fadein-out",
                              style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
                   ))
                 )
               ),
               
               
               
              actionButton("complete_rollup", "Confirm Selections")
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
