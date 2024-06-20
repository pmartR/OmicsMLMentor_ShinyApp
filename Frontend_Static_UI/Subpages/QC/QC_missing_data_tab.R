## change terminology to detected

missing_data <- function(){

           div(
               
          column(4,
             collapseBoxGroup(
               id = "missing_data_box",
               
               collapseBox(
                 collapsed = F,
                 titletext = "Percentage observed biomolecules by Sample",
                 value = "missing_data_sample_box",
                 
                 "Samples with a low level of biomolecule detection can offer limited or ",
                 "biased information in subsequent modeling. ",
                 br(), br(),
                 
                 "Samples with low biomolecule detection can be removed by ",
                 "setting a minimum detection threshold below.",
                 
                 br(), br(),
                   
                   numericInput("missing_value_thresh", 
                                "Keep samples with at least X% of biomolecules detected:", 
                                min = 0, max = 100, value = 50),
                 
                 br(),
                 
                 # actionButton("done_sample_miss", "Done")
                 
                 fluidRow(
                   column(10, ""),
                   column(2, actionButton("done_sample_miss", "Done", style="float:right"))
                 )

               ),
               
               collapseBox(
                 value = "missing_by_biomolecule",
                 titletext = textOutput("qc_biomolecule_title"),
                 
                 hidden(
                   
                   div(
                     id = "protein_rollup_box",
                     
                     "Since some operations run on the protein level, please select a rollup method to use later:",
                     
                     br(),
                     br(),
                     
                     radioButtons(
                       paste0("qc_which_rollup"),
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
                       paste0("qc_which_combine_fn"),
                       "Center By:",
                       c("Median" = "median", "Mean" = "mean"),
                       selected = character(0)
                     ),
                     
                     hidden(numericInput(paste0("qc_qrollup_thresh"),
                                         "Quantile cutoff percent:",
                                         value = 5, step = 5, min = 1e-308, max = (100 - 1e-308)
                     )),
                     
                     hr(),
                     
                     actionButton(paste0("qc_apply_rollup"), "Get protein-level information"),
                     
                     hidden(div(
                       br(),
                       "Getting protein-level information, please wait...",
                                id = paste0("qc_rollup_busy"),
                                class = "fadein-out",
                                style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
                     )),
                     
                     hr()
                   )
                 ),
                 
                 hidden(div(
                   id = "qc_biomolecule_detect",
                   br(),
                   "Some biomolecules may not be detected in all samples.",
                   br(),
                   br(),
                   paste0("Some unsupervised models accept incomplete detection of biomolecules, however",
                          " currently supported supervised models cannot. To use these models, biomolecules with incomplete detection can be handled in the following ways:"),
                   
                   br(),
                   br(),
                   tags$div(
                     tags$ul(
                       tags$li("Removing the biomolecule"),
                       tags$li("Estimating the undectected biomolecule values from other values"),
                       tags$li("Convert biomolecule values to 0s where undetected across samples, while dectected biomolecule values are set to 1")
                     )
                   ),
                   
                   br(),
                   
                   "Refer to graphs and summary on the right for assistance in determining",
                   " whether or not to keep undetected biomolecule values.",
                   br(),
                   strong("Note: percentage of biomolecule detection may change in further processing steps."),
                   br(),
                   br(),
                   
                   radioGroupButtons("keep_missing", "Keep data as-is?",
                                     choices = c("No", "Yes"), selected = "No"),
                   
                   pickerInput(
                     "missing_options",
                     "Preview biomolecule detection handling:",
                     choices = c(
                       # "Keep data as-is" = "keep",
                       "🟩 Estimate values in samples with no biomolecule detection" = "impute",
                       "🟧 Convert undetected biomolcule values to 0, all other values to 1" = "convert",
                       "🟥 Remove biomolecules with incomplete detection" = "remove"
                     ),
                     multiple = T
                   ),
                   
                   textOutput("Note_nonmissing"),
                   textOutput("warn_missing_biom"),
                   
                   br(),
                   
                   fluidRow(
                     column(10, ""),
                     column(2, disabled(actionButton("done_biom_miss", "Done", style="float:right")))
                   )
                   
                   # actionButton("done_biom_miss", "Done")
                 )
                 
               ))
                 
                 
               ), 
             
             
             hidden(actionButton("done_md", "Confirm Selections"))
               
             ),
          
          column(8,
                 collapseBoxGroup(
                   id = "qc_missing_plots",
                   collapseBox(
                     "Total biomolecules detected by sample - Plot",
                     value = "missing_data_sample_plot",
                     collapsed = F,
                     plotlyOutput("missing_data_hist_sample"),
                     prettySwitch("missing_data_hist_sample_names", "Show sample names", value = TRUE, inline = TRUE, fill = TRUE, status = "primary"),
                     prettySwitch("missing_data_hist_sample_prop", "Show proportion labels", value = TRUE, inline = TRUE, fill = TRUE, status = "primary")
                   ),
                    
                   collapseBox(
                     "Individual biomolecule detection across samples - Plot",
                     value = "missing_data_biomolecule_plot",
                     hidden(div(
                       id = "qc_biomolecule_detect_plot",
                       plotlyOutput("missing_data_hist_biomolecule")
                     )),
                     br(),
                     uiOutput("slider_options_ui")
                     
                   )
                   
                   
                 )
                 
                 
          )
          )
  
}
