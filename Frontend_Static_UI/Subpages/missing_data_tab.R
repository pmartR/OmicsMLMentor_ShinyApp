## change terminology to detected

missing_data <- function(){
  
  # tabPanel("Missing data",
           
  
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
                                min = 0, max = 100, value = 20),
                 
                 br(),
                 
                 # actionButton("done_sample_miss", "Done")
                 
                 fluidRow(
                   column(10, ""),
                   column(2, actionButton("done_sample_miss", "Done", style="float:right"))
                 )

               ), 
               
               collapseBox(
                 value = "missing_by_biomolecule",
                 titletext = "Detection by Biomolecule",
                 
                 br(),
                 "Some biomolecules may not be detected in all samples.",
                 br(),
                 br(),
                 paste0("Some unsupervised models accept incomplete detection of biomolecules, however",
                        " unsupervised models cannot. To use these models, biomolecules with incomplete detection can be handled in the following ways:"),
                 
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
                 
                 pickerInput(
                   "missing_options",
                   "Preview biomolecule detection handling:",
                   choices = c(
                     "Keep data as-is" = "keep",
                     "Estimate values in samples with no biomolecule detection" = "impute",
                     "Convert undetected biomolcule values to 0, all other values to 1" = "convert",
                     "Remove biomolecules with incomplete detection" = "remove"
                   ),
                   multiple = T
                 ),
                 
                 radioGroupButtons("keep_missing", "Keep data as-is?",
                                   choices = c("No", "Yes")),
                 
                 shiny::textOutput("Note_nonmissing"),
                 
                 br(),
                 
                 fluidRow(
                   column(10, ""),
                   column(2, actionButton("done_biom_miss", "Done", style="float:right"))
                 )
                 
                 # actionButton("done_biom_miss", "Done")
                 
               )
                 
                 
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
                     plotOutput("missing_data_hist_sample")
                   ),
                   
                   collapseBox(
                     "Individual biomolecule detection across samples - Plot",
                     value = "missing_data_biomolecule_plot",
                     plotOutput("missing_data_hist_biomolecule"),
                     uiOutput("slider_options_ui")
                     
                   )
                   
                   
                 )
                 
                 
          )
          )
  
}
