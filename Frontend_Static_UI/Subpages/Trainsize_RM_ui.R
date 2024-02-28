TS_RM_UI <- function() {
  tabPanel("Training structure",
           value = "TS_RM",
           class = "collapse_page",
           column(
             4,
             collapseBoxGroup(
               id = "TS_side_collapse", multiple = FALSE, open = c("train_param_RM"),
               
               # biomolecule filters
               collapseBox(
                 
                 
                 ## based on https://neptune.ai/blog/cross-validation-in-machine-learning-how-to-do-it-right
                 ## https://www.brainstobytes.com/test-training-and-validation-sets/
                 
                 div(
                   "Specify data subsets"#,
                   # hidden(div(id = "ok_data_filters", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
                 ),
                 value = "train_param_RM",
                 collapsed = F,
                 
                 ## Do we allow a seed or pick our own?
                 
                 ## if hp tuning and holdout
                 uiOutput("holdout_set"), ## holdout_valid
                 
                 ## If no hp tuning or holdout
                 uiOutput("crossval_perform"), ## !holdout_valid
                 
                 ## If hp tuning
                 uiOutput("crossval_hp"), ## tuning option avaialable
                 
                 ### What model is returned in this case??
                 # conditionalPanel(
                 #   "input.training_type == 'loocv'",
                 #   
                 #   div(
                 #     br(),
                 #     "N models will generate equal to the number of samples. Training data will include all but one sample, and validation to assess model performance will be performed on the remaining sample. The average performance over all the models estimates overall training performance.",
                 #     br(),
                 #     br()
                 #   )
                 #   ),
                 # 
                 # conditionalPanel(
                 #   "input.training_type == 'kfcv'",
                 #   div(
                 #     br(),
                 #     "Training data will include all but one subset (K-1), and testing to assess model performance will be performed on the remaining subset. The average performance over all the models estimates overall training performance.",
                 #     br(),
                 #     br(),
                 #   numericInput("nFolds", "Number of folds to use", value = 5, min = 2, max = 10)
                 #   )
                 # ),
                 
                 uiOutput("train_test_summary") ## Test vs Validation report by group, leave one out vs k-fold image, random seed
                 
                 # actionButton("rec_split", "Use recommended split"),
                 # actionButton("done_crossval_option", "Done")
               )
             ), # parent collapse
             
             hidden(
               actionButton("complete_TS_RM", "Confirm Selections")
               )
             
           ), # column 4
           column(
             8,
             collapseBoxGroup(
               id = "TS_plots_preview", multiple = TRUE, open = c("TS_preview"),
               collapseBox("Visualize training/testing split",
                           collapsed = F,
                           value = "TS_preview",
                           
                           tabsetPanel(
                             id = "training_tabset",
                             tabPanel(
                               "Training structure",
                               withSpinner(plotOutput("TS_preview_plot"))
                             )
                           )
               )             )
           ) # column 8
  )
}
