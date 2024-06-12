

EM_tab <- function() {
  
  # tabPanel("Expert Mentor",
           # class = "collapse_page",
           fluidRow(
             column(
               4,
               collapseBoxGroup(
                 id = "em_collapse_left",
                 # file upload collapse sub-div
                 collapseBox(
                   icon_id = "em-upload-fmeta",
                   icon = icon("exclamation-sign", lib = "glyphicon"),
                   value = "em_summary",
                   collapsed = F,
                   
                   "Model Selection Summary",
                   
                   uiOutput("pick_EM_model_UI"),
                   
                   ## stuff from previous page and what is selected from models
                   ## N models available from goals
                   ## Features detected in data
                   uiOutput("em_data_goals"),
                   
                   ## Selected model info
                   uiOutput("em_select_model")
                   
                 ),
                 
                 
                 conditionalPanel(
                   "input.user_level_pick != 'beginner'",
                 collapseBox(
                   "Advanced Settings", 
                   icon_id = "em-advset",
                   icon = icon("exclamation-sign", lib = "glyphicon"),
                   value = "em_advset_box",
                   collapsed = T,
                   
                   #### To be re-implementing with weightings
                   column(6,
                     wellPanel(
                         br(),
                         
                         checkboxInput("equal_sort", "Prioritize all model features equally", value = T),
                         
                         conditionalPanel("input.equal_sort != true", {
                           div(
                             "Prioritize model features in this order:",
                             orderInput(inputId = "soft_sort",
                                        label = "",
                                        items = list(
                                          "N samples",
                                          "N classifications",
                                          "N predictors",
                                          "Minimum group size",
                                          "Sample/predictor ratio",
                                          "Missingness handling",
                                          "Predictor selection",
                                          "Explainability",
                                          "Equation"
                                        ),
                                        width = "100%",
                                        class = "btn-group-vertical"
                             )
                           )
                         })
                         
                         )),
                   
                     
                     column(6,
                      wellPanel("Rank models considering these elements:",
                                
                                checkboxInput(
                                  inputId = "feature_selection",
                                  "Keep only the best predictors in the model",
                                  value = T),
                                
                                checkboxInput(
                                  inputId = "explainability",
                                  "Generate easily interpretable results",
                                  value = T),

                                checkboxInput(
                                  inputId = "equation",
                                  "Generates an equation for determining a prediction",
                                  value = T),

                                checkboxInput(
                                  inputId = "handles_missingness",
                                  "Handle any missingness",
                                  value = T),

                                
                                checkboxInput(
                                  inputId = "high_dimensional_data",
                                  "Handles many features",
                                  value = T
                                  ),
                                
                                conditionalPanel(
                                  "input.user_level != 'familiar'",
                                  
                                  div(
                                    checkboxInput(
                                      inputId = "samples_per_feature",
                                      "Handle a large feature:sample ratio",
                                      value = T),
                                    
                                    checkboxInput(
                                      inputId = "prone_to_overfit",
                                      "Innately avoid overfitting",
                                      value = T),
                                    
                                    checkboxInput(
                                      inputId = "correlation",
                                      "Manage highly correlated features",
                                      value = T),
                                    
                                    checkboxInput(
                                      inputId = "handles_outliers",
                                      "Robustly manage possible outliers",
                                      value = T)
                                    
                                    
                                  ))
                                
                      ) )

                   # tabsetPanel(
                   #   id = "advanced_settings", 
                   #   
                   #   tabPanel(
                   #     "Sort By",
                   #     br(),
                   #     
                   #     orderInput(inputId = "soft_sort",
                   #                 label = "What order should model options be prioritized?",
                   #                items = list(
                   #                  "Feature selection",
                   #                  "Handles missingness",
                   #                  "Predictor weightings",
                   #                  "Generates equation",
                   #                  "Handles many predictors",
                   #                  "Predicts 2+ response groups"
                   #                ),
                   #                class = "btn-group-vertical"
                   #                )
                   #     
                   #     ),
                   #   
                   #   tabPanel(
                   #     "Filter By",
                   #     
                   #     checkboxInput(inputId = "feature_selection",  
                   #                   "Feature selection"),
                   #     
                   #     checkboxInput(inputId = "equation",  
                   #                   "Generates equation"),
                   #     
                   #     checkboxInput(inputId = "missingness",  
                   #                   "Handles missingness"),
                   #     
                   #     checkboxInput(inputId = "explainability",  
                   #                   "Predictor weightings"),
                   #     
                   #     checkboxInput(inputId = "pred_sample_ratio",  
                   #                   "Handles many predictors"),
                   #     
                   #     checkboxInput(inputId = "n_levels",  
                   #                   "Predicts 2+ response groups"),
                   #     
                   #     
                   #     )
                   #   
                   #   
                   #   
                   #   )
                   
                 ))
                 
               ), # parent collapse
               div(id = "check_em", class = "tooltip-wrapper",
                   actionButton(inputId = "em_select", 
                                label = "Confirm Selections"
                   )
               )
             ), # column 4
             
             column(
               8,
               # 
               # br(),
               # 
               # br(),
               
               wellPanel(
                 
                 div(
                   style = 'height:700px',
                   
                   br(),
                   
                   fluidRow(
                    uiOutput("EM_dashboard")
                   )

                 )
               )
             ) # column 8
           ) # fluidrow
  # )
}
