# 
# models <- list(
#   `Linear support vector machine` = "lsvm",
#   `Polynomial support vector machine` = "psvm",
#   `Radial basis support vector machine` = "rsvm",
#   `Multinomial regression` = "multi",
#   `Multinomial regression with LASSO` = "multilasso",
#   `Logistic regression` = "logistic",
#   `Logistic regression with LASSO` = "loglasso",
#   `Random forest` = "rf" 
# )
# 
# choiceValues <- models
# 
# 
# choiceNames <- map(names(models), function(mod){
#   flipBox(
#     id = paste0("expert_mentor_", mod),
#     width = 3,
#     front = div(
#         h1(mod),
#         br(),
#         
#         img(src = "graph.png", width = 250, height = 120)#,
#         #br(),
#         #actionButton(paste0(mod, "_button"), "Learn more")
#       ),
#     back = div(
#         h1(mod),
#         br(),
#         
#         "Info here"
#       )
#   )
# })
# 
# 
# EM_tab <- function() {
#   tabPanel("Expert Mentor",
#            class = "collapse_page",
#            fluidRow(
#              column(
#                4,
#                collapseBoxGroup(
#                  id = "em_collapse_left",
#                  # file upload collapse sub-div
#                  collapseBox(
#                    icon_id = "em-upload-fmeta",
#                    icon = icon("exclamation-sign", lib = "glyphicon"),
#                    value = "em_summary",
#                    collapsed = F,
#                    
#                    "Model Selection Summary",
#                    
#                    ## stuff from previous page and what is selected from models
#                    ## N models available from goals
#                    ## Features detected in data
#                    uiOutput("em_data_goals"),
#                    
#                    ## Selected model info
#                    uiOutput("em_select_model")
#                    
#                  ),
#                  
#                  
#                  collapseBox(
#                    "Advanced Settings", 
#                    icon_id = "em-advset",
#                    icon = icon("exclamation-sign", lib = "glyphicon"),
#                    value = "em_advset_box",
#                    collapsed = T,
#                    
#                    tabsetPanel(
#                      id = "advanced_settings", 
#                      
#                      tabPanel(
#                        "Sort By",
#                        
#                        orderInput(id = "soft_sort",
#                                    "What order should model options be prioritized?",
#                                   c(
#                                     "Reports best predictors",
#                                     "Generates an equation",
#                                     "Handles missing data",
#                                     "Selects best predictors",
#                                     "Handles large number of predictors",
#                                     "Handles few number of samples"
#                                   )
#                                   
#                                   )
#                        
#                        ),
#                      
#                      tabPanel(
#                        "Filter By",
#                        
#                        checkboxInput(id = "feature_selection",  
#                                      "Selects best predictors for model"),
#                        
#                        checkboxInput(id = "equation",  
#                                      "Model outputs an equation"),
#                        
#                        checkboxInput(id = "missingness",  
#                                      "Model handles missing data"),
#                        
#                        checkboxInput(id = "explainability",  
#                                      "Model shows most important predictors"),
#                        
#                        checkboxInput(id = "pred_sample_ratio",  
#                                      "Model handles a large number of predictors, even with few samples"),
#                        
#                        checkboxInput(id = "n_levels",  
#                                      "Model can predict 2+ groups"),
#                        
#                        
#                        )
#                      
#                      
#                      
#                      )
#                    
#                  )
#                  
#                ), # parent collapse
#                div(id = "check_em", class = "tooltip-wrapper",
#                    actionButton(inputId = "em_select", 
#                                 label = "Confirm model selections"
#                    )
#                )
#              ), # column 4
#              column(
#                8,
#                wellPanel(
# 
#                  div(
#                    style = 'overflow-y: scroll',
#                    
#                    radioButtons(
#                      "models",
#                      label = "",
#                      choiceNames = choiceNames,
#                      choiceValues = choiceValues,
#                      inline = T,
#                      selected = character(0)
#                    )
#                  )
#                )
#              ) # column 8
#            ) # fluidrow
#   )
# }
