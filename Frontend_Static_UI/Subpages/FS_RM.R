# VS_RM_UI <- function() {
#   tabPanel("Feature Selection",
#            value = "FS_RM",
#            class = "collapse_page",
#            column(
#              4,
#              collapseBoxGroup(
#                id = "FS_RM_side_collapse", multiple = FALSE, open = c("choice_VS_RM"),
#                
#                collapseBox(
#                  
#                  div(
#                    "Use Feature Selection"#,
#                    # hidden(div(id = "ok_data_filters", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
#                  ),
#                  value = "choice_VS_RM",
#                  collapsed = F,
#                  
#                  radioGroupButtons("use_reg", label = "Perform feature selection?", choices = c("Yes", "No")),
#                  
#                  actionButton("done_FS_option", "Done")
#                ),
#                
#                conditionalPanel("input.use_reg == 'Yes'", {
#                  collapseBox(
#                    
#                    div(
#                      "Regularization method"#,
#                      # hidden(div(id = "ok_data_filters", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
#                    ),
#                    value = "reg_methods_box",
#                    collapsed = F,
#                    
#                    pickerInput("reg_method", choices = c("Lasso", "Ridge", "Elastic Net"#, "Forward Selection", "Backward Selection"
#                    )),
#                    
#                    checkboxInput("optimize_reg", "Optimize regularization?", value = F),
#                    
#                    
#                    conditionalPanel(
#                      "input.optimize_reg == true",
#                      numericInput("optim_lvls", 
#                                   "Number of levels to test for optimization",
#                                   min = 1, max = 10, value = 5
#                      )
#                    ),
#                    
#                    uiOutput("summary_optim_fs"),
#                    
#                    actionButton("reg_optimize", "Optimize"),
#                    actionButton("done_method_RM_box", "Done")
#                  )
#                })
#                
#                # collapseBox(
#                #   
#                #   div(
#                #     "Regularization parameters"#,
#                #     # hidden(div(id = "ok_data_filters", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
#                #   ),
#                #   value = "FS_param",
#                #   collapsed = F,
#                #   numericInput("selection_num", "Keep X number of variables:", min = 0, max = 50, value = 10),
#                #   
#                #   actionButton("done_FSparam_box", "Done")
#                # )
#              ), # parent collapse
#              
#              hidden(actionButton("complete_FS", "Confirm Selections"))
#              
#            ), # column 4
#            column(
#              8,
#              collapseBoxGroup(
#                id = "FS_plots_preview", multiple = TRUE, open = c("FS_preview"),
#                collapseBox("Visualize feature selection and optimization plots",
#                            collapsed = F,
#                            value = "FS_preview",
#                            withSpinner(plotOutput("FS_preview_plot"))
#                )             )
#            ) # column 8
#   )
# }