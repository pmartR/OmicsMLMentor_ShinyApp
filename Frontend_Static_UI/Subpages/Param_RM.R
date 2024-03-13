
## ?dials::cost_complexity() can help with optimizing parameters ##
## https://www.tidymodels.org/start/tuning/ ##

param_RM_UI <- function(tuning) {
  div("Parameter Optimization",
           #value = "PO_RM",
           #class = "collapse_page",
           column(
             4,
             
             ## Feature selection
             
             conditionalPanel(
               
               "input.pick_model_EM == 'Multinomial regression' | input.pick_model_EM == 'Logistic regression'", {
               collapseBoxGroup(
                 id = "FS_RM_side_collapse", multiple = FALSE, open = c("choice_VS_RM"),
                 
                 collapseBox(
                   
                   div(
                     "Use Feature Selection"#,
                   ),
                   value = "choice_VS_RM",
                   collapsed = F,
                   
                   radioGroupButtons("use_reg", label = "Perform feature selection?", choices = c("Yes", "No")),
                   
                   actionButton("done_FS_option", "Done")
                 ),
                 
                 conditionalPanel("input.use_reg == 'Yes'", {
                   collapseBox(
                     
                     div(
                       "Regularization method"#,
                       # hidden(div(id = "ok_data_filters", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
                     ),
                     value = "reg_methods_box",
                     collapsed = F,
                     
                     pickerInput("reg_method", choices = c("Lasso", "Ridge", "Elastic Net"#, "Forward Selection", "Backward Selection"
                     )),
                     
                     checkboxInput("optimize_reg", "Optimize?", value = F),
                     
                     
                     conditionalPanel(
                       "input.optimize_reg == true",
                       numericInput("optim_lvls", 
                                    "Number of levels to test for optimization",
                                    min = 1, max = 10, value = 5
                       )
                     ),
                     
                     uiOutput("summary_optim_fs"),
                     
                     # actionButton("reg_optimize", "Optimize"),
                     actionButton("done_method_RM_box", "Done")
                   )
                 })
               ) # parent collapse
             }),
             
             collapseBoxGroup(
               id = "PO_RM_side_collapse", multiple = FALSE, open = c("side_param_RM"),
               
               # biomolecule filters
               collapseBox(
                 
                 div(
                   "Define model parameters"#,
                   # hidden(div(id = "ok_data_filters", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
                 ),
                 value = "side_param_RM",
                 collapsed = F,
                 
                 uiOutput("model_specific_parameters"),
                 
                 uiOutput("optimization_summary"), ## Number of levels ^ number of selected parameters
                 
                 hidden(div("Performing analysis, please wait...",
                            id = "HP_busy",
                            class = "fadein-out",
                            style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
                 )),
                 
                 uiOutput("param_opti_UI")
               )

             ), # parent collapse
             
             
             hidden(
             actionButton("complete_param", "Confirm Selections")
             )
             
           ), # column 4
           column(
             8,
             collapseBoxGroup(
               id = "Param_plots_preview", multiple = TRUE, open = c("param_preview"),
               collapseBox("Visualize parameter optimizations",
                           collapsed = F,
                           value = "param_preview",
                           uiOutput("param_preview_plot_ui")
               )             )
           ) # column 8
  )
}
