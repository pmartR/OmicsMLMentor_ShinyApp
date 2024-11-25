
# QC_tab_overlord <- function(){
#   uiOutput("QC_tab_overlord_ui")
# }

# QC_tab_overlord_ui <- function(){
#   div(
#     fluidRow(
#       align = "center",
#       
#       column(1, ""), ## spacer
#       
#       column(1, 
#              actionBttn(
#                inputId = "show_low_obs",
#                label = HTML("Single<br/>Observations"),
#                style = "jelly", 
#                color = "default",
#                size = "s"
#              )
#       ),
#       
#       column(
#         2,
#         br(),
#         shinyWidgets::progressBar(id = "QC_lo_done", value = 0, size = "xs")
#       ),
#       
#       column(1, 
#              disabled(actionBttn(
#                inputId = "show_outlier_detect",
#                label = HTML("Outlier<br/>Detection"),
#                style = "jelly", 
#                color = "default",
#                size = "s"
#              ))
#       ),
#       
#       column(
#         2,
#         br(),
#         shinyWidgets::progressBar(id = "QC_outlier_done", value = 0, size = "xs")
#       ),
#       
#       
#       column(1, 
#              disabled(actionBttn(
#                inputId = "show_missing_data",
#                label = HTML("Data<br/>Missingness"),
#                style = "jelly", 
#                color = "default",
#                size = "s"
#              ))
#       ),
#       
#       column(
#         2,
#         br(),
#         shinyWidgets::progressBar(id = "QC_missing_data_done", value = 0, size = "xs")
#       ),
#       
#       column(1, 
#              disabled(actionBttn(
#                inputId = "review_QC",
#                label = HTML("Review<br/>Selections"),
#                style = "jelly", 
#                color = "default",
#                size = "s"
#              ))
#       ),
#       
#       column(1, "") ## spacer
#       
#     ),
#     
#     fluidRow(
#       
#       column(
#         12,
#         
#         br(),
#         
#         div(
#           id = "low_ob_box",
#           
#           br(), LQ_mols()
#           
#         ),
#         
#         div(
#           id = "remove_outlier_box",
#           
#           br(), QC_outliers()
#           
#         ),
#         
#         div(
#           id = "missing_data_box",
#           
#           br(), missing_data()
#           
#         ),
#         
#         div(
#           id = "QC_review_selection_box",
#           
#           br(),
#           progress_tab("QC"),
#           
#           actionButton("qc_review_done", "Done")
#           
#         )
#         
#       )
#     )
#   )
# }

QC_tab_overlord <- function(){
  div(
    fluidRow(
      align = "center",
      
      column(1, ""), ## spacer
      
      column(1, 
             actionBttn(
               inputId = "show_refnorm",
               label = HTML("Reference<br/>Normalization"),
               style = "jelly", 
               color = "default",
               size = "s"
             )
      ),
      
      column(
        1,
        br(),
        shinyWidgets::progressBar(id = "QC_refnorm_done", value = 0, size = "xs")
      ),
      
      column(1, 
             disabled(actionBttn(
               inputId = "show_low_obs",
               label = HTML("Single<br/>Observations"),
               style = "jelly", 
               color = "default",
               size = "s"
             ))
      ),
      
      column(
        1,
        br(),
        shinyWidgets::progressBar(id = "QC_lo_done", value = 0, size = "xs")
      ),
      
      column(1, 
             disabled(actionBttn(
               inputId = "show_outlier_detect",
               label = HTML("Outlier<br/>Detection"),
               style = "jelly", 
               color = "default",
               size = "s"
             ))
      ),
      
      column(
        1,
        br(),
        shinyWidgets::progressBar(id = "QC_outlier_done", value = 0, size = "xs")
      ),
      
      
      column(1, 
             disabled(actionBttn(
               inputId = "show_missing_data",
               label = HTML("Data<br/>Missingness"),
               style = "jelly", 
               color = "default",
               size = "s"
             ))
      ),
      
      column(
        1,
        br(),
        shinyWidgets::progressBar(id = "QC_missing_data_done", value = 0, size = "xs")
      ),
      
      column(1, 
             disabled(actionBttn(
               inputId = "review_QC",
               label = HTML("Review<br/>Selections"),
               style = "jelly", 
               color = "default",
               size = "s"
             ))
      ),
      
      column(2, "") ## spacer
      
    ),
    
    fluidRow(
      
      column(
        12,
        
        br(),
        
        div(
          id = "refnorm_box",
          
          br(), refnorm_tab()
          
        ),
        
        div(
          id = "low_ob_box",
          
          br(), LQ_mols()
          
        ),
        
        div(
          id = "remove_outlier_box",
          
          br(), QC_outliers()
          
        ),
        
        div(
          id = "missing_data_box",
          
          br(), missing_data()
          
        ),
        
        div(
          id = "QC_review_selection_box",
          
          br(),
          progress_tab(
            "QC",
            plot_choices = c(
              "Single Observations" = "QC__single_obs",
              "RMD Overall" = "QC__rmd_overall",
              "RMD Outliers" = "QC__rmd_outliers",
              "Missingness by Sample" = "QC__missing_samples",
              "Missingness by Feature" = "QC__missing_features"
            ),
            done_btn = actionButton("qc_review_done", "Continue to Model Set-Up"),
            reset_btn = actionButton("reset_qc", "Revert to start of Quality Control")
          )
        )
        
      )
    )
  )
}
