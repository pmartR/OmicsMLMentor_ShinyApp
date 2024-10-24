

# RM_tab <- function(){
#   div(
#       div(
#         uiOutput("run_model_UI")
#       ),
#       div(
#         tabsetPanel(
#           tabPanel("ROC Curve",
#                    plotOutput("predict_plot_ROC")),
#           tabPanel("Prediction Bar",
#                    plotOutput("predict_plot_predictBar")),
#           tabPanel("Confusion Heatmap",
#                    plotOutput("predict_plot_confusionHeatmap")),
#           tabPanel("Confidence Scatter",
#                    plotOutput("predict_plot_confidenceScatter"))
#           )
#         )
#   )
# }




RM_tab <- function(){
  div(
    fluidRow(
      
      column(
        4,
        collapseBoxGroup(
          id = "rm_collapse",
          
          uiOutput("run_model_UI")
          
        )
      ),
      
      column(
        8,
        collapseBoxGroup(
          
          collapseBox(
            "Prediction Plots",
            value = "predictions_plots_box",
            collapsed = F,
            
            tabsetPanel(
              tabPanel("ROC Curve",
                       plotlyOutput("predict_plot_ROC")),
              tabPanel("Prediction Bar",
                       plotlyOutput("predict_plot_predictBar")),
              tabPanel("Confusion Heatmap",
                       plotlyOutput("predict_plot_confusionHeatmap")),
              tabPanel("Confidence Scatter",
                       plotlyOutput("predict_plot_confidenceScatter"))
            )
            
          )
          
        )
      )
    )
  )
}
