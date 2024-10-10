

RM_tab <- function(){
  div(
      div(
        actionButton("run_model","Run Model")
      ),
      div(
        tabsetPanel(
          tabPanel("ROC Curve",
                   plotOutput("predict_plot_ROC")),
          tabPanel("Prediction Bar",
                   plotOutput("predict_plot_predictBar")),
          tabPanel("Confusion Heatmap",
                   plotOutput("predict_plot_confusionHeatmap")),
          tabPanel("Confidence Scatter",
                   plotOutput("predict_plot_confidenceScatter"))
          )
        )
  )
  }