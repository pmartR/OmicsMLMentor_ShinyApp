
RM_tab <- function(){
  div(
    fluidRow(
      
      column(
        4,
        collapseBoxGroup(
          id = "rm_collapse",
          
          uiOutput("run_model_UI")
          
        ),
        
        disabled(actionButton("complete_RM", "Continue"))
      ),
      
      column(
        8,
        collapseBoxGroup(
          
          collapseBox(
            "Prediction Plots",
            value = "predictions_plots_box",
            collapsed = F,
            
            tabsetPanel(
              tabPanel("Prediction Bar",
                       br(),
                       plotlyOutput("predict_plot_predictBar")),
              tabPanel("Confidence Scatter",
                       br(),
                       plotlyOutput("predict_plot_confidenceScatter"),
                       uiOutput("true_pos_picker_UI")),
              tabPanel("ROC Curve",
                       br(),
                       plotlyOutput("predict_plot_ROC")),
              tabPanel("Confusion Heatmap",
                       br(),
                       plotlyOutput("predict_plot_confusionHeatmap"))
            )
          )
          
        )
      )
    )
  )
}
