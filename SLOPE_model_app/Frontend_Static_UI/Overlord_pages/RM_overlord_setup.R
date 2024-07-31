

RM_tab_overlord <- function(){
  div(
      div(
        actionButton("run_model","Run Model")
      ),
      div(
        plotOutput("predict_plot")
      )
    )
}