
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
        
        uiOutput("run_model_plots_UI")
      )
    )
  )
}
