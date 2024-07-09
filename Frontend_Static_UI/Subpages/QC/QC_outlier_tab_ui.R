
QC_outliers <- function(){
  
  tabname <- "QC"
  
  div(
  #   "Detect and Remove Outliers",
    
    column(
      3,
      
      uiOutput("outlier_collapse_UI"),
      actionButton("outliers_done", "Confirm Selections")
    ),
    
    column(
      8,
      
      tabsetPanel(
        id = "QC_outlier_tabset",
        tabPanel(
          value = "all_out",
          "Visualize all potential outliers",
          plotlyOutput("rmd_plot_qc_all")
        ),
        
        tabPanel(
          value = "inspect_samp",
          "Inspect samples",
          plotlyOutput("rmd_plot_qc_select")
        )
        
      )
      # collapseBoxGroup(
      #   id = "QC_outlier_plots",
      #   collapseBox(
      #     "Visualize all potential outliers",
      #     value = "outlier_plots_all",
      #     collapsed = F,
      #     plotlyOutput("rmd_plot_qc_all")
      #   )
      # )
    ), # column 8
  
    # column(
    #     4,
    #   collapseBoxGroup(
    #     id = "QC_outlier_plots_inspect",
    #     collapseBox(
    #       "Inspect samples",
    #       value = "outlier_plots_select",
    #       collapsed = F,
    #       plotlyOutput("rmd_plot_qc_select")
    #     )
    # ))
  )
  
}
