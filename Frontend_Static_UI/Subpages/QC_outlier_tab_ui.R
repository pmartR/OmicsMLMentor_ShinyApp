
QC_outliers <- function(){
  
  tabname <- "QC"
  
  div(
  #   "Detect and Remove Outliers",
    
    column(
      3,
      collapseBoxGroup(
        id = "QC_outlier_sidebar",
        # biomolecule filters
        
        collapseBox(
        value = "outlier_QC",
        collapsed = F,
        title = "Robust Mahalanobis distance criteria",
        fluidRow(
          column(
            12,
            div(
              id = paste0( tabname, "_rmd_metrics_js"), 
              style = "color:grey",
              class = "inline-wrapper-1",
              
              uiOutput("outlier_remove_advanced_pval"),
              
              
              ########## Update me laterrr ###############
              uiOutput(paste0( tabname, "_rmd_metrics_UI")),
              uiOutput(paste0( tabname, "_rmd_propmis_warn_icon_UI")),
              hidden(
                div(
                  id = paste0( tabname, "_rmd_novariance_warn_icon"),
                  icon(
                    "exclamation-sign", lib = "glyphicon", 
                    style="color:red;display:inline-block"
                  )
                )
              )
            )
          )
          )
        ),
        
        collapseBox(
          value = "outlier_QC_inspect",
          collapsed = F,
          
          title = "Inspect samples for removal",
          fluidRow(
            column(
              12,
              uiOutput(paste0( tabname, "_rmdfilt_sample_select_UI")),
              
              
              actionButton("all_outs_inspect_out", "Select all outliers for inspection", inline = T),
              actionButton("all_outs_inspect_none", "De-select all", inline = T),
              
              hr(),
              
              ## Remove samples
              uiOutput(paste0( tabname, "_rmdfilt_sample_remove_UI")),
              
              actionButton("all_outs_remove_out", "Select all outliers for removal", inline = T),
              actionButton("all_outs_remove_none", "De-select all", inline = T)
            )
          ))
      ), # ,# parent collapse
      actionButton("outliers_done", "Confirm Selections")
    ),
    
    column(
      8,
      
      tabsetPanel(
        id = "QC_outlier_tabset",
        tabPanel(
          value = "all_out",
          "Visualize all potential outliers",
          plotOutput("rmd_plot_qc_all")
        ),
        
        tabPanel(
          value = "inspect_samp",
          "Inspect samples",
          plotOutput("rmd_plot_qc_select")
        )
        
      )
      # collapseBoxGroup(
      #   id = "QC_outlier_plots",
      #   collapseBox(
      #     "Visualize all potential outliers",
      #     value = "outlier_plots_all",
      #     collapsed = F,
      #     plotOutput("rmd_plot_qc_all")
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
    #       plotOutput("rmd_plot_qc_select")
    #     )
    # ))
  )
  
}
