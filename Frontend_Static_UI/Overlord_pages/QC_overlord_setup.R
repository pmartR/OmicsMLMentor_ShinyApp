
QC_tab_overlord <- function(){
  div(
    fluidRow(
      align = "center",
      
      column(1, ""), ## spacer
      
      column(1, 
             actionBttn(
               inputId = "show_low_obs",
               label = HTML("Single<br/>Observations"),
               style = "jelly", 
               color = "default",
               size = "s"
             )
      ),
      
      column(
        2,
        br(),
        progressBar(id = "QC_lo_done", value = 0, size = "xs")
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
        2,
        br(),
        progressBar(id = "QC_outlier_done", value = 0, size = "xs")
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
        2,
        br(),
        progressBar(id = "missing_data_done", value = 0, size = "xs")
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
      
      column(1, "") ## spacer
      
    ),
    
    fluidRow(
      
      column(
        12,
        
        br(),
        
        div(
          id = "low_ob_box",
          
          # box(
          #   # id = "experimental_upload_box",
          #   title = "Removal of Single-observation Biomolecules",
          #   width = 12,
          #   status = "primary",
          #   headerBorder = F
          # ),
          
          br(), LQ_mols()
          
        ),
        
        div(
          id = "remove_outlier_box",
          
          # box(
          #   # id = "experimental_upload_box",
          #   title = "Detection and Removal of Outliers",
          #   width = 12,
          #   status = "primary",
          #   headerBorder = F
          # ),
          
          br(), QC_outliers()
          
        ),
        
        div(
          id = "missing_data_box",
          
          # box(
          #   # id = "experimental_upload_box",
          #   title = "Handling Missingness in Data",
          #   width = 12,
          #   status = "primary",
          #   headerBorder = F
          # ),
          
          br(), missing_data()
          
        ),
        
        div(
          id = "QC_review_selection_box",
          
          # box(
          #   # id = "experimental_upload_box",
          #   title = "Review Selections",
          #   width = 12,
          #   status = "primary",
          #   headerBorder = F
          # ),
          
          br(),
          progress_tab("QC"),
          
          actionButton("qc_review_done", "Done")
          
        )
        
      )
    )
  )
}