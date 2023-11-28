

upload_tab_overlord <- function(){
  div(
    fluidRow(
      align = "center",
      
      column(1, ""), ## spacer
      
      column(1, 
             actionBttn(
               inputId = "show_exp_upload",
               label = HTML("Experimental<br/>Data"),
               style = "jelly", 
               color = "default",
               size = "s"
             )
      ),
      
      column(
        3,
        br(),
        progressBar(id = "upload_exp_done", value = 0, size = "xs")
      ),
      
      column(1, 
             disabled(actionBttn(
               inputId = "show_sample_upload",
               label = HTML("Sample<br/>Data"),
               style = "jelly", 
               color = "default",
               size = "s"
             ))
      ),
      
      column(
        3,
        br(),
        progressBar(id = "upload_samp_done", value = 0, size = "xs")
      ),
      
      column(1, 
             disabled(actionBttn(
               inputId = "review_upload",
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
          id = "experimental_upload_box",
          
          # box(
          #   # id = "experimental_upload_box",
          #   title = "Upload Experimental Data",
          #   width = 12,
          #   status = "primary",
          #   headerBorder = F
          # ),
          
          br(),
          upload_tab()
          
        ),
        
        div(
          id = "sample_upload_box",
          
          # box(
          #   title = "Upload Sample Data",
          #   # id = "sample_upload_box",
          #   width = 12,
          #   status = "primary",
          #   headerBorder = F
          # ),
          
          br(),
          groups_tab()
          
        ),
        
        div(
          id = "review_upload_box",
          
          # box(
          #   title = "Review Selections",
          #   # id = "review_upload_box",
          #   width = 12,
          #   status = "primary",
          #   headerBorder = F
          # ),
          
          br(),
          progress_tab("Upload"),
          
          actionButton("review_upload_done", "Done")
          
        )
        
      )
    )
  )
}