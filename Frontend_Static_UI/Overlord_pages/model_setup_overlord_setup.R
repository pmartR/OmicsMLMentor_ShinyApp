
Model_setup_tab_overlord <- function(){
  div(
    fluidRow(
      align = "center",
      
      column(1, ""), ## spacer
      
      column(1, 
             actionBttn(
               inputId = "show_VS",
               label = HTML("Variable<br/>Specifications"),
               style = "jelly", 
               color = "default",
               size = "s"
             )
      ),
      
      column(
        2,
        br(),
        progressBar(id = "VS_done", value = 0, size = "xs")
      ),
      
      column(1, 
             disabled(actionBttn(
               inputId = "show_AGoals",
               label = HTML("Analysis<br/>Goals"),
               style = "jelly", 
               color = "default",
               size = "s"
             ))
      ),
      
      column(
        2,
        br(),
        progressBar(id = "AGoals_done", value = 0, size = "xs")
      ),
      
      
      column(1, 
             disabled(actionBttn(
               inputId = "show_EM",
               label = HTML("Expert<br/>Mentor"),
               style = "jelly", 
               color = "default",
               size = "s"
             ))
      ),
      
      column(
        2,
        br(),
        progressBar(id = "EM_done", value = 0, size = "xs")
      ),
      
      column(1, 
             disabled(actionBttn(
               inputId = "review_MSU",
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
          id = "VS_box",
          
          # box(
          #   # id = "experimental_upload_box",
          #   title = "Variable Tracking, Type, and Interactions",
          #   width = 12,
          #   status = "primary",
          #   headerBorder = F
          # ),
          
          br(),
          var_select_tab()
          
        ),
        
        div(
          id = "AG_box",
          
          # box(
          #   # id = "experimental_upload_box",
          #   title = "Define Analysis Goals",
          #   width = 12,
          #   status = "primary",
          #   headerBorder = F
          # ),
          
          br(),
          analysis_goals()
          
        ),
        
        div(
          id = "EM_box",
          
          # box(
          #   # id = "experimental_upload_box",
          #   title = "Expert Mentor",
          #   width = 12,
          #   status = "primary",
          #   headerBorder = F
          # ),
          
          br(),
          EM_tab()
          
        ),
        
        div(
          id = "MSU_review_selection_box",
          
          # box(
          #   # id = "experimental_upload_box",
          #   title = "Review Selections",
          #   width = 12,
          #   status = "primary",
          #   headerBorder = F
          # ),
          
          br(),
          progress_tab("MSetup"),
          
          actionButton("msu_review_done", "Done")
          
        )
        
      )
    )
  )
}