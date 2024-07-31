
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
        shinyWidgets::progressBar(id = "VS_done", value = 0, size = "xs")
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
        shinyWidgets::progressBar(id = "AGoals_done", value = 0, size = "xs")
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
        shinyWidgets::progressBar(id = "EM_done", value = 0, size = "xs")
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
          
          br(),
          var_select_tab()
          
        ),
        
        div(
          id = "AG_box",
          
          br(),
          analysis_goals()
          
        ),
        
        div(
          id = "EM_box",
          
          br(),
          EM_tab()
          
        ),
        
        div(
          id = "MSU_review_selection_box",
          
          br(),
          progress_tab(
            "MSetup",
            plot_choices = c(
              "Expert Mentor Table"
            ),
            done_btn = actionButton("msu_review_done", "Continue to Pre-processing"),
            reset_btn = actionButton("reset_msu", "Revert to start of Model Set-Up")
          ),
            
        )
        
      )
    )
  )
}
