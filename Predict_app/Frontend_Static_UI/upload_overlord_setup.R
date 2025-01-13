

upload_tab <- function(){
  div(
    fluidRow(
      
      column(
        4,
        collapseBoxGroup(
          id = "upload_collapse",
          
          uiOutput("model_load_UI"),

          uiOutput("e_data_load_UI"),
          
          uiOutput("e_data_spec_UI"),
          
          uiOutput("f_data_load_UI"),
          
          uiOutput("f_data_spec_UI"),
          
          uiOutput("e_meta_load_UI"),
          
          uiOutput("e_meta_spec_UI")
          
        ),
        
        
        actionButton(inputId = "load_example", 
                              label = "Load example model and data"
        ),
        disabled(actionButton(inputId = "check_selections_upload", 
                     label = "Confirm selections"
        ))
        ),
      
      column(
        8,
        collapseBoxGroup(
          
          collapseBox(
            "Preview Uploads",
            value = "preview_data_box",
            collapsed = F,
            
            tabsetPanel(
              id = "preview_data"
            )
            
          )
          
        )
      )
    )
  )
}
