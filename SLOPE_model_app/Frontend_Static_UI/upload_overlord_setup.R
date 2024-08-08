

upload_tab <- function(){
  div(
    fluidRow(
      
      column(
        4,
        collapseBoxGroup(
          id = "upload_collapse",
          
          collapseBox(
            collapsed = F,
            value = "model_upload",
            "Upload model",
            
            uiOutput("model_upload_UI"),
            
            fluidRow(
              column(10, ""),
              column(2, actionButton("upload_model_done", "Done", style="float:right"))
            )
          ),
          
          
          collapseBox(
            value = "data_upload",
            collapsed = F,
            "Upload data",
            
            # pickerInput(
            #   "data_type",
            #   "What kind of data do you have?",
            #   multiple = T,
            #   choices = ALL_DATATYPE_NAMES,
            #   selected = character(0),
            #   options = pickerOptions(maxOptions = 1),
            # ),
            
            # uiOutput("datatype_note"), ## Must be consistent with model datatype
            
            uiOutput("e_data_upload_UI"),
            prettySwitch(inputId = "use_fdata", "Include Sample Information? (optional)"),
            
            
            conditionalPanel("input.use_fdata", {
              
              uiOutput("how_make_fdata_UI")
              
              # uiOutput("f_data_upload_UI")
            }),
            
            fluidRow(
              column(10, ""),
              column(2, actionButton("upload_data_done", "Done", style="float:right"))
            )
            
          ),
          
          
          uiOutput("e_data_spec_UI"),
          
          uiOutput("f_data_spec_UI"),
          
        ),
        
        actionButton(inputId = "check_selections_upload", 
                     label = "Confirm selections"
        )
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
