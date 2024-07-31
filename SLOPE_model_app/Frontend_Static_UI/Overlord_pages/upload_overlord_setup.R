

upload_tab_overlord <- function(){
  div(
    fluidRow(
      pickerInput(
        "data_type",
        "What kind of data do you have?",
        multiple = T,
        choices = ALL_DATATYPE_NAMES,
        selected = character(0),
        options = pickerOptions(maxOptions = 1),
      ),
      div(
        fileInput("upload_edata", label = "Upload New Expression Data"),
        uiOutput("e_data_spec_UI")
      ),
      div(
        fileInput("upload_fdata",label = "Upload Sample Information"),
        uiOutput("f_meta_spec_UI")
      ),
      div(
        fileInput("upload_model", label = "Upload Model (RDS Object)")
      ),
      div(
        actionButton(inputId = "check_selections_upload", 
                     label = "Confirm selections"
        )
      )
    )
  )
}