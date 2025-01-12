
## Cloud Data Retrieval
# Create a reactive value to hold AWS-specific objects
AWSobj <- reactiveValues(e_data = NULL, f_data = NULL, e_meta = NULL)

#' @details Store any values passed in the URL
header_params = reactiveValues()

minio_upload_data <- reactiveValues(project_omic = NULL)

## Data holder up until object creation
reactive_dataholder <- reactiveValues(e_data = NULL,
                                      f_data = NULL,
                                      e_meta = NULL)

## Object creation
omicsData <- reactiveValues(obj = NULL,
                            objQC = NULL,
                            objMSU = NULL,
                            objModel = NULL,
                            objPP = NULL,
                            objRM = NULL
)

## Peptide object specific variables
pepQCData <- reactiveValues(
  objQCPro = NULL,
  keep = FALSE,
  transforms_df = NULL
)

## User input storage for report generation
user_inputs <- reactiveValues(
  upload = NULL,
  qc = NULL,
  msu = NULL,
  pp = NULL,
  rm = NULL,
)

hp_inputs <- reactiveValues(
  input_names = list(),
  input_labels = list(),
)

## Determine if selected model is supervised
supervised <- reactive({
  if(is.null(input$skip_ag)) return(FALSE)
  (input$skip_ag && input$pick_model %in% models_supervised) ||
    (!input$skip_ag && input$ag_prompts == "supervised")
})

popup <- reactiveValues()

## Feed forward population

## Auto-remove baddies through filtering steps
auto_remove_na <- function(omicsData){
  id_col <- which(colnames(omicsData$e_data) == get_edata_cname(omicsData))
  remove_edata <- omicsData$e_data[[id_col]][
    apply(is.na(omicsData$e_data[-id_col]), 1, all)]
  if(length(remove_edata) > 0){
    filt <- custom_filter(omicsData, e_data_remove = remove_edata)
    return(applyFilt(filt, omicsData))
  } else return(omicsData)
}

observeEvent(omicsData$obj, {
  omicsData$objQC <- omicsData$obj
})

observeEvent(omicsData$objQC, {
  omicsData$objMSU <- omicsData$objQC
})

observeEvent(omicsData$objMSU, {
  omicsData$objPP <- omicsData$objMSU
})

## Track selected model and response
Model_spec <- reactiveValues(
  model = NULL,
  response = NULL
)

## Track current popup
popup <- reactiveValues()

## VS -- Viewer for reduced data rows and VS page in general
preview_keep_cols <- reactiveValues(result = NULL)


## Downloads -- Plot and table saving
plots <- reactiveValues(plot_table = data.frame("Select a plot" = character(0), 
                                                "Download?" = character(0), 
                                                check.names = F, stringsAsFactors = F))

plot_save_options <- reactiveValues(result = NULL)

# tables of results and other things (intentionally have plot_table in plots$... reactive list)
# +1000 points for variable called tables_table, which is accessed by calling tables$tables_table
tables <- reactiveValues(tables_table = data.frame('Table' = c('val1', 'val2'), 
                                                   'Download?' = dt_checkmark, 
                                                   stringsAsFactors = FALSE, 
                                                   check.names = FALSE))
