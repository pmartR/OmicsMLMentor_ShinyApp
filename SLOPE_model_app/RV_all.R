
## AWS
# Create a reactive value to hold AWS-specific objects
AWSobj <- reactiveValues(e_data = NULL, f_data = NULL, e_meta = NULL)

## Data holder up until object creation
reactive_dataholder <- reactiveValues(e_data = NULL,
                                      f_data = NULL,
                                      e_meta = NULL)

## Object creation
omicsData <- reactiveValues(obj = NULL,
                            objQC = NULL,
                            objRM = NULL
)

## Peptide object specific variables
pepQCData <- reactiveValues(
  objQCPro = NULL,
  keep = FALSE,
  transforms_df = NULL
)

## Feed forward population

observeEvent(omicsData$obj, {
  omicsData$objQC <- omicsData$obj
})

## Track selected model and response
Model_spec <- reactiveValues(
  model = NULL,
  response = NULL
)


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
