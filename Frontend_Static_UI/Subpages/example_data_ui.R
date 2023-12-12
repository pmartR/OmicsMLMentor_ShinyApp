example_data_UI <- function(){
  
  wellPanel(
    
    br(),
    strong("Supported upload files are restricted to .csv files in the following format:"),
    br(),
    
    tags$div(
      tags$ul(
        tags$li(paste0("Count or Abundance data as a cross-tab with rows for each biomolecule",
                       " and columns for each sample. A column is required that",
                       " specifies an unique identifier for each biomolecule",
                       ". The header must contain sample names and designation for",
                       "the column of unique identifiers, typically the biomolecule",
                       " type name (e.g. Metabolite, Lipid, Peptide).")
                ),
        
        tags$li(paste0("Sample data is a cross-tab with rows for each sample",
                       "and columns containing ",
                       "additional information about the each sample such as",
                       " experimental groups or conditions. One column must designate",
                       " a sample in each corresponding row. Sample names are",
                       " expected to be consistent with sample names in the ",
                       "header of Count or Abundance data.")
        ),
        
        tags$li(paste0("Biomolecule information (only required for peptide data",
                       ") is a cross-tab with one column containing the unique",
                       " biomolecule identifiers and additional columns containing",
                       " mappings to other identifiers or metainformation. ",
                       "Biomolecule identifiers are expected to be consistent",
                       " with identifiers in Count or Abundance data.  For ",
                       "proteomics data, this cross-tab contains the peptide-to-protein mapping.")
        )
      )
    ),
    
    br(),
    br(),
    actionButton("template_edata", "Download template Abundance Data", inline = T),
    actionButton("template_fdata", "Download template Sample Data", inline = T),
    actionButton("template_emeta", "Download template Abundance Data", inline = T)
  )
  
  
}