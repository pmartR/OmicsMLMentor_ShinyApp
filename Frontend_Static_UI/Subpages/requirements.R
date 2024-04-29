
requirements_function <- function(){
  
  wellPanel(
    div(
      style = 'height:500px; overflow-y: scroll',
    paste0(
      "SLOPE requires quantitative ‘omics data to build a model. The following",
      " types of 'omics data are currently supported:"),
    
    br(),
    br(),
    
    h4("Metabolomics"),
    tags$ul(
      tags$li("NMR"),
        tags$ul(
          tags$li("By identified metabolites identified in the spectra"),
          tags$li("By bins of spectra")
        ),
      tags$li("GC-MS")
    ),
    
    br(),
    br(),
    
    h4("Lipidomics"),
    tags$ul(
      tags$li("Single ionization"),
      tags$li("Both negative and positive ionizations")
    ),
    
    br(),
    br(),
    
    h4("Proteomics"),
    tags$ul(
      tags$li("Peptide-level"),
      tags$ul(
        tags$li("Label-free"),
        tags$li("Isobaric labeled")
      ),
      tags$li("Protein-level"),
      tags$ul(
        tags$li("Label-free"),
        tags$li("Isobaric labeled")
      ),
    ),
    
    br(),
    br(),
    
    h4("Transcriptomics"),
    tags$ul(
      tags$li("Transcript-level"),
      tags$li("Gene-level"),
    )

  ))
  
  # includeMarkdown("Frontend_Static_UI/datareqs.md")
}
