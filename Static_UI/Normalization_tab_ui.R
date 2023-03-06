# Normalization UI tab, applied over all datatypes

# omicsData_objects

norm_tab_temp <- function(tabname) {
  tabPanel("Normalization",
           value = "norm",
           class = "collapse_page",
           column(
             4,
             
             ## Sidebar ##
             collapseBoxGroup(
               id = "normalization_picker"),
               
               collapseBox(
                 "Specify Normalization",
                 icon_id = "norm_picker_icon",
                 icon = icon("exclamation-sign", lib = "glyphicon"),
                 value = "picker",
                 collapsed = F,
                 uiOutput("normalize_option_UI")
               
             ),
             
             uiOutput("normalize_sidepanel")
           ),
           
           column(
             8,
             
             ## Main Panel ##
             collapseBoxGroup(
               id = "normalization_mainpanel",
               
               collapseBox(
                 "Normalized Data Plots",
                 value = "normdata_mainpanel",
                 collapsed = F,
                 
                 tabsetPanel(
                   id = "normalize_boxplot_tabset",
                   tabPanel(
                     "Pre-Normalization",
                     br(),
                     withSpinner(plotlyOutput("normalized_boxplots_pre"))
                   ),
                   # withSpinner(uiOutput(paste0(tabname, '_normalized_boxplots_pre_UI')))),
                   tabPanel(
                     "Normalization Preview",
                     br(),
                     withSpinner(uiOutput("normalized_boxplots_post_UI"))
                   )
                 )
               )
             ),
             
             uiOutput("normalize_mainpanel_spans")
           )
  )
}

## Special for RNA-seqomics

norm_tab <- function() {
  
  norm_tab_temp()
  
  # navbarMenu(
  #   "Normalization",
  #   
  #   #### Proteomics ####
  #   
  #   "Proteomics",
  #   
  #   norm_tab_temp("Protein"),
  #   norm_tab_temp("Label-free"),
  #   norm_tab_temp("Isobaric"),
  #   
  #   "---",
  #   
  #   
  #   #### Lipidomics ####
  #   
  #   "Lipidomics",
  #   
  #   norm_tab_temp("Negative"),
  #   norm_tab_temp("Positive"),
  #   # norm_tab_temp('Lipid'),
  #   "---",
  #   
  #   #### Metabolomics ####
  #   
  #   "Metabolomics",
  #   
  #   norm_tab_temp("NMR"),
  #   norm_tab_temp("GC-MS"),
  #   "---",
  #   
  #   #### Transcriptomics ####
  #   
  #   "Transcriptomics",
  #   norm_tab_RNA("RNA-seq"),
  #   "---",
    
    #### Progress ####
    # "Current Status",
    # progress_tab("Normalization", plot_choices = c(
    #   "Sample Boxplots",
    #   "Sample Correlation Heatmap",
    #   "Missing Value Sample Bar Plots",
    #   "Missing Value Biomolecule Histogram",
    #   "Principal Component Analysis (projection pursuit estimation)"
    # ))
    # progress_tab_norm("Normalization")
    #### End Sections ####
  # )
}
