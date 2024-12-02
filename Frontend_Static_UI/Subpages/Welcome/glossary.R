statistical_methods <- c(`Cross Validation`="cross_validation", 
                         `Hyperparameter Tuning`="hyperparameter_tuning", 
                         `Imputation`="imputation",
                         `Reference Peptide Summarization (rrollup)` = "rrollup",
                         `Z-Score Peptide Summarization (zrollup)` = "zrollup",
                         `Quantile Peptide Summarization (qrollup)` = "qrollup",
                         `SPANS` = "spans",
                         `Robust Mahalanobis Distance (RMD) Filter` = "rmd_filter",
                         `Median Normalization` = "median_normalization",
                         `Mean Normaliaztion` = "mean_normalization",
                         `Zero-to-One Normalization` = "zero_one_normalization",
                         `Z-Score Transformation` = "zscore",
                         `Median Absolute Deviation Transformation` = "mad",
                         `Select All Biomolecules` = "all_b",
                         `Select Complete Biomolecules` = "complete_b",
                         `Normalization Subset - Top L Order Statistics (LOS)` = "los",
                         `Normalization Subset - Proportion of Peptides Present (PPP)` = "ppp",
                         `Normalization Subset - Rank-Invariant Biomolecules (RIP)` = "rip",
                         `Normalization Subset - PPP + RIP` = "ppp_rip",
                         `Backtransformation` = "backtransform"
)


glossary_popup <- function(){
  # Modal
  showModal(
    modalDialog(
      size = "l",
      title = "Glossary",
      
      fluidRow(
      tabsetPanel(
        id = "gloss_tabs",
        
        # tabPanel("Current page",
        #          uiOutput("current_page_glossary")
        #          ),
        
        tabPanel("Model information",
                 wellPanel(
                   
                   strong("Click graph for more information"),
                   
                   hr(),
                   
                   div(
                     style = 'height:500px; overflow-y: scroll',
                     
                     br(),
                     
                     fluidRow(
                       map(models_long_name[!models_long_name %in% c("multilasso", "loglasso")], 
                           function(x) 
                         uiOutput(paste0("EM_", x)))
                     )
                     
                   )
                 )
        ),
        
        tabPanel("Statistical methods",
                 wellPanel(
                   strong("Click graph for more information"),
                   hr(),
                   div(
                     style = 'height:500px; overflow-y: scroll',
                     br(),
                     fluidRow(
                       map(statistical_methods, 
                           function(x) {
                             uiOutput(paste0("SM_", x))
                           })
                             
                     )
                     
                   )
                 )
        )
      ))
    )
  )
}