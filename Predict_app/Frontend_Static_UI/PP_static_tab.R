preprocess_UI <- function() {
  div(
    column(
      4,
      
      collapseBoxGroup(
        id = "preprocess_collapse", multiple = FALSE, open = c("filters"),
        # biomolecule filters
        collapseBox(
            "Pre-processing",
            value = "filters",
            collapsed = F,
            
            br(),
            strong(paste0(
              "Pre-processing steps include data transformations",
              " and normalization consistent with the processing used to ",
              "generate the model. Peptide-level data will be quantified at the protein level. "
              
            )),
            
            br(), br(),
              
            strong(paste0("Filters (such as miminum number of occurances of",
              " a specific molecule) can be optionally applied for new data with at",
              " least 5 samples present. These filters will not remove molecules",
              " that the model utilizes, but other removed molecules may have an",
              " effect on normalization or protein quantification. ",
              " Please select your preference below."
              
            )),
            
            br(), br(),
            
            radioGroupButtons(
              "apply_filters",
              "Apply Filters Used in Original Model?",
              choices = c("Yes", "No"),
              selected = "No"
            ),
            
            fluidRow(
              column(10, ""),
              column(2, 
                     actionButton("confirm_filters", "Done", style="float:right"))
            )
        )
      ),
      
      disabled(actionButton("confirm_preprocess", "Continue"))
      
    ),
    
    column(
      8,
      
      
      collapseBoxGroup(
        id = "preprocess_collapse_preview", multiple = FALSE, open = c("plots"),
        # biomolecule filters
        collapseBox(
          "Pre-processing Visualizations",
          value = "plots",
          collapsed = F,
          tabsetPanel(
            id = "plots_preprocess",
            
            shiny::tabPanel(
              "Backfilled",
              br(),
              textOutput("transformation_backfill_text"),
              withSpinner(plotlyOutput("transformation_backfill_plot"))
            ),
            
            shiny::tabPanel(
              "Transformation",
              br(),
              withSpinner(plotlyOutput("transformation_scaling_plot"))
            ),
            
            shiny::tabPanel(
              "Normalization",
              br(),
              withSpinner(plotlyOutput("transformation_norm_plot"))
            )
            
          )
        )
      ) ## Collapse box group
      ) ## column
    ) ## div
}
