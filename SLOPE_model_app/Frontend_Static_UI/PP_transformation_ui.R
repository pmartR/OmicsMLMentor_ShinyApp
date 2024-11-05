preprocess_UI <- function() {
  div(
    column(
      12,
      collapseBoxGroup(
        id = "preprocess_collapse", multiple = FALSE, open = c("transformation"),
        # biomolecule filters
        collapseBox(
          div(
            "Backfilled",
          ),
          value = "backfill",
          collapsed = F,
          textOutput("transformation_backfill_text"),
          plotlyOutput("transformation_backfill_plot")
        ),
        # molecule filters
        collapseBox(
          div(
            "Transformation",
          ),
          value = "transformation",
          collapsed = F,
          plotlyOutput("transformation_scaling_plot")
          #plotOutput("transformation_norm_plot")
          #uiOutput("molecule_filter_UI"),
          #actionButton("check_processing_steps","Check Pre-Processing Steps"),
          #actionButton("confirm_processing_steps","Confirm Pre-Processing Steps")
        ),
        collapseBox(
          div(
            "Filters",
            radioGroupButtons(
              "apply_filters",
              "Apply Filters Used in Original Model?",
              choices = c("Yes", "No"),
              selected = "No"
            ),
            actionButton("confirm_filters","Confirm")
          ),
          value = "filters",
          collapsed = F
          #actionButton("apply_filters","Apply Filters"),
          #uiOutput("transformation_filters_UI")
        ),
        collapseBox(
          div(
            "Normalization",
          ),
          value = "normalization",
          collapsed = F,
          plotlyOutput("transformation_norm_plot")
          #uiOutput("transformation_normalization_UI")
        ),
        uiOutput("protein_rollup_pp_UI")
      )
    )
)
}
