# Progress tab;

progress_tab <- function(tabset, ..., plot_choices = c(
                           "Sample Boxplots",
                           "Sample Correlation Heatmap",
                           "Missing Value Sample Bar Plots",
                           "Missing Value Biomolecule Histogram"
                         ), done_btn = "", reset_btn = "") {
  div(
    fluidRow( # begin fluidrow

      column( # sidebarpanel
        5,

        # Upload edata
        div(
          collapseBoxGroup(
            id = paste0(tabset, "_collapse_left"),
  
            collapseBox(
              "Data Summary",
              collapsed = F,
              value = paste0(tabset, "_progress_summ"),
              uiOutput(paste0(tabset, "_progress_summary"))
            ),
            collapseBox(
              "Next Steps",
              collapsed = F,
              value = paste0(tabset, "_progress_next"),
              uiOutput(paste0(tabset, "_progress_next_steps")),
              done_btn,
              reset_btn
            )
          ) # parent collapse
        )
      ), # column 4

      column(
        7,
        collapseBoxGroup(
          id = paste0(tabset, "_preview_collapse"),
          ...,

          collapseBox(
            "Summary Visualization",
            collapsed = F,
            value = paste0(tabset, "_progress_vis"),
            div(
              style = "width: 100%;",
              uiOutput(paste0(tabset, "_progress_plot")),
              pickerInput(paste0(tabset, "_progress_plot_view"),
                          "Select summary visualization to view",
                          choices = plot_choices)
            )
          ),

          collapseBox(
            "User Inputs",
            value = paste0(tabset, "_progress_inputs"),
            uiOutput(paste0(tabset, "_progress_inputs_list"))
          )
        ) # Collapse parent
      ) # main_column
    ) # fluidrow
  ) # tabpanel
}
