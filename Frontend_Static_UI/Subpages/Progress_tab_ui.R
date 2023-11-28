# Progress tab;

progress_tab <- function(tabset, ..., plot_choices = c(
                           "Sample Boxplots",
                           "Sample Correlation Heatmap",
                           "Missing Value Sample Bar Plots",
                           "Missing Value Biomolecule Histogram"
                         )) {
  tabPanel("Progress",
    value = paste0(tabset, "_progress"),
    class = "collapse_page",
    fluidRow( # begin fluidrow

      column( # sidebarpanel
        5,

        # Upload edata
        collapseBoxGroup(
          id = paste0(tabset, "_collapse_left"),

          collapseBox(
            "Data Summaries",
            collapsed = F,
            value = paste0(tabset, "_progress_summ"),
            uiOutput(paste0(tabset, "_progress_summ"))
          ),

          collapseBox(
            value = "datselect",
            collapsed = F,
            div(
              "Vizualization Selection",
              hidden(
                div(
                  id = paste0("_ok_datselect", tabset),
                  style = "color:orange;float:right",
                  icon("ok", lib = "glyphicon")
                )
              )
            ),

            pickerInput(paste0(tabset, "_progress_plot_view"),
              "Select summary visualization to view",
              choices = plot_choices
            )
          )
        ), # parent collapse
        uiOutput(paste0(tabset, "_progress_engage_UI"))
      ), # column 4

      column(
        7,
        collapseBoxGroup(
          id = paste0(tabset, "_preview_collapse"),
          ...,

          collapseBox(
            "Summary Visualization",
            collapsed = F,
            value = paste0(tabset, "_progress_plot"),
            uiOutput(paste0(tabset, "_progress_plot"))
          ),

          collapseBox(
            "User Inputs",
            value = paste0(tabset, "_progress_inputs"),
            uiOutput(paste0(tabset, "_progress_inputs"))
          )
        ) # Collapse parent
      ) # main_column
    ) # fluidrow
  ) # tabpanel
}
