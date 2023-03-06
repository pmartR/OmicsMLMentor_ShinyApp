# Progress tab;

progress_tab <- function(tabset, ..., plot_choices = c(
                           "Sample Boxplots",
                           "Sample Correlation Heatmap",
                           "Missing Value Sample Bar Plots",
                           "Missing Value Biomolecule Histogram"
                         )) {
  tabPanel("Progress",
    value = sprintf("%s_progress", tabset),
    class = "collapse_page",
    fluidRow( # begin fluidrow

      column( # sidebarpanel
        5,

        # Upload edata
        collapseBoxGroup(
          id = sprintf("%s_collapse_left", tabset),

          collapseBox(
            "Data Summaries",
            collapsed = F,
            value = sprintf("%s_progress_summ", tabset),
            uiOutput(paste0(tabset, "_progress_summ"))
          ),

          collapseBox(
            value = "datselect",
            collapsed = F,
            div(
              "Vizualization Selection",
              hidden(
                div(
                  id = sprintf("%s_ok_datselect", tabset),
                  style = "color:orange;float:right",
                  icon("ok", lib = "glyphicon")
                )
              )
            ),

            pickerInput(sprintf("%s_progress_plot_view", tabset),
              "Select summary visualization to view",
              choices = plot_choices
            )
          )
        ), # parent collapse
        uiOutput(sprintf("%s_progress_engage_UI", tabset))
      ), # column 4

      column(
        7,
        collapseBoxGroup(
          id = sprintf("%s_preview_collapse", tabset),
          ...,

          collapseBox(
            "Summary Visualization",
            collapsed = F,
            value = sprintf("%s_progress_plot", tabset),
            uiOutput(paste0(tabset, "_progress_plot"))
          ),

          collapseBox(
            "User Inputs",
            value = sprintf("%s_progress_inputs", tabset),
            uiOutput(paste0(tabset, "_progress_inputs"))
          )
        ) # Collapse parent
      ) # main_column
    ) # fluidrow
  ) # tabpanel
}
