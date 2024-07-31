transformation_UI <- function() {
  div(
    column(
      4,
      collapseBoxGroup(
        id = "transform_collapse", multiple = FALSE, open = c("transformation"),
        # biomolecule filters
        collapseBox(
          
          div(
            "Transformation and Scaling",
            hidden(div(id = "ok_data_filters", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
          ),
          value = "transformation",
          collapsed = F,
          uiOutput("transform_picker_UI"),
          
          actionButton("done_tr_box", "Done")
        )
      ), # parent collapse
      
      hidden(actionButton("complete_transform", "Confirm Selections"))
      
    ), # column 4
    column(
      8,
      collapseBoxGroup(
        id = "transform_plots_preview", multiple = TRUE, open = c("transform_preview"),
        collapseBox("Visualize transformation and data scaling",
                    collapsed = F,
                    value = "transform_preview",
                    uiOutput("transform_preview_plot_render")
                    #plotlyOutput("transform_preview_plot")
        )             )
    ) # column 8
  )
}