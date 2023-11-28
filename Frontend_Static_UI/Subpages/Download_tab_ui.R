### Enforce filter for stats selection?

# Template for different "stats" tabs for each data type
download_tab <- function(tabname) {
  tabPanel(
    "Download",
    class = "collapse_page",

    collapseBoxGroup(
      id = "download_collapse",
      
      collapseBox(
        "Select items to download",
        icon_id = "ok_download_display",
        icon_style = "color:orange;float:right",
        icon = icon("ok", lib = "glyphicon"),
        value = "download_tabset",
        collapsed = F,

        tabsetPanel(
          tabPanel(
            "Plots",
            fluidRow(
              column(
                6,
                br(),
                DTOutput("download_plot_table")
              ),
              column(1),
              column(
                6,
                br(),
                withSpinner(uiOutput("download_plot_preview_UI")),
                hidden(
                  collapseBoxGroup(
                    id = "download_plot_options_UI",
                    collapseBox(
                      "Axes Options",
                      value = "axes_options",
                      style_UI("download"),
                      div(appButton(inputId = "download_apply_style_plot", label = "Update plot style"))
                    ),
                    collapseBox(
                      "Save Options",
                      value = "save_options",
                      div(
                        uiOutput("download_plot_selected_save_options"),
                        div(class = "inline-wrapper-1",
                           div(id = "download_apply_save_options_tooltip", class = "tooltip-wrapper", actionButton("download_apply_save_options", "Apply")),
                           conditionalPanel("input.download_file_type!='HTML Widget'", actionButton("download_preview_image", "Preview"))
                        ),
                        br(),
                        br(),
                        div(style="overflow:auto", uiOutput("download_image_preview"))
                      )
                    )
                  )
                )
              )
            )
          ),

          # tabPanel(
          #   "Trelliscope Displays",
          #   fluidRow(
          #     column(6,
          #            br(),
          #            DTOutput('download_trelliscope_table')
          #     ),
          #     column(6,
          #            br(),
          #            uiOutput('download_trelliscope_summary')
          #     )
          #   )
          # ),

          tabPanel(
            "Data Files",
            fluidRow(
              column(
                6,
                br(),
                DTOutput("download_table_table")
              ),
              column(
                6,
                br(),
                withSpinner(DTOutput(
                  "download_table_preview" # , style = "font-size: 65%"
                ))
              )
            )
          ),

          # tabPanel(
          #   "SLOPE Processing Checkpoints",
          #   fluidRow(
          #     column(6,
          #            br(),
          #            DTOutput('download_checkpoint_table')
          #            ),
          #     column(6,
          #            br(),
          #            uiOutput('download_checkpoint_summary')
          #            )
          #   )
          # ),

          tabPanel(
            "Processing Report",
            fluidRow(
              column(2),
              column(
                8,
                br(),
                "Functionality coming soon!",
                br(),
                DTOutput("download_report_options")
              ),
              column(2)
            )
          )
        ) # tabset
      ) # collapsepanel
    ), # Parent collapse

    div(
      id = "js_zipbutton",
      style = "float:left",
      class = "grey_button",
      disabled(appButton(inputId = "makezipfile",
        label = tags$b("Bundle up all selected items"),
        icon = icon("briefcase"),
        lib = "glyphicon"
      ))
    ),

    div(
      id = "js_downloadbutton",
      style = "margin-left:4px;float:left",
      class = "grey_button",
      disabled(downloadButton(
        "download_processed_data",
        tags$b("Download bundle")
      ))
    )
  )
}
