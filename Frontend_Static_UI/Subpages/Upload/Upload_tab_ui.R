## Allow file uploads for e_meta and e_data

## Upload tab
upload_tab <- function() {

  div(
    fluidRow( # begin fluidrow

      column( # sidebarpanel
        4,

        # Upload edata
        collapseBoxGroup(
          id = "upload_collapse_left",


          collapseBox(
            "Data File Specifications",
            icon_id = "spec_icon",
            icon = icon("exclamation-sign", lib = "glyphicon"),
            value = "datselect",
            collapsed = F,
            
            div(
              id = 'data_type_js',
              pickerInput(
                "data_type",
                "What kind of data do you have?",
                multiple = T,
                choices = ALL_DATATYPE_NAMES,
                selected = character(0),
                options = pickerOptions(maxOptions = 1),
              )
            ),

            uiOutput("data_select_UI"),

            hidden(div(id = "use_example_js",
                       checkboxInput("use_example", "Use example data?"))), 
            

            # div(style="display:inline-block",actionButton("data_type_done", "Done", style="float:right"))
            
            fluidRow(
              column(10, ""),
              disabled(column(2, actionButton("data_type_done", "Done", style="float:right")))
            )


          ),

          ## Upload edata
          uiOutput("e_data_upload_UI"),

          # specify various data info sub-collapse div
          uiOutput("e_data_spec_UI"),


          ## Upload emeta
          uiOutput("e_meta_upload_UI"),

          uiOutput("e_meta_spec_UI"),

          actionButton("upload_done", "Confirm selections"
                       )

        ), # parent collapse
      ), # column 4

      column(
        8,
        collapseBoxGroup(
          id = "upload_preview_collapse",

          div(collapseBox(
            "Data Preview",
            value = "summary_tables",
            collapsed = F,

            tabsetPanel(id = "preview_data")
          )),

          ## Plots and stats here
          uiOutput("detected_box_upload")
          # div(collapseBox(
          #   "Data Properties",
          #   value = "summary",
          #   # uiOutput("Characteristics_module_tabset")
          #   withSpinner(plotlyOutput("boxplot_UI"))#,
          #   # uiOutput("show_log_UI")
          # ))

        ) # Collapse parent
      ) # main_column
    ) # fluidrow
  ) # div
}


