
download_tab <- function(tabname) {
  tabPanel(
    "Download",
    class = "collapse_page",
    
    fluidRow(
      column(
        6,
        
        collapseBoxGroup(
          id = "download_collapse_pages",
          
          collapseBox(
            "Download options",
            icon_id = "ok_download_display",
            icon_style = "color:orange;float:right",
            icon = icon("ok", lib = "glyphicon"),
            value = "download_tabset",
            collapsed = F,
            
            div(
              column(
                6,
                br(),
                wellPanel(DTOutput(height = "600px","download_plot_table"))
              ),
              column(
                6,
                br(),
                wellPanel(DTOutput(height = "600px","download_table_table"))
              ),
              
              br(),
              
              div(
                actionButton("dwn_select_all", "Select all"), 
                actionButton("dwn_deselect_all", "Deselect all"), 
                style = "float:left"),
              div(actionButton("dwn_done", "Done"), style = "float:right")
              
            )
            
          ))
          
          ),
      
      column(6,
             
             collapseBoxGroup(
               id = "download_collapse_preview",
               
               collapseBox(
                 "Preview clicked row",
                 icon_id = "o",
                 icon_style = "color:orange;float:right",
                 icon = icon("ok", lib = "glyphicon"),
                 value = "download_tabset_RM_preview",
                 collapsed = F,
                 
                 withSpinner(uiOutput("preview_selected_dwn_UI"))
                 
               )
             )
      ),
      
      br(),
      br(),
      
      column(12,
             
             # column(3,
             #        checkboxInput(label = "Include report detailing all analysis steps?", 
             #                      "include_report", value = T),
             #        
             #        conditionalPanel("input.include_report == true", {
             #          textInput("report_name", "Report file name:", 
             #                    value = paste0("SLOPE_report_", 
             #                                   gsub("( |:|-)", "_", Sys.time()),
             #                                   ".html"
             #                    )
             #          )
             #        })
             # ),
             
             ## Not for MP!
             column(6, 
                    checkboxInput(label = "Include model as an R object (RDS file)?",
                                  "include_model", value = T),
                    
                    conditionalPanel("input.include_model == true", {
                      textInput("RDS_name", "RDS file name:", 
                                value = paste0("SLOPE_model_", 
                                               gsub("( |:|-)", "_", Sys.time()),
                                               ".RDS"
                                )
                      )
                    })
             ),
             
             column(6,
             
             # div(
             hr(),
             uiOutput("Bundle_button"),
             uiOutput("New_model_button"),
             uiOutput("Download_button")
             #   style = "float:left;"
             # )
             
             )
             
      ))
  )
}