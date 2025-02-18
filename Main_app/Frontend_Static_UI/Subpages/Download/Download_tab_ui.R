### Enforce filter for stats selection?

# Template for different "stats" tabs for each data type
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
        "Upload",
        icon_id = "ok_download_display",
        icon_style = "color:orange;float:right",
        icon = icon("ok", lib = "glyphicon"),
        value = "download_tabset_upload",
        collapsed = F,
        
        div(
          column(
            6,
                br(),
                wellPanel(DTOutput(height = "350px","download_plot_table_Upload"))
              ),
          column(
            6,
            br(),
            wellPanel(DTOutput(height = "350px","download_table_table_Upload"))
            ),
          
          br(),
          
          div(
            actionButton("upload_dwn_select_all", "Select all"), 
            actionButton("upload_dwn_deselect_all", "Deselect all"), 
            style = "float:left"),
          div(actionButton("upload_dwn_done", "Done"), style = "float:right")
          
        )
        
      ),
      
      collapseBox(
        "Quality Control",
        icon_id = "ok_download_display",
        icon_style = "color:orange;float:right",
        icon = icon("ok", lib = "glyphicon"),
        value = "download_tabset_QC",
        collapsed = T,
        
        div(
          column(
            6,
            br(),
            wellPanel(DTOutput(height = "350px","download_plot_table_QC"))
          ),
          column(
            6,
            br(),
            wellPanel(DTOutput(height = "350px","download_table_table_QC"))
          ),
          
          br(),
          
          div(
            actionButton("QC_dwn_select_all", "Select all"), 
            actionButton("QC_dwn_deselect_all", "Deselect all"), 
            style = "float:left"),
          div(actionButton("QC_dwn_done", "Done"), style = "float:right")
          
        )
        
      ),
      
      collapseBox(
        "Model Set-Up",
        icon_id = "ok_download_display",
        icon_style = "color:orange;float:right",
        icon = icon("ok", lib = "glyphicon"),
        value = "download_tabset_MSU",
        collapsed = T,
        
        div(
          column(
            6,
            br(),
            wellPanel(DTOutput(height = "350px","download_plot_table_MSU"))
          ),
          column(
            6,
            br(),
            wellPanel(DTOutput(height = "350px","download_table_table_MSU"))
          ),
          
          br(),
          
          div(
            actionButton("MSU_dwn_select_all", "Select all"), 
            actionButton("MSU_dwn_deselect_all", "Deselect all"), 
            style = "float:left"),
          div(actionButton("MSU_dwn_done", "Done"), style = "float:right")
          
        )
        
      ),
      
      collapseBox(
        "Pre-processing",
        icon_id = "ok_download_display",
        icon_style = "color:orange;float:right",
        icon = icon("ok", lib = "glyphicon"),
        value = "download_tabset_PP",
        collapsed = T,
        
        div(
          column(
            6,
            br(),
            wellPanel(DTOutput(height = "350px","download_plot_table_PP"))
          ),
          column(
            6,
            br(),
            wellPanel(DTOutput(height = "350px","download_table_table_PP"))
          ),
          
          br(),
          
          div(
            actionButton("PP_dwn_select_all", "Select all"), 
            actionButton("PP_dwn_deselect_all", "Deselect all"), 
            style = "float:left"),
          div(actionButton("PP_dwn_done", "Done"), style = "float:right")
          
        )
        
      ),
      
      collapseBox(
        "Run model",
        icon_id = "ok_download_display",
        icon_style = "color:orange;float:right",
        icon = icon("ok", lib = "glyphicon"),
        value = "download_tabset_RM",
        collapsed = T,
        
        div(
          column(
            6,
            br(),
            wellPanel(DTOutput(height = "350px","download_plot_table_RM"))
          ),
          column(
            6,
            br(),
            wellPanel(DTOutput(height = "350px","download_table_table_RM"))
          ),
          
          br(),
          
          div(
            actionButton("RM_dwn_select_all", "Select all"), 
            actionButton("RM_dwn_deselect_all", "Deselect all"), 
            style = "float:left"),
          div(actionButton("RM_dwn_done", "Done"), style = "float:right")
          
        )
        
      ))),
    
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
    
    column(3,
           checkboxInput(label = "Include report detailing all analysis steps?", 
                         "include_report", value = T),
           
           conditionalPanel("input.include_report == true", {
             textInput("report_name", "Report file name:", 
                       value = paste0("SLOPE_report_", 
                                      gsub("( |:|-)", "_", Sys.time()),
                                      ".html"
                       )
             )
           })
           ),
    
    column(3, 
           uiOutput("include_model_UI"),
           
           conditionalPanel("input.include_model == true", {
             textInput("RDS_name", "RDS file name:", 
                       value = paste0("SLOPE_model_", 
                                      gsub("( |:|-)", "_", Sys.time()),
                                      ".RDS"
                       )
             )
           })
           ),
           
           # div(
             hr(),
             uiOutput("Bundle_button"),
             uiOutput("New_model_button"),
             uiOutput("Download_button"),
           #   style = "float:left;"
           # )

    ))
  )
}
