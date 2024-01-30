# Normalization UI tab, applied over all datatypes

# omicsData_objects

norm_tab <- function(tabname) {
  tabPanel("Normalization",
           value = "norm",
           class = "collapse_page",
           column(
             4,
             
             ## Sidebar ##
             collapseBoxGroup(
               id = "normalization_picker",
               
               collapseBox(
                 "Specify Normalization",
                 icon_id = "norm_picker_icon",
                 icon = icon("exclamation-sign", lib = "glyphicon"),
                 value = "picker",
                 collapsed = F,
                 
                 uiOutput(paste0(tabname, "_normalize_option_UI"))
               
             )
             
             ),
             
             uiOutput(paste0(tabname, "_normalize_sidepanel")),

             uiOutput("complete_norm_UI")
           ),
           
           column(
             8,
             
             ## Main Panel ##
             collapseBoxGroup(
               id = "normalization_mainpanel",
               
               collapseBox(
                 "Normalized Data Plots",
                 value = "normdata_mainpanel",
                 collapsed = F,
                 
                 tabsetPanel(
                   id = paste0(tabname, "_normalize_boxplot_tabset"),
                   tabPanel(
                     "Pre-Normalization",
                     br(),
                     withSpinner(plotlyOutput(paste0(tabname, "_normalized_boxplots_pre")))
                   ),
                   # withSpinner(uiOutput(paste0(tabname, '_normalized_boxplots_pre_UI')))),
                   tabPanel(
                     "Normalization Preview",
                     br(),
                     withSpinner(uiOutput(paste0(tabname, "_normalized_boxplots_post_UI")))
                   )
                 )
               )
             ),
             
             uiOutput(paste0(tabname, "_normalize_mainpanel_spans"))
           )
  )
}

## Already normalized

# normed_tab <- function(tabname){
#   
#   # input$normalized
#   
#   tabPanel("Normalization",
#            value = "norm",
#            class = "collapse_page",
#            column(
#              4,
#              
#              ## Sidebar ##
#              collapseBoxGroup(
#                id = "normalization_picker",
#              
#              collapseBox(
#                "Normalization status",
#                icon_id = "norm_picker_icon",
#                icon = icon("exclamation-sign", lib = "glyphicon"),
#                value = "picker",
#                collapsed = F,
#                
#                br(),
#                "Dataset is alrrady normalized, no selections needed for this page."
#                
#              )),
#              
#              actionButton(paste0(tabname, "_complete_norm"), "Confirm Selections")
#            ),
#            
#            column(
#              8,
#              
#              ## Main Panel ##
#              collapseBoxGroup(
#                id = "normalization_mainpanel",
#                
#                collapseBox(
#                  "Normalized Data Plots",
#                  value = "normdata_mainpanel",
#                  collapsed = F,
#                  
#                  tabsetPanel(
#                    id = "normalize_boxplot_tabset",
#                    tabPanel(
#                      "Normalized Data",
#                      br(),
#                      withSpinner(plotlyOutput(paste0(tabname, "_normalized_boxplots_pre")))
#                    )
#                  )
#                )
#              )
#            )
#   )
# }

