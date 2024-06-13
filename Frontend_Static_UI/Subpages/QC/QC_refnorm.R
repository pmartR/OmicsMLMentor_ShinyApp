## Reference tab info

upload_reference <- function(tabname) {
  
  ##### Sidebar by Isobaricpepdata #####
  if (tabname == "Isobaricpepdata") {
    sidebar <- column( # sidebarpanel
      4,
      # Upload fdata
      collapseBoxGroup(
        id = "references_collapse_left",
        
        # specify various data info sub-collapse div
        collapseBox(
          "Specify Reference Column and Notation",
          icon_id = "Isobaricpepdata_reference_input_icon",
          icon = icon("exclamation-sign", lib = "glyphicon"),
          value = "columnids",
          collapsed = F,
          
          # uiOutput('Isobaricpepdata_fdata_cname_UI'),
          uiOutput("Isobaricpepdata_ref_group_UI"),
          uiOutput("Isobaricpepdata_ref_col_UI"),
          uiOutput("Isobaricpepdata_ref_notation_UI"),
          uiOutput("Isobaricpepdata_ref_idcols_warning"),
          uiOutput("Isobaricpepdata_ref_done_idcols_UI"),
          uiOutput("Isobaricpepdata_ref_warning"),
          hidden(div(br(),br(),
                     "Normalizing, please wait...", 
                     br(), br(),
                     id = "Isobaricpepdata_ref_busy", class = "fadein-out busy"))
        ) # End collapse panel
      ), # parent collapse
      hidden(actionButton("refnorm_complete", "Confirm selections"))
    ) # column 4
    
    ##### Sidebar by Nmrdata #####
  } else if (tabname == "Nmrdata") {
    sidebar <- column( # sidebarpanel
      4,
      
      # Allow normalization by-pass
      collapseBoxGroup(
        id = "references_collapse_left",
        
        collapseBox(
          "Normalization options",
          icon_id = "Nmrdata_reference_option_icon",
          icon = icon("exclamation-sign", lib = "glyphicon"),
          value = "datselect",
          collapsed = F,
          
          uiOutput("Nmrdata_reference_choice_UI"),
          uiOutput("Nmrdata_reference_source_UI")#,
          # uiOutput("Nmrdata_file_fdata_UI")
        ), # End collapse panel
        
        # specify various data info sub-collapse div
        collapseBox(
          "Specify Reference Identifier",
          icon_id = "Nmrdata_reference_input_icon",
          icon = icon("exclamation-sign", lib = "glyphicon"),
          value = "columnids",
          collapsed = F,
          
          uiOutput("Nmrdata_ref_id_UI"),
          uiOutput("Nmrdata_ref_idcols_warning"),
          uiOutput("Nmrdata_ref_done_idcols_UI"),
          uiOutput("Nmrdata_ref_warning")
        ) # End collapse panel
      ), # parent collapse
      
      hidden(actionButton("refnorm_complete", "Confirm selections"))
    ) # column 4
  } else {
    sidebar <- column( # sidebarpanel
      4,
      
      # Allow normalization by-pass
      collapseBoxGroup(
        id = "references_collapse_left",
        
        collapseBox(
          "Normalization options",
          icon_id = "non_option_icon",
          icon = icon("exclamation-sign", lib = "glyphicon"),
          value = "datselect",
          collapsed = F,
          br(),
          paste0(
            "This page is only available for isobaric labeled peptide data or",
            " NMR data, no further action is required. ",
            "Please select 'Confirm selections' to ",
            "continue. "
            ),
          br()
        )
      ), # parent collapse
      actionButton("refnorm_complete", "Confirm selections")
    ) # column 4
  }
  
  # Main tabpanel ####
  tabPanel(tabname,
           value = paste0(tabname, "_reference"),
           class = "collapse_page",
           fluidRow( # begin fluidrow
             
             sidebar,
             
             column(
               8,
               collapseBoxGroup(
                 id = "upload_preview_collapse",
                 
                 collapseBox(
                   "Data Preview",
                   value = "summary_tables",
                   collapsed = F,
                   
                   tabsetPanel(
                     id = paste0(tabname, "_ref_preview_tables"),
                     tabPanel(
                       "Sample Data File",
                       br(),
                       withSpinner(uiOutput(paste0(tabname, "_ref_head_fdata_UI")))
                     ),
                     tabPanel(
                       paste0("Uploaded Experimental File"),
                       br(),
                       withSpinner(uiOutput(paste0(tabname, "_ref_head_edata_UI")))
                     ),
                     tabPanel(
                       paste0("Reference Normalized Data File"),
                       br(),
                       withSpinner(uiOutput(paste0(tabname, "_norm_edata_UI")))
                     )
                   )
                 ),
                 
                 collapseBox(
                   "Normalized Boxplot Preview",
                   value = "summary_boxplots",
                   
                   tabsetPanel(
                     id = paste0(tabname, "_ref_out_tabset"),
                     tabPanel(
                       id = "prior",
                       "Prior to Reference Normalization",
                       br(),
                       withSpinner(uiOutput(paste0(tabname, "_ref_upload_bp")))
                     ),
                     tabPanel(
                       id = "post",
                       "Reference Normalized",
                       br(),
                       withSpinner(uiOutput(paste0(tabname, "_ref_norm_bp")))
                     )
                   )
                 )
               ) # Collapse parent
             ) # main_column
           ) # fluidrow
  ) # tabpanel
}

refnorm_tab <- function(){
  uiOutput("ref_tab")
}
