filter_tab_temp <- function(tabname, keep_missing, user_level) {
  
  # if(!is.null(input$keep_missing)){
  #   # no_mol_filt <- T
  #   no_impute <- T
  # } else if(input$keep_missing == "Yes"){
  #   # no_mol_filt <- F
  #   no_impute <- T
  # } else {
  #   # no_mol_filt <- F
  #   no_impute <- F
  # }
  
  ## Determine which filters are needed
  if(tabname == "RNA-seq"){
    biofilt_UI <- c(
      # "lowvar_mol",
      "molfilt", 
      # "imputefilt",
      # "cvfilt", 
      # "imdanovafilt",
      "customfilt",
      # "profilt" # default excluded
      "TCfilt" # default excluded
    )
    sampfilt_UI <- c(
      # "corr_mol",
      # "lowvar_samp",
      # "rmdfilt",
      "Libfilt",
      # "NZfilt",
      "customfilt"
    )
  } else {
    
    biofilt_UI <- c(
      # "lowvar_mol",
      # "corr_mol",
      "molfilt", 
      "imputefilt",
      "cvfilt", 
      "customfilt",
      "profilt"
    )
    
    if(!(tabname %in% c("Pepdata", "Isobaricpepdata"))){ ## no profilt
      biofilt_UI <- grep("profilt", biofilt_UI, invert = T, value = T)
    }
    
    if(keep_missing){ ## No impute
      biofilt_UI <- grep("imputefilt", biofilt_UI, invert = T, value = T)
    }
    
    # if(no_mol_filt){
    #   biofilt_UI <- grep("molfilt", biofilt_UI, invert = T, value = T)
    # }
    
    
    sampfilt_UI <- c(
      # "lowvar_samp",
      # "rmdfilt",
      # "Libfilt",
      # "NZfilt",
      "customfilt"
    )
    
  }
  
  if(user_level == "beginner"){
    sampfilt_UI <- sampfilt_UI[sampfilt_UI != "customfilt"]
  }
  
  
  ## Page overall UI
  tabPanel("Filters",
           value = paste0(tabname, "_filter"), class = "collapse_page",
           column(
             4,
             collapseBoxGroup(
               id = paste0(tabname, "_filter_collapse"),
               # biomolecule filters
               select_biofilter_UI(tabname, biofilt_UI),
               # sample filters
               if(length(sampfilt_UI) > 0) 
                 select_sampfilter_UI(tabname, sampfilt_UI) else NULL
             ), # ,# parent collapse
             
             actionButton("apply_filters", "Apply selected filters"),
             actionButton("complete_filters", "Confirm selections")
           ), # column 4
           column(
             8,
             collapseBoxGroup(
               id = paste0(tabname, "_filter_plots"),
               collapseBox(
                 "Visualize filters",
                 value = "filter_plots",
                 collapsed = F,
                 br(),
                 tabsetPanel(id = "filter_previews")
                 # br(),
                 # withSpinner(plotOutput(paste0(tabname, "_filter_plots")))
               ) # ,
               # collapseBox('Axes options', value = 'axes_options',
               # collapsed = F,
               #                 HTML('-')
               # uiOutput('filter_plot_options'),
               # uiOutput('filter_apply_style')
               # )
             )
           ) # column 8
  )
}

select_biofilter_UI <- function(
    tabname,
    filters_req ## vector of filters
){
  
  UI_list <- list(
    
    ## correlation filtering
    corr_mol = fluidRow(
      column(
        6,
        tags$b("Biomolecule correlation filter"),
        div(
          id = paste0(tabname, "_add_corSam_filt_ttip_control"),
          prettySwitch(
            inputId = paste0(tabname, "_add_corSam"),
            label = div(
              "Add/Remove",
              hidden(div(
                id = paste0(tabname, "_corSam_exists"),
                style = "color:orange;float:right",
                icon("ok", lib = "glyphicon")
              ))
            ),
            width = "100%"
          ),
          actionButton(inputId = paste0(tabname, "_preview_corSam"), "Preview")
        )
      ),
      column(
        6,
        
        numericInput(paste0(tabname, "_corSam_minvar"), 
                     "Maximum allowable absolute correlation", value = .75, min = 0, max = 1)
      )
    ),
    
    #Low varience filter options
    lowvar_mol = fluidRow(
      column(
        6,
        tags$b("Low Varience Molecule filter"),
        div(
          id = paste0(tabname, "_add_LVmol_filt_ttip_control"),
          prettySwitch(
            inputId = paste0(tabname, "_add_LVmol"),
            label = div(
              "Add/Remove",
              hidden(div(
                id = paste0(tabname, "_LVmol_exists"),
                style = "color:orange;float:right",
                icon("ok", lib = "glyphicon")
              ))
            ),
            width = "100%"
          ),
          actionButton(inputId = paste0(tabname, "_preview_LVmol"), "Preview")
        )
      ),
      column(
        6,
        
        uiOutput(paste0(tabname, "_LVmol_minvar_UI")) ## control max
      )
    ),
    
    #molecule filter options
    molfilt = fluidRow(
      column(
        6,
        tags$b("Molecule filter"),
        div(
          id = paste0(tabname, "_add_molfilt_ttip_control"),
          prettySwitch(
            inputId = paste0(tabname, "_add_molfilt"),
            label = div(
              "Add/Remove",
              hidden(div(
                id = paste0(tabname, "_molfilt_exists"),
                style = "color:orange;float:right",
                icon("ok", lib = "glyphicon")
              ))
            ),
            width = "100%"
          ),
          actionButton(inputId = paste0(tabname, "_preview_molfilt"), "Preview")
        )
      ),
      column(
        6,
        
        uiOutput(paste0(tabname, "_mol_min_num_UI"))
      )
    ),
    
    ## cvfilt options
    cvfilt = fluidRow(
      column(
        6,
        tags$b("CV filter", style="display:block"),
        div(
          id = paste0(tabname, "_add_cvfilt_ttip_control"),
          class = "tooltip-wrapper",
          prettySwitch(inputId = paste0(tabname, "_add_cvfilt"), "Add/Remove", width = "100%")
        ),
        
        br(),
        
        div(
          id = paste0(tabname, "_add_cvfilt_preview_div"),
          class = "tooltip-wrapper",
          actionButton(inputId = paste0(tabname, "_preview_cvfilt"), "Preview")
        )
      ),
      column(
        6,
        numericInput(paste0(tabname, "_cv_threshold"), "Maximum CV", 150, step = 1, min = 0),
        radioGroupButtons(
          inputId = paste0(tabname, "_cvfilt_use_groups"),
          label = "Use groups to calculate CV?",
          choices = c("Yes" = TRUE, "No" = FALSE)
        )
      )
    ),
    
    imputefilt = fluidRow(
      column(
        6,
        tags$b("Biomolecule detection filter"),
        div(
          id = paste0(tabname, "_add_impute_ttip_control"),
          
          uiOutput("add_impute_ui")#,
          # actionButton(inputId = paste0(tabname, "_preview_impute"), "Preview")
        )
      ),
      column(
        6,
        
        uiOutput("missing_options_filter_UI"),
        
        uiOutput("slider_options_filter_ui")
      )
    ),
    
    customfilt = fluidRow(
      column(
        6,
        tags$b("Custom biomolecule filter", style="display:block"),
        div(
          id = paste0(tabname, "_add_edata_customfilt_ttip_control"),
          class = "tooltip-wrapper",
          prettySwitch(
            inputId = paste0(tabname, "_add_edata_customfilt"),
            label = "Add/Remove",
            width = "100%"
          )
        )
        # actionButton(inputId = paste0('%s_preview_edata_customfilt'), "Preview")
      ),
      column(
        6,
        radioGroupButtons(paste0(tabname, "_edata_remove_or_keep"),
                          label = "Remove or keep these choices?",
                          choices = c("Remove", "Keep"),
                          selected = "Remove"),
        uiOutput(paste0(tabname, "_edata_regex"))
      ),
      uiOutput(paste0(tabname, "_edata_filter_preview"))
    ),
    
    protfilt = div(
      id = paste0(tabname, "_profilt_UI"),
      tagList(
        fluidRow(
          column(
            6,
            tags$b("Proteomics filter"),
            prettySwitch(paste0(tabname, "_add_profilt"),
                         label = "Add/Remove",
                         width = "100%"
            ),
            actionButton(inputId = paste0(tabname, "_preview_profilt"), "Preview")
          ),
          column(
            6,
            numericInput(paste0(tabname, "_min_num_peps"), 
                         "Minimum number of peptides mapped to each protein:", 
                         2, step = 1, min = 2),
            checkboxInput(paste0(tabname, "_degen_peps"), 
                          "Remove Degenerate Peptides?", TRUE)
          )
        )
      )
    ),
    
    TCfilt =  div(
      id = paste0(tabname, "_TCfilt_UI"),
      tagList(
        fluidRow(
          column(
            6,
            tags$b("Total Count filter"),
            prettySwitch(paste0(tabname, "_add_TCfilt"),
                         label = "Add/Remove",
                         width = "100%"
            ),
            actionButton(inputId = paste0(tabname, "_preview_TCfilt"), "Preview")
          ),
          column(
            6,
            numericInput(paste0(tabname, "_min_num_trans"), 
                         "Minimum number of RNA-seq counts:", 
                         10, step = 1, min = 0)
          )
        )
      )
    )
  )
  
  selected_filts <- UI_list[filters_req]
  filt_UIs <- list()
  
  ## Add hrs
  for(n in 1:length(selected_filts)){
    filt_UIs[[length(filt_UIs) + 1]] <- selected_filts[[n]]
    if(n != length(selected_filts)){
      filt_UIs[[length(filt_UIs) + 1]] <- hr()
    }
  }
  
  
  collapseBox(div(
    "Biomolecule Filters",
    hidden(div(id = paste0(tabname, "_ok_data_filters"), 
               style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
  ),
  value = "data_filters",
  collapsed = F,
  filt_UIs
  )
  
}

select_sampfilter_UI <- function(
    tabname,
    filters_req ## vector of filters
){
  
  UI_list <-list(
    
    lowvar_samp = fluidRow(
      column(
        6,
        tags$b("Low Varience Sample filter"),
        div(
          id = paste0(tabname, "_add_LVSam_filt_ttip_control"),
          prettySwitch(
            inputId = paste0(tabname, "_add_LVSam"),
            label = div(
              "Add/Remove",
              hidden(div(
                id = paste0(tabname, "_LVSam_exists"),
                style = "color:orange;float:right",
                icon("ok", lib = "glyphicon")
              ))
            ),
            width = "100%"
          ),
          actionButton(inputId = paste0(tabname, "_preview_LVSam"), "Preview")
        )
      ),
      column(
        6,
        
        uiOutput(paste0(tabname, "_LVSam_minvar_UI")) ## control max
      )
    ),
    
    
    rmdfilt = fluidRow(
      column(
        5,
        tags$b("rMd filter", style="display:block"),
        div(
          id = paste0(tabname, "_add_rmdfilt_ttip_control"),
          class = "tooltip-wrapper",
          prettySwitch(
            inputId = paste0(tabname, "_add_rmdfilt"),
            label = "Add/Remove",
            width = "100%"
          ),
          actionButton(inputId = paste0(tabname, "_preview_rmdfilt"), "Preview")
        )
      ),
      column(
        7,
        numericInput(paste0(tabname, "_pvalue_threshold"), "P-value threshold:", 0.0001, step = 0.0001, max = 1, min = 0),
        div(
          id = paste0(tabname, "_rmd_metrics_js"), 
          style = "color:grey",
          class = "inline-wrapper-1",
          uiOutput(paste0(tabname, "_rmd_metrics_UI")),
          uiOutput(paste0(tabname, "_rmd_propmis_warn_icon_UI")),
          hidden(
            div(
              id = paste0(tabname, "_rmd_novariance_warn_icon"),
              icon(
                "exclamation-sign", lib = "glyphicon", 
                style="color:red;display:inline-block"
              )
            )
          )
        ),
        pickerInput(
          paste0(tabname, "_rmdfilt_plot_type"),
          "Plot everything or inspect certain samples?", 
          choices = c("Plot all samples" = "all", 
                      "Select from all samples" = "subset", 
                      "Select from outliers" = "outliers")
        ),
        uiOutput(paste0(tabname, "_rmdfilt_sample_select")),
        radioGroupButtons(paste0(tabname, "_rmdfilt_ignore_singletons"),
                          label = "Ignore singleton groups?",
                          choices = c("Ignore" = TRUE, "Use" = FALSE)
        )
      )
    ),
    
    Libfilt = div(
      id = paste0(tabname, "_Libfilt_UI"),
      tagList(
        fluidRow(
          column(
            6,
            tags$b("Library size filter"),
            prettySwitch(paste0(tabname, "_add_Libfilt"),
                         label = "Add/Remove",
                         width = "100%"
            ),
            actionButton(inputId = paste0(tabname, "_preview_Libfilt"), "Preview")
          ),
          column(
            6,
            numericInput(paste0(tabname, "_min_lib_size"), 
                         "Minimum library size in sample:", 
                         value = 0, step = 1, min = 0)
          )
        ),
        hr()
      )
    ),
    
    NZfilt = div(
      id = paste0(tabname, "_NZfilt_UI"),
      tagList(
        fluidRow(
          column(
            6,
            tags$b("Non-zero observation filter"),
            prettySwitch(paste0(tabname, "_add_NZfilt"),
                         label = "Add/Remove",
                         width = "100%"
            ),
            actionButton(inputId = paste0(tabname, "_preview_NZfilt"), "Preview")
          ),
          column(
            6,numericInput(paste0(tabname, "_min_nonzero"), 
                           "Minimum number of nonzero observations:", 
                           value = 0, step = 1, min = 0)
          )
        ),
        hr()
      )
    ), 
    
    customfilt =  fluidRow(
      column(
        6,
        tags$b("Custom sample filter"),
        prettySwitch(
          inputId = paste0(tabname, "_add_fdata_customfilt"),
          label = "Add/Remove",
          width = "100%"
        )
        # TODO make preview for custom filt
        # actionButton(inputId = paste0('%s_preview_customfilt'), "Preview")
      ),
      column(
        6,
        radioGroupButtons(paste0(tabname, "_remove_or_keep"), 
                          label = "Remove or keep these choices?", 
                          choices = c("Remove", "Keep"), selected = "Remove"),
        uiOutput(paste0(tabname, "_fdata_customfilt")) # ,
        # uiOutput(paste0('%s_fdata_regex'))
      )
    )
    
  )
  
  selected_filts <- UI_list[filters_req]
  filt_UIs <- list()
  
  ## Add hrs
  for(n in 1:length(selected_filts)){
    filt_UIs[[length(filt_UIs) + 1]] <- selected_filts[[n]]
    if(n != length(selected_filts)){
      filt_UIs[[length(filt_UIs) + 1]] <- hr()
    }
  }
  
  # sample filters
  collapseBox("Sample Filters",
              value = "sample_filters",
              filt_UIs
  )
}
