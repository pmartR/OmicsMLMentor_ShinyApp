select_biofilter_UI <- function(
    tabname,
    filters_req ## vector of filters
    ){
  
  UI_list <- list(
    
    #molecule filter options
    molfilt = fluidRow(
      column(
        6,
        tags$b("Molecule filter"),
        div(
          id = sprintf("%s_add_molfilt_ttip_control", tabname),
          prettySwitch(
            inputId = sprintf("%s_add_molfilt", tabname),
            label = div(
              "Add/Remove",
              hidden(div(
                id = sprintf("%s_molfilt_exists", tabname),
                style = "color:orange;float:right",
                icon("ok", lib = "glyphicon")
              ))
            ),
            width = "100%"
          ),
          actionButton(inputId = sprintf("%s_preview_molfilt", tabname), "Preview")
        )
      ),
      column(
        6,
        
        uiOutput(sprintf("%s_mol_min_num_UI", tabname))
      )
    ),
    
    ## cvfilt options
    cvfilt = fluidRow(
      column(
        6,
        tags$b("CV filter", style="display:block"),
        div(
          id = sprintf("%s_add_cvfilt_ttip_control", tabname),
          class = "tooltip-wrapper",
          prettySwitch(inputId = sprintf("%s_add_cvfilt", tabname), "Add/Remove", width = "100%"),
          actionButton(inputId = sprintf("%s_preview_cvfilt", tabname), "Preview"),
        )
      ),
      column(
        6,
        numericInput(sprintf("%s_cv_threshold", tabname), "Maximum CV", 150, step = 1, min = 0),
        radioGroupButtons(
          inputId = sprintf("%s_cvfilt_use_groups", tabname),
          label = "Use groups to calculate CV?",
          choices = c("Yes" = TRUE, "No" = FALSE)
        )
      )
    ),
    
    ## imdanova options
    imdanovafilt = fluidRow(
      column(
        6,
        tags$b("imd-ANOVA filter", style="display:block"),
        div(
          id = sprintf("%s_add_imdanovafilt_ttip_control", tabname),
          class = "tooltip-wrapper",
          prettySwitch(
            inputId = sprintf("%s_add_imdanovafilt", tabname),
            label = "Add/Remove",
            width = "100%"
          ),
          actionButton(inputId = sprintf("%s_preview_imdanovafilt", tabname), "Preview")
        )
      ),
      column(6,
             id = sprintf("%s-imd-anova-options", tabname),
             uiOutput(sprintf("%s_min_nonmiss_UI", tabname)),
             radioGroupButtons(sprintf("%s_imdanovafilt_remove_singletons", tabname),
                               label = "Remove singleton groups?",
                               choices = c("Remove" = TRUE, "Keep" = FALSE)
             )
      )
    ),
    
    customfilt = fluidRow(
      column(
        6,
        tags$b("Custom biomolecule filter", style="display:block"),
        div(
          id = sprintf("%s_add_edata_customfilt_ttip_control", tabname),
          class = "tooltip-wrapper",
          prettySwitch(
            inputId = sprintf("%s_add_edata_customfilt", tabname),
            label = "Add/Remove",
            width = "100%"
          )
        )
        # actionButton(inputId = sprintf('%s_preview_edata_customfilt', tabname), "Preview")
      ),
      column(
        6,
        radioGroupButtons(sprintf("%s_edata_remove_or_keep", tabname),
                          label = "Remove or keep these choices?",
                          choices = c("Remove", "Keep"),
                          selected = "Remove"),
        uiOutput(sprintf("%s_edata_regex", tabname))
      ),
      uiOutput(sprintf("%s_edata_filter_preview", tabname))
    ),
    
    protfilt = div(
      id = sprintf("%s_profilt_UI", tabname),
      tagList(
        fluidRow(
          column(
            6,
            tags$b("Proteomics filter"),
            prettySwitch(sprintf("%s_add_profilt", tabname),
                         label = "Add/Remove",
                         width = "100%"
            ),
            actionButton(inputId = sprintf("%s_preview_profilt", tabname), "Preview")
          ),
          column(
            6,
            numericInput(sprintf("%s_min_num_peps", tabname), 
                         "Minimum number of peptides mapped to each protein:", 
                         2, step = 1, min = 2),
            checkboxInput(sprintf("%s_degen_peps", tabname), 
                          "Remove Degenerate Peptides?", TRUE)
          )
        )
      )
    ),
    
    TCfilt =  div(
        id = sprintf("%s_TCfilt_UI", tabname),
        tagList(
          fluidRow(
            column(
              6,
              tags$b("Total Count filter"),
              prettySwitch(sprintf("%s_add_TCfilt", tabname),
                           label = "Add/Remove",
                           width = "100%"
              ),
              actionButton(inputId = sprintf("%s_preview_TCfilt", tabname), "Preview")
            ),
            column(
              6,
              numericInput(sprintf("%s_min_num_trans", tabname), 
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
    hidden(div(id = sprintf("%s_ok_data_filters", tabname), 
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
    rmdfilt = fluidRow(
      column(
        5,
        tags$b("rMd filter", style="display:block"),
        div(
          id = sprintf("%s_add_rmdfilt_ttip_control", tabname),
          class = "tooltip-wrapper",
          prettySwitch(
            inputId = sprintf("%s_add_rmdfilt", tabname),
            label = "Add/Remove",
            width = "100%"
          ),
          actionButton(inputId = sprintf("%s_preview_rmdfilt", tabname), "Preview")
        )
      ),
      column(
        7,
        numericInput(sprintf("%s_pvalue_threshold", tabname), "P-value threshold:", 0.001, step = 0.001, max = 1, min = 0),
        div(
          id = sprintf("%s_rmd_metrics_js", tabname), 
          style = "color:grey",
          class = "inline-wrapper-1",
          uiOutput(sprintf("%s_rmd_metrics_UI", tabname)),
          uiOutput(sprintf("%s_rmd_propmis_warn_icon_UI", tabname)),
          hidden(
            div(
              id = sprintf("%s_rmd_novariance_warn_icon", tabname),
              icon(
                "exclamation-sign", lib = "glyphicon", 
                style="color:red;display:inline-block"
              )
            )
          )
        ),
        pickerInput(
          sprintf("%s_rmdfilt_plot_type", tabname),
          "Plot everything or inspect certain samples?", 
          choices = c("Plot all samples" = "all", 
                      "Select from all samples" = "subset", 
                      "Select from outliers" = "outliers")
          ),
        uiOutput(sprintf("%s_rmdfilt_sample_select", tabname)),
        radioGroupButtons(sprintf("%s_rmdfilt_ignore_singletons", tabname),
                          label = "Ignore singleton groups?",
                          choices = c("Ignore" = TRUE, "Use" = FALSE)
        )
      )
    ),
    
    Libfilt = div(
      id = sprintf("%s_Libfilt_UI", tabname),
      tagList(
        fluidRow(
          column(
            6,
            tags$b("Library size filter"),
            prettySwitch(sprintf("%s_add_Libfilt", tabname),
                         label = "Add/Remove",
                         width = "100%"
            ),
            actionButton(inputId = sprintf("%s_preview_Libfilt", tabname), "Preview")
          ),
          column(
            6,
            numericInput(sprintf("%s_min_lib_size", tabname), 
                         "Minimum library size in sample:", 
                         value = 0, step = 1, min = 0)
          )
        ),
        hr()
      )
    ),
    
    NZfilt = div(
      id = sprintf("%s_NZfilt_UI", tabname),
      tagList(
        fluidRow(
          column(
            6,
            tags$b("Non-zero observation filter"),
            prettySwitch(sprintf("%s_add_NZfilt", tabname),
                         label = "Add/Remove",
                         width = "100%"
            ),
            actionButton(inputId = sprintf("%s_preview_NZfilt", tabname), "Preview")
          ),
          column(
            6,numericInput(sprintf("%s_min_nonzero", tabname), 
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
          inputId = sprintf("%s_add_fdata_customfilt", tabname),
          label = "Add/Remove",
          width = "100%"
        )
        # TODO make preview for custom filt
        # actionButton(inputId = sprintf('%s_preview_customfilt', tabname), "Preview")
      ),
      column(
        6,
        radioGroupButtons(sprintf("%s_remove_or_keep", tabname), 
                          label = "Remove or keep these choices?", 
                          choices = c("Remove", "Keep"), selected = "Remove"),
        uiOutput(sprintf("%s_fdata_customfilt", tabname)) # ,
        # uiOutput(sprintf('%s_fdata_regex', tabname))
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

filter_tab_temp <- function(tabname) {
  
  ## Determine which filters are needed
  if(tabname == "RNA-seq"){
    biofilt_UI <- c("molfilt", 
                 # "cvfilt", 
                 # "imdanovafilt",
                 "customfilt",
                 # "profilt" # default excluded
                 "TCfilt" # default excluded
    )
    sampfilt_UI <- c(
      # "rmdfilt",
      "Libfilt",
      "NZfilt",
      "customfilt"
    )
  } else if(tabname %in% c("Label-free", "Isobaric")){
    biofilt_UI <- c("molfilt", 
                 "cvfilt", 
                 "imdanovafilt",
                 "customfilt",
                 "profilt" # default excluded
                 # "TCfilt" # default excluded
    )
    sampfilt_UI <- c(
      "rmdfilt",
      # "Libfilt",
      # "NZfilt",
      "customfilt"
    )
  } else {
    biofilt_UI <- c("molfilt", 
                 "cvfilt", 
                 "imdanovafilt",
                 "customfilt"
                 # "profilt" # default excluded
                 # "TCfilt" # default excluded
    )
    sampfilt_UI <- c(
      "rmdfilt",
      # "Libfilt",
      # "NZfilt",
      "customfilt"
    )
  }
  
  
  ## Page overall UI
  tabPanel("Filters",
    value = paste0(tabname, "_filter"), class = "collapse_page",
    column(
      4,
      collapseBoxGroup(
        id = sprintf("%s_filter_collapse", tabname),
        # biomolecule filters
        select_biofilter_UI(tabname, biofilt_UI),
        # sample filters
        select_sampfilter_UI(tabname, sampfilt_UI)
      ) # ,# parent collapse
    ), # column 4
    column(
      8,
      collapseBoxGroup(
        id = sprintf("%s_filter_plots", tabname),
        collapseBox(
          "Visualize filters",
          value = "filter_plots",
          collapsed = F,
          uiOutput(sprintf("%s_filter_plots", tabname))
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

filter_tab <- function() {
  filter_tab_temp("")
  
  # navbarMenu(
  #   "Filter",
  #   #### Proteomics ####
  # 
  #   "Proteomics",
  # 
  #   filter_tab_temp("Protein"),
  # 
  #   filter_tab_temp("Label-free"),
  #   filter_tab_temp("Isobaric"),
  # 
  #   "---",
  # 
  # 
  #   #### Lipidomics ####
  # 
  #   "Lipidomics",
  # 
  #   filter_tab_temp("Negative"),
  #   filter_tab_temp("Positive"),
  #   # filter_tab_temp('Lipid'),
  #   "---",
  # 
  #   #### Metabolomics ####
  # 
  #   "Metabolomics",
  # 
  #   filter_tab_temp("NMR"),
  #   filter_tab_temp("GC-MS"),
  # 
  #   "---",
  #   
  #   "Transcriptomics",
  #   
  #   filter_tab_temp("RNA-seq"),
  #   
  #   "---",
  #   
  # 
  #   #### Status After Filters ###
  #   "Current Status",
  #   # progress_tab("Filter",
  #   #   plot_choices = c(
  #   #     "Filter Effects",
  #   #     "Sample Boxplots",
  #   #     "Sample Correlation Heatmap",
  #   #     "Missing Value Sample Bar Plots",
  #   #     "Missing Value Biomolecule Histogram"
  #   #   )
  #   #   # collapseBox("Filter Effects", value = "Filter_effects_barplot",
  #   #   #                 uiOutput("filter_effects_summary_barplots")
  #   #   #                 )
  #   # )
  # 
  #   #### End Sections ####
  # ) # navmenu
}
