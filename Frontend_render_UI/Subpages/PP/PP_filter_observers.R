
## Top page of filters
observeEvent(input$em_select, once = T, {
  output$filter_page <- renderUI({
    isolate(filter_tab_temp(get_omicsData_type(omicsData$objPP),
                    # F,
                    input$keep_missing == "Yes",
                    input$user_level_pick,
                    attr(omicsData$obj, "data_info")$data_scale_actual
                    ))
  })
})

# # only show the loading screen once on initial load
filter_status <- reactiveValues(loaded = FALSE)

#  filter storage
filters <- reactiveValues()

# will store the last filter created and max cv
filter_flags <- reactiveValues()

# reactive value which gets the number of molecules of filtered before objects are actually filtered.  Observer below populates this reactive value
filter_effects <- reactiveValues()

filter_settings <- reactiveValues()
filter_settings_stored <- reactiveValues()

## get applied filters
apply_filt_flags <- reactive({
  
  req(!is.null(omicsData$objPP))
  front <- get_omicsData_type(omicsData$objPP)
  input_text <- grep(paste0(front, "_add_"), names(input), value = T)
  flags <- map_lgl(input_text, function(x) input[[x]])
  names(flags) <- gsub(paste0(front, "_add_"), "", input_text)
  flags
  
})


output$cv_threshold_UI <- renderUI({
  
  req(!inherits(omicsData$objMSU, "seqData"))
  
  max_cv <- max(cv_filter(omicsData$objMSU)$CV, na.rm = TRUE)
  nm <- get_omicsData_type(omicsData$objMSU)
  
  numericInput(paste0(nm, "_cv_threshold"), 
               "Maximum CV", value = round(min(150, max_cv - 1)), 
               step = 1, min = 0, max = max_cv)
})

## if UI applied filters are different than stored, don't allow to continue until applied
## For more robustness -- check applied filter settings against stored settings
observeEvent(c(apply_filt_flags(), filter_settings_stored$stored), {
  
  name <- get_omicsData_type(omicsData$objPP)
  flags <- apply_filt_flags()
  active_filters <- names(flags[flags])
  active_filters <- gsub("edata|fdata", "", active_filters)
  
  stored_settings <- filter_settings_stored$stored[[name]]
  
  settings_stored <- stored_settings[names(stored_settings) %in% active_filters]
  settings_current <- filter_settings[[name]][names(filter_settings[[name]]) %in% active_filters]
  
  compare <- map_chr(active_filters, function(x)
    switch(x, 
           "cvfilt" = "cvFilt",
           "molfilt" = "moleculeFilt",
            "imputefilt" = "imputationFilt",
           "_customfilt" = "customFilt",
           "totalCountFilt" = "totalCountFilt",
           "NZfilt" = "RNAFilt",
           "Libfilt" = "RNAFilt",
           "profilt" = "proFilt"
           )
    )
  
  ## Include QC filters
  compare_plus <- c(compare, map_chr(attr(omicsData$objPP, "filters"), 1))
  
  cond1 <- !all(compare %in% map_chr(attr(omicsData$objfilters, "filters"), 1))
  
  ## Check if difference is due to silly molecule filt
  
  difference <- setdiff(compare, 
          map_chr(attr(omicsData$objfilters, "filters"), 1)
  )
  
  if(
    cond1 &&
    length(difference) == 1 &&
    difference == "moleculeFilt"){
    
    num <- input[[paste0(name, "_mol_min_num")]]
    df <- molecule_filter(omicsData$objPP)
    if(nrow(df) == nrow(df[df$Num_Observations >= num,])) cond1 <- F
    
  }
  
  if(cond1 ||
     !all(map_chr(attr(omicsData$objfilters, "filters"), 1) %in% compare_plus) ||
     ((length(settings_stored) != 0 || length(settings_current) != 0 ) &&
     !identical(settings_stored, settings_current))
     ){
    disable(id = "complete_filters")
  } else {
    enable(id = "complete_filters")
  }
  
})

# filter name mapping
FNAME_MAP <- list(
  "molfilt" = "Molecule", "cvfilt" = "Coefficient of Variation", 
  # "imdanovafilt" = "iMd-ANOVA", "rmdfilt" = "Mahalanobis Distance", 
  "profilt" = "Proteomics", "fdata_customfilt" = "Custom (Sample)", 
  "edata_customfilt" = "Custom (Biomolecule)", "totalCountFilt" = "Total Count",
  "NZfilt" = "Non-zero", "Libfilt" = "Library size",
  "impute" = "Missing observation handling"
)

### Filter function
#'@details Attempt to make a filter and throw custom error message on failure.
#'
#'@param dataname The type of 'omics-data we are acting on, defined throughout the app
#'@param filter_tag Name used to reference a particular type of filter, see usages of the function in the app.
#'@param message The message to be displayed if filter creation fails.
#'@param func The function that builds the appropriate filter.
#'@param preview Set to true if you want to make the filter regardless of whether the toggleswitch to create that filter is toggled on.
#'@param ... Extra arguments passed to \argument{func}
make_filter <- function(dataname, filter_tag, message, func, settings, preview = FALSE, ...) {

  args <- list(...)
  if (input[[paste0(dataname, "_add_", filter_tag)]] | preview) {
    filters[[dataname]][[filter_tag]] <<- tryCatch(
      {
        do.call(func, c(list(omicsData$objPP), args))
      },
      error = function(e) {
        shinyalert(message, paste0("System error: ", e))
        updatePrettySwitch(session, paste0(dataname, "_add_", filter_tag), value = FALSE)
        return(NULL)
      }
    )
    
    filter_settings[[dataname]][[filter_tag]] <<- settings
    
    if (!grepl("customfilt", filter_tag)) filter_flags[[dataname]]$last_created <<- filter_tag
  } else {
    filters[[dataname]][[filter_tag]] <<- filter_flags[[dataname]]$last_created <<- NULL
    filter_settings[[dataname]][[filter_tag]] <<- NULL
  }
}


#' @details Return filters of a certain type in an object and possibly the sub
#' elements of each filter.
get_filters_ipmart <- function(omicsData, filter_type, element = NULL) {
  filts <- attr(omicsData, "filters")
  if(!is.null(element)) filts[[filter_type]][[element]] else filts[[filter_type]]
  filts[[filter_type]]
}

observeEvent(omicsData$objPP, ignoreNULL = T, {
  filters[[get_omicsData_type(omicsData$objPP)]] <- list(
    imdanovafilt = NULL,
    cvfilt = NULL,
    molfilt = NULL,
    imputefilt = NULL,
    fdata_customfilt = NULL,
    rmdfilt = NULL,
    edata_customfilt = NULL
  )
  
}, once = T)

# 
# # Populate the above reactive value (removed samples or biomolecules)
# # We are doing reactiveValue-observe since we want to keep the value of the reactive at last runtime
observe({
  
  req(!is.null(omicsData$objModel))
  
  # we have not already filtered
  # req(!objs_filtered(), cancelOutput = TRUE)
  
  ### For each datatype, for each filter, we store the removed molecules and samples for that filter
  removed_mols <- list()
  removed_samples <- list()
  classy <- get_omicsData_type(omicsData$objPP)
  
  #
  tryCatch(
    {
      #
      edata_cname <- omicsData$objPP %>% pmartR::get_edata_cname()
      samp_cname <- omicsData$objPP %>% get_fdata_cname()
      groupDF <- attr(omicsData$objPP, "group_DF")
      
      #
      for (fname in names(filters)) {
        filt <- filters[[fname]]
        
        ### EDATA FILTERS ###
        if (inherits(filt, "moleculeFilt")) {
          removed_mols[[fname]] <- filt %>%
            filter(Num_Observations < input[[paste0(classy, "_mol_min_num")]]) %>%
            purrr::pluck(edata_cname)
        }
        else if (inherits(filt, "cvFilt")) {
          removed_mols[[fname]] <- filt %>%
            filter(CV > input[[paste0(classy, "_cv_threshold")]]) %>%
            purrr::pluck(edata_cname)
        }
        else if (inherits(filt, "totalCountFilt")) {
          
          select_bad <- filt$Total_Counts < input[[paste0(classy, "_min_count")]]
          
          removed_mols[[fname]] <- filt[[1]][select_bad]
        }
        else if (inherits(filt, "imdanovaFilt")) {
          min_nonmiss_gtest <- if (!isTruthy(input[[paste0(classy, "_min_nonmiss_gtest")]])) NULL else input[[paste0(classy, "_min_nonmiss_gtest")]]
          min_nonmiss_anova <- if (!isTruthy(input[[paste0(classy, "_min_nonmiss_anova")]])) NULL else input[[paste0(classy, "_min_nonmiss_anova")]]
          
          group_sizes <- attr(filt, "group_sizes")
          nonmiss_per_group <- pmartR:::nonmissing_per_group(omicsData$objPP)
          
          filtered_anova <- filtered_gtest <- NULL
          
          # get molecules filtered by anova and gtest
          if (!is.null(min_nonmiss_anova)) {
            filtered_anova <- pmartR:::anova_filter(nonmiss_per_group = nonmiss_per_group,
                                                    min_nonmiss_anova = min_nonmiss_anova
            )
          }
          if (!is.null(min_nonmiss_gtest)) {
            filtered_gtest <- pmartR:::gtest_filter(
              nonmiss_per_group = nonmiss_per_group,
              min_nonmiss_gtest = min_nonmiss_gtest
            )
          }
          
          # will be the intersect if both are selected, otherwise just the molecules for the selected method (ANOVA/gtest)
          if (!is.null(min_nonmiss_anova) &&!is.null(min_nonmiss_gtest)) {
            removed_mols[[fname]] <- intersect(filtered_anova, filtered_gtest)
          } else {
            if (!is.null(min_nonmiss_anova) &&is.null(min_nonmiss_gtest)) {
              removed_mols[[fname]] <- filtered_anova
            } else {
              if (is.null(min_nonmiss_anova) &&!is.null(min_nonmiss_gtest)) {
                removed_mols[[fname]] <- filtered_gtest
              }
            }
          }
          # imdanova has an option to remove singleton groups, if it is set to true we add the singleton groups to the list of removed samples.
          if (as.logical(
            input[[paste0(classy, "_imdanovafilt_remove_singletons")]]
          )
          ) {
            removed_samples[[fname]] <- groupDF %>%
              filter(Group %in% singleton_groups()$Group) %>%
              purrr::pluck(samp_cname)
          }
        }
        ### SAMPLE FILTERS
        # after removing samples, see if any rows were removed as a result of producing all NA rows
        # also store a list of the samples filtered
        # else if (inherits(filt, "rmdFilt")) {
        #   pvalue_threshold <- input[[paste0(classy, "_pvalue_threshold")]]
        # 
        #   tmp_obj <- omicsData$objPP
        #   tmp_obj <- applyFilt(filt, tmp_obj, pvalue_threshold = pvalue_threshold)
        # 
        #   removed_mols[[fname]] <- setdiff(omicsData$objPP$e_data[, edata_cname], tmp_obj$e_data[, edata_cname])
        #   rm(tmp_obj)
        # 
        #   filtered_samples <- summary(filt, pvalue_threshold = pvalue_threshold)$filtered_samples
        #   removed_samples[[fname]] <- filtered_samples
        # }
        else if (inherits(filt, "customFilt")) {
          # TODO: combine sample and molecule filter into a single filter
          tmp_obj <- omicsData$objPP
          tmp_obj <- applyFilt(filt, tmp_obj)
          
          # fdata filter
          if (!is.null(filt$f_data_remove)) {
            removed_mols[[fname]] <- setdiff(omicsData$objPP$e_data[, edata_cname], tmp_obj$e_data[, edata_cname])
            removed_samples[[fname]] <- filt$f_data_remove
          }
          # edata filter
          else if (!is.null(filt$e_data_remove)) {
            removed_mols[[fname]] <- setdiff(omicsData$objPP$e_data[, edata_cname], tmp_obj$e_data[, edata_cname])
          }
          rm(tmp_obj)
        }
        else if (fname == "NZfilt") {
          
          foo <- summary(filt,
                         min_nonzero = input[[paste0(classy, "_min_nonzero")]]
          )
          
          removed_samples[[fname]] <- foo$num_filtered
          
        }
        else if (fname == "Libfilt") {
          
          foo <- summary(filt,
                         size_library = input[[paste0(classy, "_min_lib_size")]]
          )
          
          removed_samples[[fname]] <- foo$num_filtered
        }
      }
    },
    error = function(e) {
      shinyalert(paste0("System error when creating filter effects preview: ", e))
    }
  )
  
  #
  filter_effects$removed_mols <- removed_mols
  filter_effects$removed_samples <- removed_samples
})


# Whenever we observe object creation, create the observers for the filter tab of that object
observeEvent(omicsData$objPP, {
  
  name <- get_omicsData_type(omicsData$objPP)
  
  # ### Low varience ###
  # observeEvent(input[[paste0("_add_LVmol")]], {
  #   
  #   make_filter(name, "LVmol", "Something went wrong assigning your low varience biomolecule filter.",
  #               LVmol_filt
  #   )
  # })
  # 
  # # ... preview ...
  # observeEvent(input[[paste0("_preview_LVmol")]], {
  #   
  #   make_filter(name, "LVmol", "Something went wrong assigning your low varience biomolecule filter.",
  #               LVmol_filt, preview = TRUE
  #   )
  # })
  # 
  # observeEvent(input[[paste0("_add_LVSam")]], {
  #   
  #   make_filter(name, "LVmol", "Something went wrong assigning your low varience sample filter.",
  #               LVSam_filt
  #   )
  # })
  # 
  # # ... preview ...
  # observeEvent(input[[paste0("_preview_LVSam")]], {
  #   
  #   make_filter(name, "LVmol", "Something went wrong assigning your low varience sample filter.",
  #               LVSam_filt, preview = TRUE
  #   )
  # })
  # 
  # ## correlation ##
  # 
  # observeEvent(input[[paste0("_add_LVSam")]], {
  #   
  #   make_filter(name, "corSam", "Something went wrong assigning your correlation filter.",
  #               cor_result
  #   )
  # })
  # 
  # # ... preview ...
  # observeEvent(input[[paste0("_preview_LVSam")]], {
  #   
  #   make_filter(name, "LVmol", "Something went wrong assigning your correlation filter.",
  #               cor_result, preview = TRUE
  #   )
  # })
  
  
  ### molecule filter ###
  # ...
  
  # ... disable functionality ...
  observeEvent(input[[paste0(name, "_mol_min_num")]], {
    
    req(!is.null(get_group_table(omicsData$objPP)))
    
    # req(!objs_filtered())
    value <- input[[paste0(name, "_mol_min_num")]]
    
    valid_mol_num <- !is.na(!is.na(value)) && !is.null(value) &&
      !(value > max(get_group_table(omicsData$objPP))) &&
      !(value < 2)
    
    valid_mol_num <- isTruthy(valid_mol_num)
    
    # disable switches and add tooltips
    if (!valid_mol_num) updatePrettySwitch(session, paste0(name, "_add_molfilt"), value = FALSE)
    togglestate_add_tooltip(session, paste0(name, "_add_molfilt_ttip_control"),
                            condition = valid_mol_num,
                            tooltip_text = paste0(
                              ttext[["MOL_THRESH_INVALID"]], 2,
                              " and maximum group size (",
                              max(get_group_table(omicsData$objPP)),
                              ")."
                            )
    )
  })
  
  observeEvent(input[[paste0(name, "_add_molfilt")]], {
    # req(!objs_filtered())
    removeTab("filter_previews", target = "molfilt_plot_tab")
    
    make_filter(name, "molfilt", 
                "Something went wrong assigning your molecule filter.", 
                pmartR::molecule_filter,
                settings = list(min_num = input[[paste0(name, "_mol_min_num")]])
    )
    
    # check if molfilt was created
    molfilt_exists <- !is.null(filters[[name]]$molfilt)
    
    if(molfilt_exists){
      appendTab(
        "filter_previews",
        select = TRUE,
        tabPanel(
          title = "Molecule filter",
          value = "molfilt_plot_tab",
          br(),
          withSpinner(plotlyOutput("molfilt_plot"))
        )
      )
    } else {
      removeTab("filter_previews", target = "molfilt_plot_tab")
    }
    
    toggleState(paste0(name, "_mol_min_num"), 
                condition = !molfilt_exists)
    toggleState(paste0(name, "_preview_molfilt"), 
                condition = !input[[paste0(name, "_add_molfilt")]])
    toggleCssClass(paste0(name, "_mol_min_num"), 
                   "grey_text", condition = molfilt_exists)
  })
  
  # ... preview
  observeEvent(input[[paste0(name, "_preview_molfilt")]], {
    make_filter(name, 
                "molfilt", "Something went wrong assigning your molecule filter.", 
                pmartR::molecule_filter, preview = TRUE,
                settings = list(min_num = input[[paste0(name, "_mol_min_num")]])
    )
    removeTab("filter_previews", target = "molfilt_plot_tab")
    
    appendTab(
      "filter_previews",
      select = TRUE,
      tabPanel(
        title = "Molecule filter",
        value = "molfilt_plot_tab",
        br(),
        withSpinner(plotlyOutput("molfilt_plot"))
      )
    )
  })
  
  ### cv filter ###
  observeEvent(input[[paste0(name, "_add_cvfilt")]], {
    # req(!objs_filtered())
    # since cv filter can successfully create a bad filter, we need to clear this beforehand
    filters[[name]][["cvfilt"]] <<- filter_flags[[name]]$last_created <<- NULL
    
    removeTab("filter_previews", target = "cvfilt_plot_tab")
    
    make_filter(name, "cvfilt", "Something went wrong assigning your coefficient of variation filter.",
                pmartR::cv_filter,
                use_groups = isTRUE(as.logical(input[[paste0(name, "_cvfilt_use_groups")]])),
                settings = list(cv_threshold = input[[paste0(name, "_cv_threshold")]])
    )
    
    cvfilt_exists <- !is.null(filters[[name]]$cvfilt)
    
    if(cvfilt_exists){
      appendTab(
        "filter_previews",
        select = TRUE,
        tabPanel(
          title = "Coefficient of variation filter",
          value = "cvfilt_plot_tab",
          br(),
          withSpinner(plotlyOutput("cvfilt_plot"))
        )
      )
    } else {
      removeTab("filter_previews", target = "cvfilt_plot_tab")
    }
    
    toggleState(paste0(name, "_preview_cvfilt"), condition = !input[[paste0(name, "_add_cvfilt")]])
    toggleState(paste0(name, "_cv_threshold"), condition = !cvfilt_exists)
    toggleCssClass(paste0(name, "_cv_threshold"), "grey_text", condition = cvfilt_exists)
    toggleState(paste0(name, "_cvfilt_use_groups"), condition = !cvfilt_exists)
  })
  
  # ... preview ...
  observeEvent(input[[paste0(name, "_preview_cvfilt")]], {
    
    make_filter(name, "cvfilt", "Something went wrong assigning your coefficient of variation filter.",
                pmartR::cv_filter,
                preview = TRUE,
                use_groups = isTRUE(as.logical(input[[paste0(name, "_cvfilt_use_groups")]])),
                settings = list(cv_threshold = input[[paste0(name, "_cv_threshold")]])
    )
    
    removeTab("filter_previews", target = "cvfilt_plot_tab")
    appendTab(
      "filter_previews",
      select = TRUE,
      tabPanel(
        title = "Coefficient of variation filter",
        value = "cvfilt_plot_tab",
        br(),
        withSpinner(plotlyOutput("cvfilt_plot"))
      )
    )
    
  })
  
  # ... disable functionality ...
  observeEvent(input[[paste0(name, "_cv_threshold")]], {
    
    cv_numeric <- input[[paste0(name, "_cv_threshold")]]
    
    # req(!objs_filtered())
    valid_cv <- isTRUE(
      cv_numeric < filter_flags[["max_cv"]][[name]] &
        cv_numeric > 1
    )
    
    # disable switches and add tooltips
    if (!valid_cv) updatePrettySwitch(session, paste0(name, "_add_cvfilt"), value = FALSE)
    togglestate_add_tooltip(session, paste0(name, "_add_cvfilt_ttip_control"), 
                            condition = valid_cv, 
                            tooltip_text = paste0(ttext[["CV_THRESH_TOO_LOW"]], 
                                                  trunc(filter_flags[["max_cv"]][[name]]*1e3)/1e3))
  })
  
  ### proteomics filter if in peptide land ###
  if (name %in% c("Pepdata", "Isobaricpepdata")) {
    observeEvent(input[[paste0(name, "_add_profilt")]], {
      
      removeTab("filter_previews", target = "profilt_plot_tab")
      make_filter(name, "profilt", 
                  "Something went wrong assigning your proteomics filter.", 
                  pmartR::proteomics_filter,
                  settings = list(min_num_peps = input[[paste0(name, "_min_num_peps")]])
      )
      
      profilt_exists <- !is.null(filters[[name]]$profilt)
      
      if(profilt_exists){
        appendTab(
          "filter_previews",
          select = TRUE,
          tabPanel(
            title = "Proteomic filter",
            value = "profilt_plot_tab",
            br(),
            withSpinner(plotlyOutput("profilt_plot"))
          )
        )
      } else {
        removeTab("filter_previews", target = "profilt_plot_tab")
      }
      
      toggleState(paste0(name, "_preview_profilt"), condition = !input[[paste0(name, "_add_profilt")]])
      toggle(paste0(name, "_profilt_exists"), condition = profilt_exists, anim = TRUE)
      toggleState(paste0(name, "_min_num_peps"), condition = !profilt_exists)
      toggleCssClass(paste0(name, "_min_num_peps"), "grey_text", condition = profilt_exists)
      toggleState(paste0(name, "_degen_peps"), condition = !profilt_exists)
      toggleCssClass(paste0(name, "_degen_peps"), condition = profilt_exists)
    })
  }
  # ... preview
  observeEvent(input[[paste0(name, "_preview_profilt")]], {
    make_filter(name, "profilt", 
                "Something went wrong assigning your proteomics filter.", 
                pmartR::proteomics_filter, preview = TRUE,
                settings = list(min_num_peps = input[[paste0(name, "_min_num_peps")]]))
    
    removeTab("filter_previews", target = "profilt_plot_tab")
    appendTab(
      "filter_previews",
      select = TRUE,
      tabPanel(
        title = "Proteomic filter",
        value = "profilt_plot_tab",
        br(),
        withSpinner(plotlyOutput("profilt_plot"))
      )
    )
  })
  
  
  ### Imputation filter ###
  
  imputation_function <- function(omicsData){
    
    thresholds <- list(
      keep = missingHandleSliderValsFilter()$md_keep,
      impute = missingHandleSliderValsFilter()$md_impute,
      convert = missingHandleSliderValsFilter()$md_convert,
      remove = missingHandleSliderValsFilter()$md_remove
    )
    edata_nathresh_transform(as.slData(omicsData), thresholds)
    
  }
  
  observeEvent(ignoreInit = T, input[[paste0(name, "_add_imputefilt")]], {
    
    req(length(input$missing_options_filter) > 0)
    
    removeTab("filter_previews", target = "imputefilt_plot_tab")
    
    make_filter(name, 
                "imputefilt", 
                "Something went wrong assigning your imputation filter.", 
                imputation_function,
                settings = list(
                  keep = missingHandleSliderValsFilter()$md_keep,
                  impute = missingHandleSliderValsFilter()$md_impute,
                  convert = missingHandleSliderValsFilter()$md_convert,
                  remove = missingHandleSliderValsFilter()$md_remove
                )
    )
    
    imputefilt_exists <- !is.null(filters[[name]]$imputefilt)
    
    if(imputefilt_exists){
      appendTab(
        "filter_previews",
        select = TRUE,
        tabPanel(
          title = "Missingness handling filter",
          value = "imputefilt_plot_tab",
          br(),
          uiOutput("imputefilt_plot_render")
        )
      )
    } else {
      removeTab("filter_previews", target = "imputefilt_plot_tab")
    }
    
    toggleState(paste0(name, "_preview_impute"), condition = !input[[paste0(name, "_add_imputefilt")]])
    toggle(paste0(name, "_impute_exists"), condition = imputefilt_exists, anim = TRUE)
    toggleState("missing_options_filter", condition = !imputefilt_exists)
    toggleCssClass("filter_filt_keep", "grey_text", condition = imputefilt_exists)
    toggleCssClass("filter_filt_impute", "grey_text", condition = imputefilt_exists)
    toggleCssClass("filter_filt_convert", "grey_text", condition = imputefilt_exists)
    toggleCssClass("filter_filt_remove", "grey_text", condition = imputefilt_exists)
  })
  
  # ... preview
  observeEvent(ignoreInit = T, input[[paste0(name, "_preview_impute")]], {
    
    req(length(input$missing_options_filter) > 0)
    
    removeTab("filter_previews", target = "imputefilt_plot_tab")

    make_filter(dataname = name,
                filter_tag = "imputefilt",
                message = "Something went wrong assigning your imputation filter.",
                func = imputation_function,
                preview = TRUE,
                settings = list(
                  keep = missingHandleSliderValsFilter()$md_keep,
                  impute = missingHandleSliderValsFilter()$md_impute,
                  convert = missingHandleSliderValsFilter()$md_convert,
                  remove = missingHandleSliderValsFilter()$md_remove
                )
    )
    
    appendTab(
      "filter_previews",
      select = TRUE,
      tabPanel(
        title = "Missingness handling filter",
        value = "imputefilt_plot_tab",
        br(),
        withSpinner(plotlyOutput("imputefilt_plot"))
      )
    )
  })
  
  
  # ### Total count, library, nonzero filter if in RNA-seq land ###
  if (name %in% c("Seqdata")) {
    observeEvent(ignoreInit = T, input[[paste0(name, "_add_totalCountFilt")]], {
      make_filter(name, 
                  "totalCountFilt", 
                  "Something went wrong assigning your proteomics filter.", 
                  pmartR::total_count_filter,
                  settings = list(min_count = input[[paste0(name, "_min_count")]])
      )
      
      removeTab("filter_previews", target = "totalCountFilt_plot_tab")
      
      totalCountFilt_exists <- !is.null(filters[[name]]$totalCountFilt)
      
      if(totalCountFilt_exists){
        appendTab(
          "filter_previews",
          select = TRUE,
          tabPanel(
            title = "Total count filter",
            value = "totalCountFilt_plot_tab",
            br(),
            withSpinner(plotlyOutput("totalCountFilt_plot"))
          )
        )
      } else {
        removeTab("filter_previews", target = "totalCountFilt_plot_tab")
      }
      
      toggleState(paste0(name, "_preview_totalCountFilt"), condition = !input[[paste0(name, "_add_totalCountFilt")]])
      toggle(paste0(name, "_totalCountFilt_exists"), condition = totalCountFilt_exists, anim = TRUE)
      toggleState(paste0(name, "_min_count"), condition = !totalCountFilt_exists)
      toggleCssClass(paste0(name, "_min_count"), "grey_text", condition = totalCountFilt_exists)
    })
    
    # ... preview
    observeEvent(ignoreInit = T, input[[paste0(name, "_preview_totalCountFilt")]], {
      make_filter(dataname = name,
                  filter_tag = "totalCountFilt",
                  message = "Something went wrong assigning your Total Count filter.",
                  func = pmartR::total_count_filter,
                  preview = TRUE,
                  settings = list(min_count = input[[paste0(name, "_min_count")]])
      )
      
      removeTab("filter_previews", target = "totalCountFilt_plot_tab")
      appendTab(
        "filter_previews",
        select = TRUE,
        tabPanel(
          title = "Total count filter",
          value = "totalCountFilt_plot_tab",
          br(),
          withSpinner(plotlyOutput("totalCountFilt_plot"))
        )
      )
    })
    
    # ### Library size filter if in RNA-seq land ###
    observeEvent(ignoreInit = T, input[[paste0(name, "_add_Libfilt")]], {
      
      removeTab("filter_previews", target = "Libfilt_plot_tab")
      make_filter(name, 
                  "Libfilt", 
                  "Something went wrong assigning your library size filter.", 
                  pmartR::RNA_filter,
                  settings = list(size_library = input[[paste0(name, "_min_lib_size")]])
      )
      
      Libfilt_exists <- !is.null(filters[[name]]$Libfilt)
      
      if(Libfilt_exists){
        appendTab(
          "filter_previews",
          select = TRUE,
          tabPanel(
            title = "Library size filter",
            value = "Libfilt_plot_tab",
            br(),
            withSpinner(plotlyOutput("Libfilt_plot"))
          )
        )
      } else {
        removeTab("filter_previews", target = "Libfilt_plot_tab")
      }
      
      toggleState(paste0(name, "_preview_Libfilt"), condition = !input[[paste0(name, "_add_Libfilt")]])
      toggle(paste0(name, "_Libfilt_exists"), condition = Libfilt_exists, anim = TRUE)
      toggleState(paste0(name, "_min_lib_size"), condition = !Libfilt_exists)
      toggleCssClass(paste0(name, "_min_lib_size"), "grey_text", condition = Libfilt_exists)
    })
    
    # ... preview
    observeEvent(ignoreInit = T, input[[paste0(name, "_preview_Libfilt")]], {
      make_filter(name, "Libfilt", "Something went wrong assigning your library size filter.",
                  pmartR::RNA_filter, preview = TRUE,
                  settings = list(size_library = input[[paste0(name, "_min_lib_size")]])
      )
      
      removeTab("filter_previews", target = "Libfilt_plot_tab")
      appendTab(
        "filter_previews",
        select = TRUE,
        tabPanel(
          title = "Library size filter",
          value = "Libfilt_plot_tab",
          br(),
          withSpinner(plotlyOutput("Libfilt_plot"))
        )
      )
    })
    
    
    # ### Nonzero biomolecules filter if in RNA-seq land ###
    observeEvent(ignoreInit = T, input[[paste0(name, "_add_NZfilt")]], {
      make_filter(name, "NZfilt", 
                  "Something went wrong assigning your non-zero observations filter.", 
                  pmartR::RNA_filter,
                  settings = list(min_nonzero = input[[paste0(name, "_min_nonzero")]])
      )
      removeTab("filter_previews", target = "NZfilt_plot_tab")
      
      NZfilt_exists <- !is.null(filters[[name]]$NZfilt)
      
      if(NZfilt_exists){
        appendTab(
          "filter_previews",
          select = TRUE,
          tabPanel(
            title = "Non-zero filter",
            value = "NZfilt_plot_tab",
            br(),
            withSpinner(plotlyOutput("NZfilt_plot"))
          )
        )
      } else {
        removeTab("filter_previews", target = "NZfilt_plot_tab")
      }
      
      toggleState(paste0(name, "_preview_NZfilt"), condition = !input[[paste0(name, "_add_NZfilt")]])
      toggle(paste0(name, "_NZfilt_exists"), condition = NZfilt_exists, anim = TRUE)
      toggleState(paste0(name, "_min_nonzero"), condition = !NZfilt_exists)
      toggleCssClass(paste0(name, "_min_nonzero"), "grey_text", condition = NZfilt_exists)
    })
    
    # ... preview
    observeEvent(ignoreInit = T, input[[paste0(name, "_preview_NZfilt")]], {
      make_filter(name, "NZfilt", "Something went wrong assigning your non-zero observations filter.",
                  pmartR::RNA_filter, preview = TRUE,
                  settings = list(min_nonzero = input[[paste0(name, "_min_nonzero")]])
      )
      
      removeTab("filter_previews", target = "NZfilt_plot_tab")
      appendTab(
        "filter_previews",
        select = TRUE,
        tabPanel(
          title = "Non-zero filter",
          value = "NZfilt_plot_tab",
          br(),
          withSpinner(plotlyOutput("NZfilt_plot"))
        )
      )
    })
  }
  
  ###  filter ###
  observeEvent(input[[paste0(name, "_add_filt")]], {
    make_filter(name, "filt", "Something went wrong assigning your robust Mahalanobis distance filter.",
                pmartR::rmd_filter,
                metrics = input[[paste0(name, "_rmd_metrics")]],
                ignore_singleton_groups = isTRUE(as.logical(
                  input[[paste0(name, "_rmdfilt_ignore_singletons")]]
                )),
                settings = list(pvalue_threshold = input[[paste0(name, "_pvalue_threshold")]])
    )
    removeTab("filter_previews", target = "rmdfilt_plot_tab")
    
    rmdfilt_exists <- !is.null(filters[[name]]$rmdfilt)
    
    if(rmdfilt_exists){
      appendTab(
        "filter_previews",
        select = TRUE,
        tabPanel(
          title = "RMd filter",
          value = "rmdfilt_plot_tab",
          br(),
          withSpinner(plotlyOutput("rmdfilt_plot"))
        )
      )
    } else {
      removeTab("filter_previews", target = "rmdfilt_plot_tab")
    }
    
    #' Check for zero variance metrics, untoggle the switch and show an error
    #' if any are zero variance
    if(rmdfilt_exists){
      #req(!objs_filtered())
      metric_vars = apply(filters[[name]]$rmdfilt[,attr(filters[[name]]$rmdfilt, "metrics")], 2, var)
      cond_zerovars <- any(metric_vars == 0)
      cond_no_missing <- attr(omicsData$objPP, "data_info")$num_miss_obs == 0 &
        "Proportion_Missing" %in% input[[paste0(name, "_rmd_metrics")]]
      cond = cond_zerovars | cond_no_missing
      
      reset_metrics <- if(cond_no_missing) {
        union("Proportion_Missing", names(metric_vars)[which(metric_vars == 0)])
      } else names(metric_vars)[which(metric_vars == 0)]
      reset_metrics <- gsub("Corr$", "Correlation", reset_metrics)
      
      updatePrettySwitch(
        session, paste0(name, "_add_rmdfilt"),
        value = !cond)
      show_add_tooltip(
        session, paste0(name, "_rmd_novariance_warn_icon"),
        condition = cond,
        tooltip_text = paste0(
          ttext[["RMDFILT_NO_VARIANCE"]],
          paste(reset_metrics, collapse = " | ")
        )
      )
      
      # deselect the offending zero-variance selection
      if (cond) {
        new_selections <- setdiff(input[[paste0(name, "_rmd_metrics")]], reset_metrics)
        updatePickerInput(
          session,
          inputId = paste0(name, "_rmd_metrics"),
          selected = new_selections
        )
        
        return()
      }
    }
    
    toggleState(paste0(name, "_preview_rmdfilt"), condition = !input[[paste0(name, "_add_rmdfilt")]])
    toggle(paste0(name, "_rmdfilt_exists"), condition = rmdfilt_exists, anim = TRUE)
    toggleState(paste0(name, "_pvalue_threshold"), condition = !rmdfilt_exists)
    toggleCssClass(paste0(name, "_pvalue_threshold"), "grey_text", condition = rmdfilt_exists)
    toggleState(paste0(name, "_rmd_metrics"), condition = !rmdfilt_exists)
    toggleState(condition = !rmdfilt_exists, selector = paste0("button[data-id='", name, "_rmd_metrics']"))
    toggleCssClass(class = "grey_disabled", condition = rmdfilt_exists, selector = paste0("button[data-id='", name, "_rmd_metrics']"))
    toggleState(paste0(name, "_rmdfilt_ignore_singletons"), condition = !rmdfilt_exists)
  })
  # ... preview
  observeEvent(input[[paste0(name, "_preview_rmdfilt")]], {
    make_filter(name, "rmdfilt", "Something went wrong assigning your robust Mahalanobis distance filter.",
                pmartR::rmd_filter,
                preview = TRUE, metrics = input[[paste0(name, "_rmd_metrics")]],
                ignore_singleton_groups = isTRUE(as.logical(
                  input[[paste0(name, "_rmdfilt_ignore_singletons")]]
                )),
                settings = list(pvalue_threshold = input[[paste0(name, "_pvalue_threshold")]])
    )
    
    removeTab("filter_previews", target = "rmdfilt_plot_tab")
    appendTab(
      "filter_previews",
      select = TRUE,
      tabPanel(
        title = "RMd filter",
        value = "rmdfilt_plot_tab",
        br(),
        withSpinner(plotlyOutput("rmdfilt_plot"))
      )
    )
  })
  # ... disable functionality (must have more than 100 biomolecules in edata)
  observe(
    {
      #req(grepl("_filter$", input$top_page, perl = TRUE), !objs_filtered())
      n_edata <- attributes(omicsData$objPP)[["data_info"]][["num_edata"]]
      cond <- isTRUE(n_edata > 100)
      if (!cond) updatePrettySwitch(session, paste0(name, "_add_rmdfilt"), value = FALSE)
      togglestate_add_tooltip(session, paste0(name, "_add_rmdfilt_ttip_control"),
                              condition = cond,
                              tooltip_text = paste0(ttext[["RMDFILT_INSUF_EDATA"]], n_edata)
      )
    },
    priority = 5
  )
  
  ### biomolecule custom filter ###
  observeEvent(input[[paste0(name, "_add_edata_customfilt")]], {
    
    customfilt_exists <- !is.null(filters[[name]]$edata_customfilt)
    req(isTruthy(input[[paste0(name, "_edata_customfilt_regex")]]))
    
    mols <- omicsData$objPP$e_data[, pmartR::get_edata_cname(omicsData$objPP)]
    
    selected <- input[[paste0(name, "_edata_customfilt_regex")]]
    req(length(selected) > 0)
    
    # get removed edata
    if (input[[paste0(name, "_edata_remove_or_keep")]] == "Remove") {
      mols_rmv <- input[[paste0(name, "_edata_customfilt_regex")]]
    }
    else {
      
      mols_rmv <- setdiff(as.character(mols), input[[paste0(name, "_edata_customfilt_regex")]])
    }
    make_filter(name, 
                "edata_customfilt", 
                "Something went wrong assigning your biomolecule custom filter.",
                pmartR::custom_filter, 
                e_data_remove = as.character(mols_rmv),
                settings = NULL
    )
    
    # if(customfilt_exists){
    #   appendTab(
    #     "filter_previews",
    #     select = TRUE,
    #     tabPanel(
    #       title = "Custom biomolecule filter",
    #       value = "edata_customfilt_plot_tab",
    #       br(),
    #       withSpinner(plotlyOutput("edata_customfilt_plot"))
    #     )
    #   )
    # } else {
    #   removeTab("filter_previews", target = "edata_customfilt_plot_tab")
    # }
    
    # toggleState(paste0(name, "_edata_preview_customfilt"), condition = !input[[paste0(name, "_add_edata_customfilt")]])
    toggle(paste0(name, "_edata_customfilt_exists"), condition = customfilt_exists, anim = TRUE)
    toggleState(paste0(name, "_edata_remove_or_keep"), condition = !customfilt_exists)
    
    ## Are these two lines redundant?
    toggleState(paste0(name, "_edata_customfilt_regex"), condition = !customfilt_exists)
    toggleState(condition = !customfilt_exists, selector = paste0("button[data-id='", name, "_edata_customfilt_regex']"))
    toggleCssClass(class = "grey_disabled", condition = customfilt_exists, selector = paste0("button[data-id='", name, "_edata_customfilt_regex']"))
    
  })
  
  # ... disable functionality (must have something removed)
  observe(
    {
      #req(grepl("_filter$", input$top_page, perl = TRUE), !objs_filtered())
      req(!is.null(input[[paste0(name, "_edata_remove_or_keep")]]) && 
            !is.null(omicsData$objPP))
      
      mols <- omicsData$objPP$e_data[, pmartR::get_edata_cname(omicsData$objPP)]
      
      if (isTruthy(input[[paste0(name, "_edata_customfilt_regex")]])) {
        # inds <- grepl(input[[paste0(name, "_edata_customfilt_regex")]], mols)
        inds <- input[[paste0(name, "_edata_customfilt_regex")]]
      }
      else {
        inds <- numeric(0)
      }
      
      # get removed edata
      if (input[[paste0(name, "_edata_remove_or_keep")]] == "Remove") {
        mols_rmv <- inds
      }
      else {
        mols_rmv <- setdiff(mols, inds)
      }
      
      cond <- length(mols_rmv) > 0
      cond2 <- length(mols_rmv) == length(mols)
      
      if (!cond || cond2){
        updatePrettySwitch(session,
                           paste0(name, "_add_edata_customfilt"),
                           value = FALSE)
      }
      
      togglestate_add_tooltip(session,
                              paste0(name, "_add_edata_customfilt_ttip_control"),
                              condition = cond && !cond2,
                              tooltip_text = c(ttext[["BIOMOL_CUSTOMFILT_NONE_REMOVED"]],
                                               ttext[["BIOMOL_CUSTOMFILT_ALL_REMOVED"]])[c(!cond, cond2)]
      )
      
    },
    priority = 5
  )
  
  ### sample custom filter ###
  observeEvent(input[[paste0(name, "_add_fdata_customfilt")]], {
    
    customfilt_exists <- !is.null(filters[[name]]$fdata_customfilt)
    
    # if(customfilt_exists){
    #   appendTab(
    #     "filter_previews",
    #     select = TRUE,
    #     tabPanel(
    #       title = "Custom sample filter",
    #       value = "fdata_customfilt_plot_tab",
    #       br(),
    #       withSpinner(plotlyOutput("fdata_customfilt_plot"))
    #     )
    #   )
    # } else {
    #   removeTab("filter_previews", target = "fdata_customfilt_plot_tab")
    # }
    
    samples_rmv <- if (input[[paste0(name, "_remove_or_keep")]] == "Remove") input[[paste0(name, "_fdata_customfilt_choices")]] else 
      setdiff(sample_names()[[name]], input[[paste0(name, "_fdata_customfilt_choices")]])
    make_filter(name, 
                "fdata_customfilt", 
                "Something went wrong assigning your custom filter.",
                pmartR::custom_filter, 
                f_data_remove = samples_rmv,
                settings = NULL
    )
    
    # toggleState(paste0(name, "_preview_customfilt"), condition = !input[[paste0(name, "_add_fdata_customfilt")]])
    toggle(paste0(name, "_customfilt_exists"), condition = customfilt_exists, anim = TRUE)
    toggleState(paste0(name, "_fdata_remove_or_keep"), condition = !customfilt_exists)
    
    ## Are these two lines redundant?
    toggleState(paste0(name, "_fdata_customfilt_choices"), condition = !customfilt_exists)
    toggleState(condition = !customfilt_exists, selector = paste0("button[data-id='", name, "_fdata_customfilt_choices']"))
    toggleCssClass(class = "grey_disabled", 
                   condition = customfilt_exists, selector = paste0("button[data-id='", name, "_fdata_customfilt_choices']"))
  })
  
}, once = T)

##### pre-compute max cv filter value for button disabling purposes

## Uses number prior to filtering (cv not affected by transformation step)
observeEvent(omicsData$objMSU, {
  req(!is.null(omicsData$objMSU))
  req(!inherits(omicsData$objMSU, "seqData"))
  
  ## Catch for this is on groups upload
  tryCatch({
    if(!inherits(omicsData$objMSU, "seqData")){
      max_cv <- max(cv_filter(omicsData$objMSU)$CV, na.rm = TRUE)
      nm <- paste(get_omicsData_type(omicsData$objPP), "_cv_threshold")
      filter_flags[["max_cv"]][[get_omicsData_type(omicsData$objPP)]] <- max_cv
    }
  }, error = function(e){""})
},
priority = 10
)



### Apply filters ###
observeEvent(input$apply_filters, ignoreInit = T, ignoreNULL = T, {
  
  name <- get_omicsData_type(omicsData$objPP)
  
  # gather indices in f_data of removed samples from all sample filters
  if (!is.null(omicsData$objPP$f_data)) {
    removed_indices <- NULL
    
    removed_samples <- unlist(filter_effects$removed_samples)
    removed_rows <- which(omicsData$objPP$f_data[, get_fdata_cname(omicsData$objPP)] %in% removed_samples)
    removed_indices <- union(removed_indices, removed_rows)
    
    # dont blow up the data
    if (length(removed_indices) >= nrow(omicsData$objPP$f_data)) error("Combined across all datasets, your sample filters removed all samples.")
  }
  
  # apply all filters in a loop
  tryCatch(
    {
      # make temp objects and clear summaries
      tmp <- omicsData$objPP
      before <- tmp
      biom_rm_count <- tmp
      
      # molecule filter
      if (!is.null(filters[[name]]$molfilt) && 
          is.null(attributes(tmp)$filters$moleculeFilt) && 
          input[[paste0(name, "_add_molfilt")]]) {
        tmp <- applyFilt(filters[[name]]$molfilt, tmp, min_num = input[[paste0(name, "_mol_min_num")]])
        user_inputs$filters$molfilt <- nrow(biom_rm_count$e_data) - nrow(tmp$e_data)
        biom_rm_count <- tmp
      }
      # proteomics filter
      if (inherits(tmp, "pepData")) {
        if (isTRUE(!is.null(filters[[name]]$profilt) &&
                   is.null(attributes(tmp)$filters$proteomicsFilt) &&
                   input[[paste0(name, "_add_profilt")]])) {
          tmp <- applyFilt(filters[[name]]$profilt, tmp, min_num_peps = input[[paste0(name, "_min_num_peps")]], 
                           redundancy = input[[paste0(name, "_degen_peps")]])
          user_inputs$filters$profilt <- nrow(biom_rm_count$e_data) - nrow(tmp$e_data)
          biom_rm_count <- tmp
        }
      }
      
      # impute filter
      if (!is.null(filters[[name]]$imputefilt) &&
          !is.null(input[[paste0(name, "_add_imputefilt")]]) &&
          input[[paste0(name, "_add_imputefilt")]]) {
        
        thresholds <- filter_settings[[name]]$imputefilt
        
        # Only apply impute 
        if (get_omicsData_type(tmp) == "Pepdata") {
          # Impute all of e_data
          imputed_data <- slopeR::imputation(as.slData(tmp))
          imputed_data <- cbind(E_DATA_CNAME = tmp$e_data[[get_edata_cname(tmp)]], imputed_data)
          names(imputed_data)[1] <- get_edata_cname(tmp)
          
          # Get the proteins whose peptides will be imputed
          impute_proteins <- pepQCData$transforms_df[which(pepQCData$transforms_df$Handling == "Estimate"),][[get_edata_cname(pepQCData$objQCPro)]]
          
          # Replace just those peptides with their imputed versions
          impute_pro_idx <- which(tmp$e_meta[[get_emeta_cname(tmp)]] %in% impute_proteins)
          impute_peps <- tmp$e_meta[[get_edata_cname(tmp)]][impute_pro_idx]
          impute_pep_idx <- which(tmp$e_data[[get_edata_cname(tmp)]] %in% impute_peps)
          tmp$e_data[impute_pep_idx,] <- imputed_data[impute_pep_idx,]
        } else {
          tmp <- edata_nathresh_transform(as.slData(tmp), thresholds)
          user_inputs$filters$imputefilt <- nrow(biom_rm_count$e_data) - nrow(tmp$e_data)
          biom_rm_count <- tmp
        }
        
        attr(tmp, "filters") <- c(attr(tmp, "filters"), list(list(type = "imputationFilt")))
        # sldata_temp <- edata_nathresh_transform(as.slData(tmp), thresholds)

        # tmp$e_data <- sldata_temp$e_data
        # tmp$f_data <- sldata_temp$f_data
        # tmp$e_meta <- sldata_temp$e_meta
        
          # Update the data_info attribute.
          # attr(tmp, 'data_info') <- pmartR:::set_data_info(
          #   e_data = tmp$e_data,
          #   edata_cname = get_edata_cname(tmp),
          #   data_scale_orig = get_data_scale_orig(tmp),
          #   data_scale = get_data_scale(tmp),
          #   data_types = dInfo$data_types,
          #   norm_info = dInfo$norm_info,
          #   is_normalized = dInfo$norm_info$is_normalized,
          #   batch_info = dInfo$batch_info,
          #   is_bc = dInfo$batch_info$is_bc
          # )
          # 
      }
      
      # cv filter
      if (!is.null(filters[[name]]$cvfilt) &&
          is.null(attributes(tmp)$filters$cvFilt) &&
          !is.null(input[[paste0(name, "_add_cvfilt")]]) &&
          input[[paste0(name, "_add_cvfilt")]]) {
        
        tmp <- applyFilt(filters[[name]]$cvfilt,
                         tmp,
                         cv_threshold = input[[paste0(name, "_cv_threshold")]]
        )
        user_inputs$filters$cvfilt <- nrow(biom_rm_count$e_data) - nrow(tmp$e_data)
        biom_rm_count <- tmp
      }
      
      # biomolecule custom filter
      if (!is.null(filters[[name]]$edata_customfilt)) {
        tmp <- applyFilt(filters[[name]]$edata_customfilt, tmp)
        user_inputs$filters$customfilt <- nrow(biom_rm_count$e_data) - nrow(tmp$e_data)
        biom_rm_count <- tmp
      }
      
      ## TC filt
      if (!is.null(filters[[name]]$totalCountFilt)) {
        tmp <- applyFilt(filters[[name]]$totalCountFilt, tmp, 
                         min_count = input$Seqdata_min_count)
        user_inputs$filters$tcfilt <- nrow(biom_rm_count$e_data) - nrow(tmp$e_data)
        biom_rm_count <- tmp
      }
      
      #### SAMPLE FILTERS ####
      
      ## filter by union of all samples filtered across all datasets.
      ## i.e if the sample in row 1 of Protein data is filtered, the sample in row 1 of Lipid is also filtered
      
      # construct a custom filter based on union of all filtered rows and apply it
      if (!is.null(omicsData$objPP$f_data) && length(removed_indices) > 0) {
        to_rmv <- omicsData$objPP$f_data[removed_indices, get_fdata_cname(omicsData$objPP)]
        
        tmp_customfilt <- custom_filter(tmp, f_data_remove = to_rmv)
        tmp <- applyFilt(tmp_customfilt, tmp)
      }
      
      if (!is.null(filters[[name]]$Libfilt)) {
        tmp <- applyFilt(filters[[name]]$Libfilt, tmp, 
                         size_library = input$min_lib_size)
      }
      
      if (!is.null(filters[[name]]$NZfilt)) {
        tmp <- applyFilt(filters[[name]]$NZfilt, tmp, 
                         min_nonzero = input$Seqdata_min_nonzero)
      }
      
      omicsData$objfilters <- tmp
      filter_settings_stored$stored <- reactiveValuesToList(filter_settings)
      rm(tmp)
      
      # flag to indicate success
      res <- NULL
    },
    error = function(e) {
      res <<- e
    }
  )
  
  # needs to happen outside trycatch otherwise execution continues to success menu
  if (!is.null(res)) {
    shinyalert("Something went wrong applying your filters:", paste0("System error: ", res))
    return(NULL)
  }
  
  # collapse the filter panels for space to view tables
  updateBoxCollapse(session, "filter_collapse", close = c("data_filters", "sample_filters"))
  
  html(
    html = "Progress",
    selector = 'a[data-value="Filter_progress"]'
  )
  html(
    html = 'Filter <span class="caret"></span>',
    selector = '.dropdown-toggle[data-value="Filter"]'
  )
  
  name <- get_omicsData_type(omicsData$objPP)
  all_filts <- names(filters[[name]])[!map_lgl(filters[[name]], is.null)]
  
  if(length(all_filts) > 0){
    # text summarizing the effect of filters...
    filters_HTML <- FNAME_MAP %>% # global var
      `[`(all_filts) %>%
      unlist() %>%
      paste(collapse = ", ")
    
    filters_HTML <- list(filters_HTML) %>% setNames(name)
  } else {
    filters_HTML <- list()
    filters_HTML[[name]] <- "No filters applied"
  }
  
  if("RNAFilt" %in% all_filts){
    set_attr <- attributes(filters[[name]]$RNAFilt)
    
    add <- HTML(c("Library size", "Non-zero"))
  } else add <- NULL
  
  if(is.null(omicsData$objfilters)) omicsData$objfilters <- omicsData$objPP
  
  output$before_filter_summary <- renderDT({
    
    df <- summary(before)
    df <- df[-1,]
    
    row.names(df) <- gsub("e_data", "Abundance data", row.names(df))
    row.names(df) <- gsub("f_data", "Sample Information", row.names(df))
    row.names(df) <- gsub("e_meta", "Biomolecule data", row.names(df))
    row.names(df) <- gsub("sss \\(", "ss \\(", row.names(df))
    
    df
    
    }, options = list(dom = "tipr"))
  
  output$after_filter_summary <- renderDT({
    
    df <- summary(omicsData$objfilters)
    df <- df[-1,]
    
    row.names(df) <- gsub("e_data", "Abundance data", row.names(df))
    row.names(df) <- gsub("f_data", "Sample Information", row.names(df))
    row.names(df) <- gsub("e_meta", "Biomolecule data", row.names(df))
    row.names(df) <- gsub("sss \\(", "ss \\(", row.names(df))
    
    df
    
    }, options = list(dom = "tipr"))
  
  # Modal
  showModal(
    modalDialog(
      size = "l",
      title = "Filters Applied to Data:",
          div(
            fluidRow(
              column(10,
                     align = "center", offset = 1,
                     HTML(paste0("<h4 style= 'color:#1A5276'>", name, " Data:</h4>")),
                     HTML(filters_HTML[[name]]),
                     add,
                     hr(),
                     splitLayout(
                       div(
                         strong("Before"),
                         DTOutput("before_filter_summary")
                       ),
                       div(
                         strong("After"),
                         DTOutput("after_filter_summary")
                       )
                    ),
                    br(),
                    textOutput("rollup_note_text")
              )
          )
      )
      # fluidRow(
      #   column(10,
      #          align = "center", offset = 1,
      #          actionButton("filter_dismiss", "Stay on this tab", width = "75%", `data-dismiss` = "modal"),
      #          actionButton("goto_norm", "Continue to normalization", style = "margin-top:5px;width:75%")
      #   )
      # )#,
      # footer = NULL
    )
  )
  
  # Tab_Completion_tracking$state[["Filter"]] <- 1
  
  # selecteddata <- names(omicsData$objPP)
  # export_filters <- list()
  # for (export_obj in selecteddata) {
  #   export_filters[[export_obj]] <- attr(omicsData$objPP[[export_obj]], "filters")
  # }
  # exportTestValues(filters = export_filters)
})


# identify whether and which datasets have filter being applied
# observe({
#   names(filters)
#   omicsData$objPP
# 
#   # lapply(names(filters), function(dtype){
#   #   filter_ids = str_extract(names(input), paste0('^%s_add.*filt$', name))
#   #
#   # })
# 
#   has_filters <- list()
# 
#   for (name in names(filters)) {
#     filter_ids <- str_extract(names(input), paste0(name, "^%s_add.*filt$", name))
#     filter_ids <- filter_ids[which(!is.na(filter_ids))]
# 
#     anyselected <- sapply(filter_ids, function(id) {
#       input[[id]]
#     }) %>% any()
# 
#     has_filters[[get_omicsData_type(omicsData$objPP)]] <- anyselected
# 
#     if (!anyselected && (is.null(input$apply_filters) || input$apply_filters < 1)) {
#       tooltip_span <- generate_warning_tooltip(paste0(name, "filter-", name, "-warning-tooltip"))
#       html(
#         html = paste0("<div>", tooltip_span, name, "/div>"),
#         selector = paste0('a[data-value="%s_filter"]', name)
#       )
#     }
#     else {
#       html(html = name, selector = paste0('a[data-value="%s_filter"]', name))
#     }
#     show_add_tooltip(session, paste0(name, "filter-%s-warning-tooltip", name), !anyselected, "No filters selected for this datatype")
#   }
# 
#   if (!any(unlist(has_filters)) && (is.null(input$apply_filters) || input$apply_filters < 1)) {
#     tooltip_span <- generate_warning_tooltip("filter-top-warning-tooltip")
#     html(
#       html = paste0("<div>", tooltip_span, "Filter<span class = 'caret'></span></div>"),
#       selector = '.dropdown-toggle[data-value="Filter"]:not(.disabled)'
#     )
# 
#     tooltip_span <- generate_warning_tooltip("filter-progress-warning-tooltip")
#     html(
#       html = paste0("<div>", tooltip_span, "Progress</div>", tooltip_span),
#       selector = 'a[data-value="Filter_progress"]'
#     )
# 
#     # !any(unlist(has_filters))
#     show_add_tooltip(
#       session, "filter-top-warning-tooltip", is.null(input$apply_filters) || input$apply_filters < 1,
#       "Select filters and confirm selections from the Progress page"
#     )
# 
#     show_add_tooltip(
#       session, "filter-progress-warning-tooltip", is.null(input$apply_filters) || input$apply_filters < 1,
#       "Confirm filter selections"
#     )
#   }
#   # else if(!is.null(input$filter_engage) || input$filter_engage > 0){
#   #   html(html = "Filter<span class = 'caret'></span>", selector = '.dropdown-toggle[data-value="Filter"]:not(.disabled)')
#   # }
# })


output$slider_options_filter_ui <- renderUI({
  
  splitter <- length(input$missing_options_filter)
  req(splitter > 0)
  
  sliders <- list()
  if ("impute" %in% input$missing_options_filter &&
      "convert" %in% input$missing_options_filter)
  {
    sliders <- append(sliders, list(
      list(value = 
             if (!is.null(missingHandleSliderVals()$md_impute[2]))
               missingHandleSliderVals()$md_impute[2]
             else if ("remove" %in% input$missing_options_filter)
               33
             else
               50,
           intentAfter = "warning", intentBefore = "success")
    ))
  }
  if ("convert" %in% input$missing_options_filter &&
      "remove" %in% input$missing_options_filter) {
    sliders <- append(sliders, list(
      list(value = 
             if (!is.null(missingHandleSliderVals()$md_remove[1]))
               missingHandleSliderVals()$md_remove[1]
             else if ("impute" %in% input$missing_options_filter)
               66
             else
               50,
           intentBefore = "warning", intentAfter = "danger")
    ))
  }
  if ("impute" %in% input$missing_options_filter &&
      "remove" %in% input$missing_options_filter &&
      !"convert" %in% input$missing_options_filter) {
    sliders <- append(sliders, list(
      list(value = 
             if (!is.null(missingHandleSliderVals()$md_remove[1]))
               missingHandleSliderVals()$md_remove[1]
             else
               50,
           intentAfter = "danger", intentBefore = "success")
    ))
  }
  
  div(
    column(11,
  MultiSlider.shinyInput(
    "missingness_handle_filter_slider",
    values = sliders,
    min = 0,
    max = 100,
    labelStepSize = 25
  )
  ),
  column(1, "  ")
  )
  
})

# change individual slider values into individual thresholds
missingHandleSliderValsFilter <- reactive({
  thresholds <- list(
    md_keep = NULL,
    md_impute = NULL,
    md_convert = NULL,
    md_remove = NULL
  )
  
  if ("keep" %in% input$missing_options_filter) {
    thresholds$md_keep <- c(0, 100)
    # keep
    return(thresholds)
  }
  
  if ("impute" %in% input$missing_options_filter) {
    if (length(input$missing_options_filter) == 1) {
      thresholds$md_impute <- c(0, 100)
      # impute
      return(thresholds)
    }
    
    thresholds$md_impute <- c(0, input$missingness_handle_filter_slider[1])
    
    if ("convert" %in% input$missing_options_filter) {
      thresholds$md_convert <- c(input$missingness_handle_filter_slider[1], 100)
      
      if ("remove" %in% input$missing_options_filter) {
        thresholds$md_convert[2] <- input$missingness_handle_filter_slider[2]
        thresholds$md_remove <- c(input$missingness_handle_filter_slider[2], 100)
      }
      
      # impute, convert, [remove]
      return(thresholds)
    }
    
    thresholds$md_remove <- c(input$missingness_handle_filter_slider[1], 100)
    # impute, remove
    return(thresholds)
  }
  
  if ("convert" %in% input$missing_options_filter) {
    if (length(input$missing_options_filter) == 1) {
      thresholds$md_convert <- c(0, 100)
      # convert
      return(thresholds)
    }
    
    thresholds$md_convert <- c(0, input$missingness_handle_filter_slider[1])
    thresholds$md_remove <- c(input$missingness_handle_filter_slider[1], 100)
    # convert, remove
    return(thresholds)
  }
  
  if ("remove" %in% input$missing_options_filter) {
    thresholds$md_remove <- c(0, 100)
  }
  
  # none, [remove]
  return(thresholds)
})


output$rollup_note_text <- renderText({
  if(inherits(omicsData$objMSU, "pepData")){
    "Note: Value estimation (imputation) will occur at the protein level."
  } else ""
})

output$missing_options_filter_UI <- renderUI({
  
  all_choices <-  c(
    "Keep data as-is" = "keep",
    "🟩 Estimate values in samples with no biomolecule detection" = "impute",
    "🟧 Convert undetected biomolcule values to 0, all other values to 1" = "convert",
    "🟥 Remove biomolecules with incomplete detection" = "remove"
  )
  
  handles_missing <- map_lgl(
    map(
      algo_rules, 
      function(x) {
        if (is.null(x$hard$any_is_na)) {
          return(TRUE)
        }
        x$hard$any_is_na[[1]]
      }
    ), 
    1
  )
  
  if(!(handles_missing[input$pick_model_EM])){
    all_choices <- all_choices[-1]
  }
  
  pickerInput(
    "missing_options_filter",
    "Handling method:",
    choices = all_choices,
    selected = input$missing_options,
    multiple = T
  )
})



observeEvent(input$em_select, ignoreNULL = T, once = T, {
  name  <- isolate(get_omicsData_type(omicsData$objPP))
  
  # output[[paste0(name, "_filter_plots")]] <- renderPlotly({
  #   
  #   req(!is.null(filter_flags[[name]]$last_created), cancelOutput = T)
  #   req(input$complete_filters == 0, cancelOutput = T)
  #   
  #   filt <- filter_flags[[name]]$last_created
  #   settings <- filter_settings[[name]][[filt]]
  #   
  #   if(filt == "imputefilt"){
  #     
  #     selections <- filters[[name]]$imputefilt
  #     tmp <- omicsData$objPP
  #     
  #     estimate_peps <- selections[selections$Handling == "Estimate", 1]
  #     convert_peps <- selections[selections$Handling == "Convert", 1]
  #     remove_peps <- selections[selections$Handling == "Remove", 1]
  #     
  #     if(length(estimate_peps) > 0){
  #       ## Impute
  #       all_imp <- slopeR::imputation(as.slData(tmp))
  #       impute_rows <- tmp$e_data[[pmartR::get_edata_cname(tmp)]] %in% estimate_peps
  #       if(length(impute_rows) > 0){
  #         tmp$e_data[impute_rows, -1] <- all_imp[impute_rows,]
  #       }
  #     }
  #     
  #     if(length(convert_peps) > 0){
  #     ## Convert
  #     convert_rows <- tmp$e_data[[pmartR::get_edata_cname(tmp)]] %in% convert_peps
  #     if(length(convert_rows) > 0){
  #       tmp$e_data[convert_rows, -1] <- apply(
  #         
  #         ## Logical T/F for presence/absence
  #         Reduce("&", list(
  #           !is.na(tmp$e_data[convert_rows, -1]), ## Normal
  #           tmp$e_data[convert_rows, -1] != 0 ## RNA, shouldn't mess up other types since we convert 0s
  #         )), 
  #         
  #         ## Convert to numeric
  #         2, as.numeric)
  #      }
  #     }
  #     
  #     if(length(remove_peps) > 0){
  #     ## Remove
  #     remove_peps <- tmp$e_data[[pmartR::get_edata_cname(tmp)]] %in% remove_peps
  #     if(length(remove_peps) > 0){
  #       tmp <- applyFilt(custom_filter(tmp, e_data_remove = remove_peps), tmp)
  #     }}
  #     
  #     
  #     if(!is.null(get_group_DF(omicsData$objPP))){
  #       p1 <- plot(omicsData$objPP, color_by = "Group", order_by = "Group") + 
  #         labs(title = "Before handling missingness")
  #       p2 <- plot(tmp, color_by = "Group", order_by = "Group") + 
  #         labs(title = "After handling missingness")
  #     } else {
  #       p1 <- plot(omicsData$objPP) + 
  #         labs(title = "Before handling missingness")
  #       p2 <- plot(tmp) + 
  #         labs(title = "After handling missingness")
  #     }
  #     
  #     wrap_plots(p1, p2, guides = "collect")
  #     # text_ylab <- "biomolecules"
  #     # 
  #     # ggplot(filters[[name]][[filt]], 
  #     #        aes(x = `Percentage missing`, fill = Handling)) +
  #     #   geom_histogram() + theme_bw() + labs(y = paste0("Count of ", text_ylab))
  #   } else if (filt == "cvfilt" ){
  #    
  #     if(settings$cv_threshold < max(filters[[name]][[filt]]$CV, na.rm = T)){
  #       do.call(plot, c(list(filters[[name]][[filt]]),
  #                       settings))
  #     } else {
  #       do.call(plot, list(filters[[name]][[filt]]))
  #     }
  #     
  #   } else {
  #     
  #     do.call(plot, c(list(filters[[name]][[filt]]),
  #                     settings))
  #   }
  #   
  # })
  
  output[[paste0( name, "_mol_min_num_UI")]] <- renderUI({
    
    req(!is.null(omicsData$objPP))
    # req(!objs_filtered(), cancelOutput = T)
    group_sizes <- get_group_table(omicsData$objPP)
    if(is.null(group_sizes)) group_sizes <- nrow(omicsData$objPP$f_data)
    max_x <- max(group_sizes)
    if(is.null(group_sizes)) max_x <- ncol(omicsData$objPP$e_data[-1])
    req(max_x > 1)
    
    numericInput(
      paste0( name, "_mol_min_num"),
      "Minimum number observed",
      2,
      step = 1, min = 2, max = max_x
    )
  })
  
  # regex filter for edata custom filter
  output[[paste0(name, "_edata_regex")]] <- renderUI({
    # req(!objs_filtered(), cancelOutput = T)
    req(!is.null(omicsData$objToFilter))
    
    mols <- as.character(omicsData$objToFilter$e_data[, pmartR::get_edata_cname(omicsData$objToFilter)])
    
    selected <- if(input$user_level_pick != "expert"){
      grep("^con|nant$|con$", tolower(mols), value = T)
    } else {
      NULL
    }
    
    pickerInput(
      paste0(name, "_edata_customfilt_regex"),
      choices = mols,
      multiple = T,
      options = list(`live-search` = TRUE, `actions-box` = TRUE),
      selected = selected
    )
    
    # textInput(paste0(name, "_edata_customfilt_regex"), "Search by text or regex")
  })
  
  # preview of removed samples
  output[[paste0(name, "_edata_filter_preview")]] <- renderUI({
    req(!is.null(omicsData$objPP), isTruthy(input[[paste0(name, "_edata_customfilt_regex")]]))
    
    mols <- omicsData$objPP$e_data[, pmartR::get_edata_cname(omicsData$objPP)]
    selected <- input[[paste0(name, "_edata_customfilt_regex")]]
    
    req(length(selected) > 0)
    # get removed edata
    if (input[[paste0(name, "_edata_remove_or_keep")]] == "Remove") {
      mols_rmv <- selected
    } else {
      mols_rmv <- setdiff(as.character(mols), input[[paste0(name, "_edata_customfilt_regex")]])
    }
    
    preview <- sample(selected, min(length(selected), 5))
    
    tagList(
      tags$p("Some examples of biomolecules selected:", style = "overflow-x:auto"),
      tags$p(paste(preview, collapse = " | "), style = "overflow-x:auto;white-space:nowrap"),
      tags$p(paste0("          Total molecules removed : ", 
                    length(mols_rmv)), style = "overflow-x:auto")
    )
  })
  
  # Sample (fdata) filter options
  output[[paste0(name, "_fdata_customfilt")]] <- renderUI({
    # req(!objs_filtered(), cancelOutput = T)
    req(!is.null(omicsData$objPP))
    fdata_IDS <- as.character(omicsData$objPP$f_data %>%
                                purrr::pluck(get_fdata_cname(omicsData$objPP)))
    
    pickerInput(
      paste0(name, "_fdata_customfilt_choices"),
      "Select Samples",
      choices = fdata_IDS, multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })
  
  # conditional options for rmdfilt if they want to inspect a sample
  output[[paste0(name, "_rmdfilt_sample_select")]] <- renderUI({
    req(!is.null(omicsData$objPP), cancelOutput = TRUE)
    if (input[[paste0(name, "_rmdfilt_plot_type")]] == "all") {
      NULL
    }
    else if (input[[paste0(name, "_rmdfilt_plot_type")]] == "subset") {
      choices <- omicsData$objPP$f_data %>%
        purrr::pluck(get_fdata_cname(omicsData$objPP)) %>%
        as.character()
      
      pickerInput(paste0(name, "_rmd_sample"), "Select samples to inspect:", choices = choices, multiple = FALSE)
    }
    else if (input[[paste0(name, "_rmdfilt_plot_type")]] == "outliers") {
      temp_rmd_filter <- rmd_filter(omicsData$objPP, metrics = input[[paste0(name, "_rmd_metrics")]])
      
      choices <- omicsData$objPP$f_data %>%
        filter(temp_rmd_filter$pvalue < input[[paste0(name, "_pvalue_threshold")]]) %>%
        purrr::pluck(get_fdata_cname(omicsData$objPP)) %>%
        as.character()
      
      pickerInput(paste0(name, "_rmd_sample"), "Select samples to inspect", choices = choices, multiple = FALSE)
    }
  })
  
  
  
  
  output[[paste0(name, "_rmd_metrics_UI")]] <- renderUI({
    
    ### Only let valid options be select-able; errors if filter is made and a constant value exists other than % missing
    map_list <- c("MAD", "Kurtosis", "Skewness", "Corr", "Proportion_Missing")
    alt_notation <-  c("MAD", "Kurtosis", "Skewness", "Correlation", "Proportion_Missing")
    metric_set <- unlist(list(
      "Median Absolute Deviation"="MAD", 
      "Kurtosis", 
      "Skewness", 
      "Correlation", 
      "Proportion Missing" = "Proportion_Missing"
    ))
    metric_set2 <- combn(metric_set, 2, simplify = F)
    
    res_check <- map(metric_set2, function(metric){
      tryCatch({
        rmd_filter(omicsData$objPP, 
                   metrics = metric)
      }, error = function(e){
        print(e)
        browser()
        return(NULL)
      })
      
    })
    
    # Of the returned results, make a list of options that should be disabled
    pairs <- unlist(metric_set2[map_lgl(res_check, is.null)])
    disable_metric <- unique(pairs[duplicated(pairs)])
    
    ok_meterics <- metric_set[!(metric_set %in% disable_metric)]
    map_list <- map_list[!(metric_set %in% disable_metric)]
    
    filter <- rmd_filter(omicsData$objPP, metrics = ok_meterics)
    ok_meterics <- ok_meterics[map_lgl(map_list, function(metric) var(filter[metric]) != 0)]
    metric_filt <- !(metric_set %in% ok_meterics)
    
    ## Set defaults from rmd filt
    if(class(omicsData$objPP) %in% c("lipidData", "metabData")){
      selected <-  c("MAD", "Kurtosis", "Skewness", "Correlation")
      selected <- selected[selected %in% alt_notation[!metric_filt]]
    }
    else {
      selected <-  alt_notation
      selected <- selected[selected %in% alt_notation[!metric_filt]]
    }
    
    # generate pickerinput
    pickerInput(paste0(name, "_rmd_metrics"), 
                div(
                  "Metrics to determine outliers",
                  div(
                    style = "color:deepskyblue;display:inline-block",
                    add_prompt(
                      icon("question-sign", lib = "glyphicon"),
                      message = "Metrics with zero variance are disabled"
                    )
                  )
                ),
                choices = alt_notation,
                selected = selected,
                choicesOpt = list(
                  disabled = metric_filt
                ),
                multiple = TRUE
    )
    
  })
  
  output[[paste0(name, "_rmd_propmis_warn_icon_UI")]] <- renderUI({
    req(input[[paste0(name, "_rmd_metrics")]])
    
    tmp_tooltip = add_prompt(
      div(
        id = paste0(name, "_rmd_propmis_warn_icon"),
        icon(
          "exclamation-sign", lib = "glyphicon", 
          style="color:red;display:inline-block"
        )
      ),
      message = ttext[["RMD_WARN_PROP_MISSING"]]
    )
    
    if(
      !inherits(omicsData$objPP, c("pepData", "proData")) &
      "Proportion_Missing" %in% input[[paste0(name, "_rmd_metrics")]]
    ){
      return(tmp_tooltip)
    } else {
      return(hidden(tmp_tooltip))
    }
  })
  
  output$imputefilt_plot_render <- renderUI({
    if (isTruthy(input$imputefilt_plot_load) || dim(omicsData$objPP$e_data)[1] < 20000) {
      withSpinner(plotlyOutput("imputefilt_plot"))
    } else {
      div(
        "This plot is large and may take a while to render.",
        actionButton("imputefilt_plot_load", "Show plot")
      )
    }
  })
  
})

## Plots for tabpanel
map(c("imputefilt", "NZfilt", "cvfilt", "molfilt", 
      # "fdata_customfilt", "emeta_customfilt", 
      "profilt", "totalCountFilt", "Libfilt", "rmdfilt"
      ), 
    function(filter_tag){
      
      
  output[[paste0(filter_tag, "_plot")]] <- renderPlotly({
    
    tabname <- isolate(get_omicsData_type(omicsData$objPP))
    settings <- filter_settings[[tabname]][[filter_tag]]
    filter <- filters[[tabname]][[filter_tag]]
    isolate(table_table_current$table[[paste0("PP__filters__", filter_tag)]] <- filter$e_data)
    isolate(table_table_current$names[[paste0("PP__filters__", filter_tag)]] <- paste0("Filter: ", filter_tag))
    
    req(!is.null(filter))
    
    if(filter_tag == "imputefilt"){
      
      selections <- filter_settings[[tabname]]$imputefilt

      tmp <- filter
      
      if(!is.null(isolate(get_group_DF(omicsData$objPP)))){
        p1 <- plot(isolate(omicsData$objPP), 
                   color_by = "Group", order_by = "Group") +
          labs(title = "Before handling missingness")
        
        if(all(unlist(tmp$e_data[-1]) %in% c(0, 1))){
          
          ## Still need group designation for color, order
          df <- as.data.frame(table(melt(tmp$e_data)[2:3]))
          p2 <- ggplot(data = df, aes(x = variable, fill = value, y = Freq)) + 
            geom_col() + theme_bw() + 
            labs(
              title = "After handling missingness",
              fill = "Conversion",
              y = "Number of biomolecules",
              x = "Sample",
              ) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
          
        } else {
          p2 <- plot_noconv(tmp, color_by = "Group", order_by = "Group") +
            labs(title = "After handling missingness")
        }

      } else {
        p1 <- plot(isolate(omicsData$objPP)) +
          labs(title = "Before handling missingness")
        
        
        if(all(unlist(tmp$e_data[-1]) %in% c(0, 1))){
          df <- as.data.frame(table(melt(tmp$e_data)[2:3]))
          p2 <- ggplot(data = df, aes(x = variable, fill = value, y = Freq)) + 
            geom_col() + theme_bw() + 
            labs(
              title = "After handling missingness",
              fill = "Conversion",
              y = "Number of biomolecules",
              x = "Sample",
            ) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
        } else {
          p2 <- plot_noconv(tmp) +
            labs(title = "After handling missingness")
        }
      }

      p <- wrap_plots(p1, p2, guides = "collect")

    } else if (filter_tag == "cvfilt" ){
      
      isolate(table_table_current$table[[paste0("PP__filters__", filter_tag)]] <- filters[[tabname]][[filter_tag]])
      isolate(table_table_current$names[[paste0("PP__filters__", filter_tag)]] <- paste0("Filter: ", filter_tag))
      

      if(settings$cv_threshold < max(filters[[tabname]][[filter_tag]]$CV, na.rm = T)){
        p <- do.call(plot, c(list(filters[[tabname]][[filter_tag]]),
                              settings))
      } else {
        p <- do.call(plot, list(filters[[tabname]][[filter_tag]]))
      }
      
      p <- p + labs(title = "Coefficient of Variation (CV)")

    } else {
      
      isolate(table_table_current$table[[paste0("PP__filters__", filter_tag)]] <- filters[[tabname]][[filter_tag]])
      isolate(table_table_current$names[[paste0("PP__filters__", filter_tag)]] <- paste0("Filter: ", filter_tag))

      p <- do.call(plot, c(list(filters[[tabname]][[filter_tag]]),
                      settings))
    }
    
    isolate(plot_table_current$table[[paste0("PP__filters__", filter_tag)]] <- p)
    isolate(plot_table_current$names[[paste0("PP__filters__", filter_tag)]] <- paste0("Filter: ", filter_tag))
    
    p
  })
})

output$add_impute_ui <- renderUI({
  
  tabname <- isolate(get_omicsData_type(omicsData$objPP))
  
  out1 <- prettySwitch(
    inputId = paste0(tabname, "_add_imputefilt"),
    label = div(
      "Add/Remove"#,
      # hidden(div(
      #   id = paste0(tabname, "_impute_exists"),
      #   style = "color:orange;float:right",
      #   icon("ok", lib = "glyphicon")
      # ))
    ),
    width = "100%"
  )
  
  out2 <- actionButton(inputId = paste0(tabname, "_preview_impute"), "Preview")

  if(length(input$missing_options_filter) == 0){
    out1 <- disabled(out1)
    out2 <- disabled(out2)
  }

  div(
    out1,
    out2
  )
})



