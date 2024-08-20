
# File string holder
zipped_file <- reactiveValues(fs = NULL)

# Buttons
output$Bundle_button <- renderUI({
  
  plots <- getShinyInput(input, "checkplot_")
  tables <- getShinyInput(input, "checktable_")
  
  cond <- any(as.logical(unlist(c(plots, tables))))
  
  button <- appButton(inputId = "makezipfile",
            label = tags$b("Bundle up all selected items"),
            icon = icon("briefcase"),
            lib = "glyphicon",
            style = "margin-left:5px"
  )
  
  if(!cond) button <- disabled(button)
  
  div(
    id = "js_zipbutton",
    style = "float:left",
    class = "grey_button",
    button
  )
  
})

output$New_model_button <- renderUI({
  
  cond <- is.null(zipped_file$fs)
  
  button <- appButton(inputId = "new_model",
                      label = tags$b("Try something else..."),
                      icon = icon("fast-backward"),
                      lib = "glyphicon",
                      style = "margin-left:5px"
  )
  
  if(cond) button <- disabled(button)
  
  div(
    id = "js_newmodelbutton",
    style = "float:left",
    class = "grey_button",
    button
  )
  
})

observeEvent(input$new_model, {
  showModal(modalDialog(
    title = "Select page to rewind to...", 
    div(
      style = "text-align: center;",
      appButton(inputId = "rewind_qc", label = "Change quality control filtering [Quality Control]"),
      br(),
      appButton(inputId = "rewind_msu", label = "Choose a different model [Model Set-up]"),
      br(),
      appButton(inputId = "rewind_pp", label = "Change pre-processing filtering [Pre-processing]"),
      br(),
      appButton(inputId = "rewind_rm", label = "Run the model with different parameters [Run Model]")
    )
  ))
})

reset_upload <- function() {
  omicsData$obj <- NULL
  reactive_dataholder$e_data <- NULL
  reactive_dataholder$e_meta <- NULL
  reactive_dataholder$f_data <- NULL
  
  for (name in names(plot_table_current$table)[which(startsWith(names(plot_table_current$table), "Upload__"))]) {
    plot_table_current$table[[name]] <- NULL
  }
  
  for (name in names(table_table_current$table)[which(startsWith(names(table_table_current$table), "Upload__"))]) {
    table_table_current$table[[name]] <- NULL
  }
  
  updateBoxCollapse(session, "groups_collapse_left", open = "fdata_options")
  updateBoxCollapse(session, "groups_collapse_left", open = "data_props_fdata")
  updateCheckboxInput(session, "use_example_fdata", value = FALSE)
  updateRadioGroupButtons(session, "use_fdata", selected = character(0))
  
  updateBoxCollapse(session, "upload_collapse_left", open = "datselect")
  updateRadioGroupButtons(session, "normalized", selected = character(0))
  updatePickerInput(session, "datascale", selected = character(0))
  updatePickerInput(session, "data_type", selected = character(0))
  updateCheckboxInput(session, "use_example", value = FALSE)
  
  updateBoxCollapse(session, "upload_preview_collapse", open = "summary_tables", close = "summary")
  updateBoxCollapse(session, "groups_collapse_right", open = "data_preview_fdata", close = "fdata_plots")
  
  shinyjs::show("experimental_upload_box")
  shinyjs::hide("sample_upload_box")
  shinyjs::hide("review_upload_box")
  
  shinyjs::addClass("show_exp_upload", "blueoutline")
  shinyjs::removeClass("show_sample_upload", "blueoutline")
  shinyjs::removeClass("review_upload", "blueoutline")
  
  shinyjs::disable("show_sample_upload")
  shinyjs::disable("review_upload")
  
  updateProgressBar(session, "upload_exp_done", value = 0)
  updateProgressBar(session, "upload_samp_done", value = 0)
}

reset_qc <- function() {
  omicsData$objQC <- omicsData$obj
  
  pepQCData$pepQCData <- NULL
  pepQCData$pepQCData$keep <- FALSE
  pepQCData$transforms_df <- NULL
  
  for (name in names(plot_table_current$table)[which(startsWith(names(plot_table_current$table), "QC__"))]) {
    plot_table_current$table[[name]] <- NULL
  }
  
  for (name in names(table_table_current$table)[which(startsWith(names(table_table_current$table), "QC__"))]) {
    table_table_current$table[[name]] <- NULL
  }
  
  shinyjs::show(id = "refnorm_box")
  shinyjs::hide(id = "low_ob_box")
  shinyjs::hide(id = "remove_outlier_box")
  shinyjs::hide(id = "missing_data_box")
  shinyjs::hide(id = "QC_review_selection_box")
  
  shinyjs::addClass("show_refnorm", "blueoutline")
  shinyjs::removeClass("show_low_obs", "blueoutline")
  shinyjs::removeClass("show_outlier_detect", "blueoutline")
  shinyjs::removeClass("show_missing_data", "blueoutline")
  shinyjs::removeClass("review_QC", "blueoutline")
  
  omicsData$objRefnorm <- NULL
  if (inherits(omicsData$objQC, "pepData")) {
    shinyjs::hide("refnorm_complete")
  }

  updateBoxCollapse(session, "references_collapse_left", open = "columnids")
  
  shinyjs::disable("show_outlier_detect")
  
  shinyjs::disable("show_missing_data")
  updateBoxCollapse(session, "missing_data_box", 
                    open = "missing_data_sample_box", 
                    close = "missing_by_biomolecule")
  if (inherits(omicsData$objQC, "pepData")) {
    shinyjs::hide("qc_biomolecule_detect")
    shinyjs::hide("qc_biomolecule_detect_plot")
  }
  shinyjs::hide("done_md")
  
  shinyjs::disable("review_QC")
  
  updateProgressBar(session, "QC_refnorm_done", value = 0)
  updateProgressBar(session, "QC_lo_done", value = 0)
  updateProgressBar(session, "QC_outlier_done", value = 0)
  updateProgressBar(session, "QC_missing_data_done", value = 0)
}

reset_msu <- function() {
  omicsData$objMSU <- omicsData$objQC
  
  for (name in names(plot_table_current$table)[which(startsWith(names(plot_table_current$table), "MSU__"))]) {
    plot_table_current$table[[name]] <- NULL
  }
  
  for (name in names(table_table_current$table)[which(startsWith(names(table_table_current$table), "MSU__"))]) {
    table_table_current$table[[name]] <- NULL
  }
  
  shinyjs::show(id = "VS_box")
  shinyjs::hide(id = "AG_box")
  shinyjs::hide(id = "EM_box")
  shinyjs::hide(id = "MSU_review_selection_box")
  
  shinyjs::addClass("show_VS", "blueoutline")
  shinyjs::removeClass("show_AGoals", "blueoutline")
  shinyjs::removeClass("show_EM", "blueoutline")
  shinyjs::removeClass("review_MSU", "blueoutline")
  
  updateBoxCollapse(session, "vs_collapse_right", open = "data_preview_all", close = "detected_plots")
  updateBoxCollapse(session, "vs_collapse_left", open = "use_cols_vs", close = "factor_cols_vs")
  shinyjs::hide("done_VS")
  
  shinyjs::disable("show_AGoals")
  updateBoxCollapse(session, "ag_collapse_center", open = "ag_choices")

  shinyjs::disable("show_EM")
  
  shinyjs::disable("review_MSU")
  
  updateProgressBar(session, "VS_done", value = 0)
  updateProgressBar(session, "AGoals_done", value = 0)
  updateProgressBar(session, "EM_done", value = 0)
}

reset_pp <- function() {
  omicsData$objPP <- omicsData$objMSU
  omicsData$objNorm <- NULL
  filter_settings_stored$stored <- NULL
  filter_effects$removed_mols <- NULL
  filter_effects$removed_samples <- NULL
  
  purrr::map(names(filters), function(nm) filters[[nm]] <- NULL)
  purrr::map(names(filter_flags), function(nm) filter_flags[[nm]] <- NULL)
  purrr::map(names(filter_settings), function(nm) filter_settings[[nm]] <- NULL)
  
  purrr::map(grep("_add_", names(input), value = T), function(id) {
    updatePrettySwitch(session = session, id, value = F)
  })

  for (name in names(plot_table_current$table)[which(startsWith(names(plot_table_current$table), "PP__"))]) {
    plot_table_current$table[[name]] <- NULL
  }
  
  for (name in names(table_table_current$table)[which(startsWith(names(table_table_current$table), "PP__"))]) {
    table_table_current$table[[name]] <- NULL
  }
  
  nm <- get_omicsData_type(omicsData$objPP)
  updatePrettySwitch(session = session, inputId = paste0(nm, "_lock_norm"), value = FALSE)
  
  shinyjs::show(id = "transform_box")
  shinyjs::hide(id = "filter_box")
  shinyjs::hide(id = "norm_box")
  shinyjs::hide(id = "rollup_box")
  shinyjs::hide(id = "PP_review_selection_box")
  
  shinyjs::addClass("show_transform", "blueoutline")
  shinyjs::removeClass("show_filters", "blueoutline")
  shinyjs::removeClass("show_normalization", "blueoutline")
  shinyjs::removeClass("show_protein_roll", "blueoutline")
  shinyjs::removeClass("review_PP", "blueoutline")
  
  updateBoxCollapse(session, id = "transform_collapse", open = "transformation")
  shinyjs::hide("complete_transform")
  
  omicsData$objfilters <- NULL
  filter_settings_stored$stored <- NULL
  shinyjs::disable("show_filters")
  shinyjs::enable("apply_filters")
  
  shinyjs::disable("show_normalization")
  updateBoxCollapse(session, id = "normalization_picker", open = "picker")
  
  shinyjs::hide("complete_rollup")
  
  shinyjs::disable("review_PP")
  
  updateProgressBar(session, "transform_done", value = 0)
  updateProgressBar(session, "filters_done", value = 0)
  updateProgressBar(session, "norm_done", value = 0)
  updateProgressBar(session, "rollup_done", value = 0)
}

reset_rm <- function () {
  omicsData$objRM <- NULL
  omicsData$objRM_reduced <- NULL
  
  for (name in names(plot_table_current$table)[which(startsWith(names(plot_table_current$table), "RM__"))]) {
    plot_table_current$table[[name]] <- NULL
  }
  
  for (name in names(table_table_current$table)[which(startsWith(names(table_table_current$table), "RM__"))]) {
    table_table_current$table[[name]] <- NULL
  }
  
  shinyjs::show(id = "rm_prompt_box")
  shinyjs::hide(id = "train_box")
  shinyjs::hide(id = "param_box")
  shinyjs::hide(id = "RM_box")
  shinyjs::hide(id = "RM_result_box")
  
  shinyjs::addClass("show_model_options", "blueoutline")
  shinyjs::removeClass("show_TrainSize", "blueoutline")
  shinyjs::removeClass("show_parameters", "blueoutline")
  shinyjs::removeClass("show_runmodel", "blueoutline")
  shinyjs::removeClass("review_RM", "blueoutline")
  
  disable("show_TrainSize")
  updateBoxCollapse(session, id = "TS_side_collapse", open = "train_param_RM")
  shinyjs::hide("complete_TS_RM")
  
  disable("show_parameters")
  updateBoxCollapse(session = session, id = "PO_RM_side_collapse", open = "side_param_RM")
  shinyjs::hide("complete_param")
  
  disable("show_runmodel")
  shinyjs::hide("complete_RM")
  
  disable("review_RM")
  
  updateProgressBar(session, "MO_done", value = 0)
  updateProgressBar(session, "TS_done", value = 0)
  updateProgressBar(session, "params_done", value = 0)
  updateProgressBar(session, "RM_done", value = 0)
  
  updateBoxCollapse(session = session, id = "download_collapse_pages", open = "download_tabset_QC")
}

qc_fix <- reactiveVal()

observeEvent(input$reset_upload, {
  req(isTruthy(input$reset_upload))
  reset_upload()
  
  removeModal()
})

observeEvent(c(input$rewind_qc, input$reset_qc), {
  req(isTruthy(input$rewind_qc) || isTruthy(input$reset_qc))
  reset_qc()
  reset_rm()
  reset_pp()
  reset_msu()
  reset_qc()
  
  updateNavbarPage(session, "top_page", "Quality Control")
  removeModal()
  
  qc_fix(runif(1))
})

observeEvent(qc_fix(), {
  reset_qc()
}, priority = -1)


observeEvent(c(input$rewind_msu, input$reset_msu), {
  req(isTruthy(input$rewind_msu) || isTruthy(input$reset_msu))
  reset_rm()
  reset_pp()
  reset_msu()
  
  updateNavbarPage(session, "top_page", "Model Set-Up")
  removeModal()
})

observeEvent(c(input$rewind_pp, input$reset_pp), {
  req(isTruthy(input$rewind_pp) || isTruthy(input$reset_pp))
  reset_rm()
  reset_pp()
  
  updateNavbarPage(session, "top_page", "Pre-processing")
  removeModal()
})

observeEvent(c(input$rewind_rm, input$reset_rm), {
  req(isTruthy(input$rewind_rm) || isTruthy(input$reset_rm))
  reset_rm()
  
  updateNavbarPage(session, "top_page", "Run Model")
  removeModal()
})

output$Download_button <- renderUI({
  
  plots <- getShinyInput(input, "checkplot_")
  tables <- getShinyInput(input, "checktable_")
  
  cond <- is.null(zipped_file$fs)
  
  button <- downloadButton(
    "download_processed_data",
    tags$b("Download bundle")
  )
  
  if(cond) button <- disabled(button)
  
  div(
    id = "js_downloadbutton",
    style = "margin-left:4px;float:left",
    class = "grey_button",
    button
  )
  
})


## Accordian behavior
observeEvent(input$upload_dwn_done, {
  
  updateBoxCollapse(session = session, 
                    id = "download_collapse_pages", 
                    close = "download_tabset_upload",
                    open = "download_tabset_QC"
  )
  
})

observeEvent(input$QC_dwn_done, {
  
  updateBoxCollapse(session = session, 
                    id = "download_collapse_pages", 
                    close = "download_tabset_QC",
                    open = "download_tabset_MSU"
  )
  
})

observeEvent(input$MSU_dwn_done, {
  
  updateBoxCollapse(session = session, 
                    id = "download_collapse_pages", 
                    close = "download_tabset_MSU",
                    open = "download_tabset_PP"
  )
  
})

observeEvent(input$PP_dwn_done, {
  
  updateBoxCollapse(session = session, 
                    id = "download_collapse_pages", 
                    close = "download_tabset_PP",
                    open = "download_tabset_RM"
                    )
  
})

observeEvent(input$RM_dwn_done, {
  
  updateBoxCollapse(session = session, 
                    id = "download_collapse_pages",
                    close = "download_tabset_RM"
  )
  
})

## Checkbox behavior

map(c("upload", "QC", "MSU", "PP", "RM"), function(pg){
  
  map(c("plot", "table"), function(type){
    
    observeEvent(input[[paste0(pg, "_dwn_select_all")]], {
      
      str <- paste0("check", type, "_", pg)
      values <- names(input)[str_detect(names(input), str)]
      
      map(values, function(val){
        updateCheckboxInput(session = session, val, value = T)
      })
      
    })
    
    observeEvent(input[[paste0(pg, "_dwn_deselect_all")]], {
      
      str <- paste0("check", type, "_", pg)
      values <- names(input)[str_detect(names(input), str)]
      
      map(values, function(val){
        updateCheckboxInput(session = session, val, value = F)
      })
      
    })
    
  })
  
})

