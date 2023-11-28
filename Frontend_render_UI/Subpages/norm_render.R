#' ## This needs to be modified for the overlord tab -- must skip this section

observeEvent(omicsData$objPP, {
  req(!is.null(isolate(omicsData$objPP)))
  
  nm <- str_to_title(class(isolate(omicsData$objPP))[[1]])
  
  output$norm_tab <- renderUI({
    norm_tab(nm)
  })
  
  load_norm_observers(nm)
  assign_norm_output(nm)
}, once = T)


#' ##### Function for glabal normalization #####
#' # Grab params for global normalization
get_params <- function(subset_fn) {

  req(!is.null(subset_fn))

  # Grab params
  params <- list()
  if ("los" == subset_fn) {
    params[["los"]] <- as.numeric(input[[paste0(gsub("_norm", "", input$top_page), "_los")]])
  } else if ("ppp" == subset_fn) {
    params[["ppp"]] <- as.numeric(input[[paste0(gsub("_norm", "", input$top_page), "_ppp")]])
  } else if ("rip" == subset_fn) {
    params[["rip"]] <- as.numeric(input[[paste0(gsub("_norm", "", input$top_page), "_rip")]])
  } else if ("ppp_rip" == subset_fn) {
    rip <- as.numeric(input[[paste0(gsub("_norm", "", input$top_page), "_rip")]])

    ppp <- as.numeric(input[[paste0(gsub("_norm", "", input$top_page), "_ppp")]])

    params[["ppp_rip"]] <- list(rip = rip, ppp = ppp)
  } else {
    params <- NULL
  }
  return(params)
}

#' # Evaluate norm bias for global normalization
inspect_norm <- function(omicsData, subset_fn, norm_fn, params) {
  group_df <- attr(omicsData, "group_DF")
  reorder <- match(
    colnames(omicsData$e_data)[-which(colnames(omicsData$e_data) == pmartR::get_edata_cname(omicsData))],
    as.character(group_df[, get_fdata_cname(omicsData)])
  )
  group <- group_df[reorder, ]$Group

  # create norm object and pull normalization parameters
  norm_object <- check_norm_possible(omicsData,
                                     subset_fn,
                                     norm_fn,
                                     params = params,
                                     apply_norm = FALSE
  )


  if (is.null(norm_object)) {
    return(NULL)
  }
  params <- norm_object$parameters$normalization

  # p value and dataframe of normalization factors for location
  p_location <- pmartR:::kw_rcpp(matrix(params$location, nrow = 1),
                                 group = as.character(group)
  )
  loc_params <- stack(params$location) %>%
    rename("VAL__" = values) # very possible someone has the column name 'values' in their data, so rename

  # p value and dataframe of normalization factors for scale, if there are scale parameters
  if (!is.null(params$scale)) {
    p_scale <- pmartR:::kw_rcpp(matrix(params$scale, nrow = 1),
                                group = as.character(group)
    )
    scale_params <- stack(params$scale) %>%
      rename("VAL__" = values)
  } else {
    p_scale <- NULL
  }

  #### Create Plots ####

  # plot by group if the object has group assignments
  if (!is.null(attributes(omicsData)$group_DF)) {
    # dataframe with group information
    loc_df <- attributes(omicsData)$group_DF %>%
      left_join(loc_params, by = setNames("ind", get_fdata_cname(omicsData)))
    # 'x' at the location parameter for each sample
    append_locs <- geom_point(
      data = loc_df,
      aes(
        x = !!rlang::sym(get_fdata_cname(omicsData)),
        y = VAL__, size = 7
      ),
      shape = 7, color = "red"
    )
    # boxplots of location parameters by group
    loc_boxplot <- plot_ly(
      data = loc_df,
      x = loc_df$Group,
      y = loc_df$VAL__,
      color = loc_df$Group,
      type = "box"
    ) %>%
      layout(
        title = "Location parameter values for chosen normalization, by group",
        xaxis = list(title = "Group"),
        yaxis = list(title = "Location Parameter Value")
      )

    # same as above but for scale
    if (!is.null(params$scale)) {
      scale_df <- attributes(omicsData)$group_DF %>%
        left_join(scale_params,
                  by = setNames("ind", get_fdata_cname(omicsData))
        )

      scale_boxplot <- plot_ly(
        data = scale_df,
        x = scale_df$Group,
        y = scale_df$VAL__,
        color = scale_df$Group,
        type = "box"
      ) %>%
        layout(
          title = "Scale parameter values for chosen normalization, by group",
          xaxis = list(title = "Group"),
          yaxis = list(title = "Scale Parameter Value")
        )
    } else {
      scale_boxplot <- NULL
    }

    # if no group df simply make the location and scale dataframes and erase the normalization factor boxplots
  } else {
    loc_df <- omicsData$f_data %>%
      left_join(loc_params,
                by = setNames("ind", get_fdata_cname(omicsData))
      )
    append_locs <- geom_point(
      data = loc_df,
      aes(
        x = !!rlang::sym(get_fdata_cname(omicsData)),
        y = VAL__,
        size = 7
      ),
      shape = 7, color = "red"
    )

    # these are irrelevant if theres no grouping structure
    loc_boxplot <- scale_boxplot <- NULL

    if (!is.null(params$scale)) {
      scale_df <- omicsData$f_data %>%
        left_join(scale_params,
                  by = setNames("ind", get_fdata_cname(omicsData))
        )
    }
  }

  return(
    list(
      p_location = p_location,
      p_scale = p_scale,
      loc_boxplot = loc_boxplot,
      scale_boxplot = scale_boxplot
    )
  )
}


## Warning fore bad norm
check_norm_possible <- function(...) {
  out <- tryCatch(
    {
      norm_object <- normalize_global(...)
      
      norm_object
    },
    error = function(e) {
      # datatypes_pos$dataholder[[type]]$Emeta <- NULL
      # input[[paste0(type, "_file_emeta")]]
      # reset(paste0(tab, "_file_emeta"))
      
      popup$status <- "Norm"
      shinyalert(
        text = paste0("Something went wrong: ", e$message),
        type = "error",
        callbackR = function(val) {
          popup$status <- "None"
          return(NULL)
        }
      )
      NULL
    },
    warning = function(w) {
      popup$status <- "Norm"
      shinyalert(
        "Careful!",
        paste0("Just to let you know: ", w$message),
        type = "warning",
        callbackR = function(val) {
          popup$status <- "None"
          return(NULL)
        }
      )
      norm_object
    }
  )
  out
}

#' 
#' #'@details Function which assigns the observers for unassigned data types
#' #'for the normalization tab
load_norm_observers <- function(new_tabs) {

  map(new_tabs, function(tab){
    if(tab %in% c("Prodata","Pepdata",  "Isobaricpepdata")){
      ## SPANS
      ### Run spans ###
      observeEvent(input[[paste0(tab, "_spans_start_button")]], {
        
        req(input[[paste0(tab, "_spans_start_button")]] > 0 &&
              !is.null(input[[paste0(tab, "_spans_norm_fn")]]) &&
              !is.null(input[[paste0(tab, "_spans_subset_fn")]]))

        showNotification("Running SPANS. SPANS may take several minutes to complete. Navigation to other tabs is disabled while SPANS is running, please wait...",
                         duration = NULL,
                         closeButton = FALSE,
                         id = paste0(tab, "_SPANS_note")
        )

        UI_elements <- paste0(tab, c(
          "_preview_normalization",
          "_lock_norm",
          "_inspect_norm",
          "_spans_start_button",
          "_normalize_option"
        ))

        map(UI_elements, hide)

        hide(selector = ".navbar-nav a")

        on.exit({
          # enable(paste0(tab, "_spans_start_button"))
          shinyjs::show(selector = ".navbar-nav a")
          removeNotification(paste0(tab, "_SPANS_note"))
          map(UI_elements, show)
          updateBoxCollapse(session, paste0(tab, "_SPANS_sidebar"), close = "choose_params")

        })

        updateBoxCollapse(session, paste0(tab, "_normalization_sidebar"), close = "choose_params")
        updateBoxCollapse(session, paste0(tab, "_normalization_mainpanel"), close = "normdata_mainpanel")
        updateBoxCollapse(session, paste0(tab, "_spans_mainpanel"), open = "spans_mainpanel")

        # Grab parameter inputs
        params <- list()
        if (any(c("los", "rip", "ppp", "ppp_rip") %in% input[[paste0(tab, "_spans_subset_fn")]])) {
          if ("los" %in% input[[paste0(tab, "_spans_subset_fn")]]) {
            if (!any(is.na(text_parser(input[[paste0(tab, "_spans_los_other")]])))) {
              params[["los"]] <- as.numeric(
                c(
                  input[[paste0(tab, "_spans_los")]],
                  text_parser(input[[paste0(tab, "_spans_los_other")]])
                )
              )
            } else {
              params[["los"]] <- as.numeric(input[[paste0(tab, "_spans_los")]])
            }
          }

          if ("ppp" %in% input[[paste0(tab, "_spans_subset_fn")]]) {
            if (!any(is.na(text_parser(input[[paste0(tab, "_spans_ppp_other")]])))) {
              params[["ppp"]] <- as.numeric(
                c(
                  input[[paste0(tab, "_spans_ppp")]],
                  text_parser(input[[paste0(tab, "_spans_ppp_other")]])
                )
              )
            } else {
              params[["ppp"]] <- as.numeric(input[[paste0(tab, "_spans_ppp")]])
            }
          }

          if ("rip" %in% input[[paste0(tab, "_spans_subset_fn")]]) {
            if (!any(is.na(text_parser(input[[paste0(tab, "_spans_rip_other")]])))) {
              params[["rip"]] <- as.numeric(
                c(
                  input[[paste0(tab, "_spans_rip")]],
                  text_parser(input[[paste0(tab, "_spans_rip_other")]])
                )
              )
            } else {
              params[["rip"]] <- as.numeric(input[[paste0(tab, "_spans_rip")]])
            }
          }

          if ("ppp_rip" %in% input[[paste0(tab, "_spans_subset_fn")]]) {
            if (!any(is.na(text_parser(input[[paste0(tab, "_spans_rip_other")]])))) {
              rip <- as.numeric(
                c(
                  input[[paste0(tab, "_spans_rip")]],
                  text_parser(input[[paste0(tab, "_spans_rip_other")]])
                )
              )
            } else {
              rip <- as.numeric(input[[paste0(tab, "_spans_rip")]])
            }

            if (!any(is.na(text_parser(input[[paste0(tab, "_spans_ppp_other")]])))) {
              ppp <- as.numeric(
                c(
                  input[[paste0(tab, "_spans_ppp")]],
                  text_parser(input[[paste0(tab, "_spans_ppp_other")]])
                )
              )
            } else {
              ppp <- as.numeric(input[[paste0(tab, "_spans_ppp")]])
            }

            params[["ppp_rip"]] <- map2(ppp, rip, function(ppp2, rip2) c(ppp2, rip2))
          }
        } else {
          params <- NULL
        }

        # Run spans
        SPANS_res[[tab]] <- spans_procedure(
          omicsData$objPP,
          norm_fn = input[[paste0(tab, "_spans_norm_fn")]],
          subset_fn = input[[paste0(tab, "_spans_subset_fn")]],
          params = params
        )
      })

      ### SPANS defaults ###
      observeEvent(input[[paste0(tab, "_load_default_spans")]], {
        updatePickerInput(session, paste0(tab, "_spans_norm_fn"), selected = c(
          "Mean" = "mean",
          "Median" = "median",
          "Z-norm" = "zscore",
          "Median Absolute Distance" = "mad"
        ))
        updatePickerInput(session, paste0(tab, "_spans_subset_fn"),
                          selected = c(
                            "No Subsetting" = "all",
                            "Top L order statistics (los)" = "los",
                            "Percentage present (ppp)" = "ppp",
                            "Rank invariant (rip)" = "rip",
                            "Percentage present and rank invariant (ppp+rip)" = "ppp_rip"
                          )
        )
      })

      ### Clear SPANS Input ###
      observeEvent(input[[paste0(tab, "_clear_input_spans")]], {
        updatePickerInput(session, paste0(tab, "_spans_norm_fn"), selected = character(0))
        updatePickerInput(session, paste0(tab, "_spans_subset_fn"), selected = character(0))
      })

      ## Global normalization
      ## Populate Global normalization options from spans table
      observeEvent(input[[paste0(tab, "_use_spans")]], {
        selected <- input[[paste0(tab, "_spans_table_rows_selected")]]
        req(!is.null(selected))

        tab <- tab
        # req(tab %in% ALL_DATATYPE_NAMES)

        df <- SPANS_res[[tab]]
        options <- df[selected, ]

        updateBoxCollapse(session,
                          id = paste0(tab, "_normalization_sidebar"),
                          open = "choose_params"
        )

        updatePickerInput(
          session,
          paste0(tab, "_subset_fn"),
          selected = options$subset_method
        )

        updatePickerInput(
          session,
          paste0(tab, "_norm_fn"),
          selected = options$normalization_method
        )

        if (options$subset_method %in% c("los", "ppp", "rip")) {
          updateNumericInput(
            session,
            paste0(tab, "_", options$subset_method),
            value = options$parameters
          )
        } else if (options$subset_method == "ppp_rip") {
          opts <- str_split(options$parameters, ";")[[1]]

          updateNumericInput(
            session,
            paste0(tab, "_ppp"),
            value = opts[1]
          )

          updateNumericInput(
            session,
            paste0(tab, "_rip"),
            value = opts[2]
          )
        }
      })

    }

    ## General
    ## Evaluate global normalization; process and UI loading
    observeEvent(input[[paste0(tab, "_inspect_norm")]], {

      # req(tab %in% ALL_DATATYPE_NAMES)
      req(input[[paste0(tab, "_inspect_norm")]] > 0)


      params <- get_params(subset_fn = input[[paste0(tab, "_subset_fn")]])

      eval <- inspect_norm(
        omicsData = omicsData$objPP,
        subset_fn = input[[paste0(tab, "_subset_fn")]],
        norm_fn = input[[paste0(tab, "_norm_fn")]],
        params = params
      )

      if (is.null(eval)) {
        return()
      }

      removeTab(
        inputId = paste0(tab, "_normalize_boxplot_tabset"),
        target = "Location Bias"
      )

      removeTab(
        inputId = paste0(tab, "_normalize_boxplot_tabset"),
        target = "Scale Bias"
      )

      if (!is.null(eval$p_location)) {
        msg <- if (eval$p_location < 0.05) {
          div(style = "color:red", "There is evidence to suggest the centering effect of this normalization differs across groups, consider choosing another normalization.")
        }
        else {
          div("Weak or no evidence to suggest a difference in the centering effect of this normalization across groups.")
        }

        insertTab(
          inputId = paste0(tab, "_normalize_boxplot_tabset"),
          target = "Normalization Preview",
          position = "after",
          tab = tabPanel(
            "Location Bias",
            br(),
            plotlyOutput(paste0(tab, "_normalize_eval_location")),
            br(),
            msg,
            paste0("P-value: ", signif(eval$p_location))
          )
        )

        output[[paste0(tab, "_normalize_eval_location")]] <- renderPlotly({
          eval$loc_boxplot
        })

        updateTabsetPanel(
          session,
          paste0(tab, "_normalize_boxplot_tabset"),
          "Location Bias"
        )
      }

      if (!is.null(eval$p_scale)) {
        msg <- if (eval$p_scale < 0.05) {
          div(style = "color:red", "There is evidence to suggest the scaling effect of this normalization differs across groups, consider choosing another normalization.")
        }
        else {
          div("Weak or no evidence to suggest a difference in the scaling effect of this normalization across groups.")
        }

        insertTab(
          inputId = paste0(tab, "_normalize_boxplot_tabset"),
          target = "Normalization Preview",
          position = "after",
          tab = tabPanel(
            "Scale Bias",
            br(),
            plotlyOutput(paste0(tab, "_normalize_eval_scale")),
            br(),
            msg,
            paste0("P-value: ", signif(eval$p_scale))
          )
        )

        output[[paste0(tab, "_normalize_eval_scale")]] <- renderPlotly({
          eval$scale_boxplot
        })

        updateTabsetPanel(
          session,
          paste0(tab, "_normalize_boxplot_tabset"),
          "Scale Bias"
        )
      }
    })

    ## Preview normalization with selected parameters
    # observeEvent(input[[paste0(tab, "_preview_normalization")]], {
    #   # req(tab %in% ALL_DATATYPE_NAMES)
    #   req(input[[paste0(tab, "_preview_normalization")]] > 0)
    #   updateTabsetPanel(session, paste0(tab, "_normalize_boxplot_tabset"),
    #                     selected = "Normalization Preview"
    #   )
    # })

    ## Load normalization parameters and lock all inputs
    observeEvent(input[[paste0(tab, "_lock_norm")]], {

      req(#tab %in% ALL_DATATYPE_NAMES && 
            !is.null(input[[paste0(tab, "_normalize_option")]]))

      UI_elements <- paste0(tab, c(
        "_normalize_option",
        "_subset_fn",
        "_norm_fn",
        "_backtransform",
        "_los",
        "_ppp",
        "_rip"#,
        # "_loess_method",
        # "_loess_span"
      ))

      if (input[[paste0(tab, "_lock_norm")]]) {

        if (input[[paste0(tab, "_normalize_option")]] %in% c("Global Normalization", "SPANS - Proteomics only")) {

          params <- get_params(subset_fn = input[[paste0(tab, "_subset_fn")]])

          omicsData$objNorm <- check_norm_possible(omicsData$objPP,
                                             subset_fn = input[[paste0(tab, "_subset_fn")]],
                                             norm_fn = input[[paste0(tab, "_norm_fn")]],
                                             params = params,
                                             apply_norm = TRUE
          )

          if (is.null(omicsData$objNorm)) {
            updatePrettySwitch(session, paste0(tab, "_lock_norm"), value = F)
            return(NULL)
          }

          norm_settings[[tab]] <- list(
            settings = input[[paste0(tab, "_normalize_option")]],
            subset_fn = input[[paste0(tab, "_subset_fn")]],
            norm_fn = input[[paste0(tab, "_norm_fn")]],
            params = params,
            backtransform = as.logical(input[[paste0(tab, "_backtransform")]])
          )
          } else {
            norm_settings[[tab]] <- list(
              settings = input[[paste0(tab, "_normalize_option")]],
              method = input[[paste0(tab, "_loess_method")]],
              span = input[[paste0(tab, "_loess_span")]]
            )
        }

        map(UI_elements, disable)
        updateBoxCollapse(session, paste0(tab, "_normalization_mainpanel"), open = "normdata_mainpanel")
        updateTabsetPanel(session, paste0(tab, "_normalize_boxplot_tabset"), selected = "Normalization Preview")

      } else {
        omicsData$objNorm <- NULL
        norm_settings[[tab]] <- NULL
        map(UI_elements, enable)
      }
    })

    ## Reactions to other inputs ##

    # Remove bias tabs when new normalization option selected
    observeEvent(input[[paste0(tab, "_normalize_option")]], {

      # req(tab %in% ALL_DATATYPE_NAMES)

      removeTab(
        inputId = paste0(tab, "_normalize_boxplot_tabset"),
        target = "Location Bias"
      )

      removeTab(
        inputId = paste0(tab, "_normalize_boxplot_tabset"),
        target = "Scale Bias"
      )
    })

    # Tab tool tip for normalization picker
    observeEvent(input[[paste0(tab, "_normalize_option")]],
                 {
                   # req(tab %in% ALL_DATATYPE_NAMES)
                   show_add_tooltip(
                     session, paste0(tab, "_norm_picker_icon"),
                     is.null(input[[paste0(tab, "_normalize_option")]]),
                     "Specify normalization to apply"
                   )
                 },
                 ignoreNULL = FALSE
    )

    # Tab tool tip for normalization sidebar
    # observeEvent(c(
    #   input[[paste0(tab, "_lock_norm")]],
    #   norm_settings[[tab]],
    #   SPANS_res[[tab]],
    #   input[[paste0(tab, "_normalize_option")]]
    # ),
    # {
    # 
    #   req(tab %in% ALL_DATATYPE_NAMES)
    # 
    #   # show_add_tooltip(
    #   #   session,
    #   #   paste0(tab, "_apply_norm_icon"),
    #   #   is.null(norm_settings[[tab]]),
    #   #   "Set parameters and apply normalization"
    #   # )
    # 
    #   show_add_tooltip(
    #     session,
    #     paste0(tab, "_spans_norm_icon"),
    #     is.null(SPANS_res[[tab]]),
    #     "Run SPANS using 2+ normalization settings"
    #   )
    # },
    # ignoreNULL = FALSE
    # )

  })

}

norm_settings <- reactiveValues()

#' observeEvent(input$normalization_engage, {
#'   
#'   req(input$normalization_engage > 0)
#'   
#'   map(names(norm_settings), function(norm_name) {
#'     ob_norm <- norm_settings[[norm_name]]
#'     
#'     if (ob_norm$settings == "Normalize Global") {
#'       omicsData_objects[[norm_name]] <- check_norm_possible(
#'         omicsData_objects[[norm_name]],
#'         subset_fn = ob_norm$subset_fn,
#'         norm_fn = ob_norm$norm_fn,
#'         params = ob_norm$params,
#'         backtransform = ob_norm$backtransform,
#'         apply_norm = T
#'       )
#'       # } else if (ob_norm$settings == "Normalize Loess") {
#'       #   omicsData_objects[[norm_name]] <- normalize_loess(
#'       #     omicsData_objects[[norm_name]],
#'       #     method = ob_norm$method,
#'       #     span = ob_norm$span
#'       #   )
#'     }
#'     
#'     disable(paste0(norm_name, "_lock_norm"))
#'     
#'   })
#'   
#'   selecteddata <- names(omicsData_objects)
#'   map(selecteddata, function(nm_ob) {
#'     tables[["Normalization"]][[nm_ob]] <- omicsData_objects[[nm_ob]]
#'   })
#'   
#'   html(html = 'Normalization <span class="caret"></span>', selector = '.dropdown-toggle[data-value="Normalization"]') # no tool tip
#'   html(html = "<div>Progress</div>", selector = 'a[data-value="Normalization_progress"]')
#'   
#'   if (any(c("Label-free", "Isobaric") %in% names(omicsData_objects))) {
#'     go_to <- actionButton("goto_pepstats",
#'                           "Continue to Peptide Statistics",
#'                           style = "margin-top:5px;width:75%"
#'     )
#'   } else {
#'     go_to <- actionButton("goto_statistics",
#'                           "Continue to Statistical Analysis",
#'                           style = "margin-top:5px;width:75%"
#'     )
#'   }
#'   
#'   showModal(
#'     modalDialog(
#'       title = "Success! Normalizations Applied to Data:",
#'       do.call(
#'         tabsetPanel,
#'         lapply(names(omicsData_objects), function(name) {
#'           tabPanel(
#'             name,
#'             fluidRow(
#'               column(
#'                 10,
#'                 align = "center", offset = 1,
#'                 
#'                 HTML(paste0('<h4 style= "color:#1A5276">', name,' Data</h4>')),
#'                 
#'                 br(),
#'                 
#'                 
#'                 tags$table(
#'                   style = "padding: 1%; width: 100%;",
#'                   
#'                   tags$tr(
#'                     tags$th("Type:"),
#'                     tags$td(
#'                       ifelse(
#'                         is.null(norm_settings[[name]]$settings),
#'                         "Not applied",
#'                         norm_settings[[name]]$settings
#'                       )
#'                     )
#'                   ),
#'                   
#'                   tags$tr(
#'                     tags$th("Subset:"),
#'                     tags$td(
#'                       ifelse(
#'                         is.null(norm_settings[[name]]$subset_fn),
#'                         "Not applied",
#'                         str_to_title(norm_settings[[name]]$subset_fn)
#'                       )
#'                     )
#'                   ),
#'                   
#'                   tags$tr(
#'                     tags$th("Normalization:"),
#'                     tags$td(
#'                       ifelse(
#'                         is.null(norm_settings[[name]]$norm_fn),
#'                         "Not applied",
#'                         str_to_title(norm_settings[[name]]$norm_fn)
#'                       )
#'                     )
#'                   ),
#'                   
#'                   tags$tr(
#'                     tags$th("Parameters:"),
#'                     tags$td(
#'                       ifelse(
#'                         is.null(norm_settings[[name]]$params),
#'                         "Not applied",
#'                         as.character(norm_settings[[type]]$params)
#'                       )
#'                     )
#'                   ),
#'                   
#'                   tags$tr(
#'                     tags$th("Backtransform:"),
#'                     tags$td(
#'                       ifelse(
#'                         is.null(norm_settings[[name]]$backtransform),
#'                         "Not applied",
#'                         ifelse(norm_settings[[name]]$backtransform,
#'                                "Yes",
#'                                "No"
#'                         )
#'                       )
#'                     )
#'                   )
#'                 ),
#'                 br(),
#'                 hr()
#'               )
#'             )
#'           )
#'         })
#'       ),
#'       
#'       fluidRow(
#'         column(10,
#'                align = "center", offset = 1,
#'                actionButton("filter_dismiss", "Stay on this tab", width = "75%", `data-dismiss` = "modal"),
#'                actionButton("goto_EDA", "Continue to Exploratory Data Analysis", style = "margin-top:5px;width:75%"),
#'                go_to
#'         )
#'       ),
#'       footer = NULL
#'     )
#'   )
#'   
#'   disable(id = "normalization_engage") ###################### May change later with addition of reset
#'   Tab_Completion_tracking$state[["Normalization"]] <- 1
#'   
#'   selecteddata <- names(omicsData_objects)
#'   export_norm <- list()
#'   for (export_obj in selecteddata) {
#'     export_norm[[export_obj]] <- attr(omicsData_objects[[export_obj]], "data_info")
#'   }
#'   exportTestValues(norm = export_norm)
#' }
#' )
#' ####
#' 
#' ## Move to other tabs
#' observeEvent(input$goto_EDA, {
#'   removeModal()
#'   updateNavbarPage(session, "top_page", selected = "EDA")
#' })
#' 
#' observeEvent(input$goto_pepstats, {
#'   removeModal()
#'   
#'   selecteddata <- names(omicsData_objects)
#'   selecteddata <- ALL_DATATYPE_NAMES[ALL_DATATYPE_NAMES %in% selecteddata] # order
#'   updateNavbarPage(session, "top_page", selected = paste0(
#'     selecteddata[selecteddata %in% c("Isobaric", "Label-free")][1],
#'     "_pepstats"
#'   ))
#' })
#' 
#' observeEvent(input$goto_statistics, {
#'   removeModal()
#'   
#'   selecteddata <- names(omicsData_objects)
#'   selecteddata <- ALL_DATATYPE_NAMES[ALL_DATATYPE_NAMES %in% selecteddata] # order
#'   updateNavbarPage(session, "top_page", selected = paste0(
#'     selecteddata[selecteddata %in% ALL_DATATYPE_NAMES][1],
#'     "_stats"
#'   ))
#' })
#' 
#' ## Tab/item completion and tooltips ##
#' 
#' # Activation of next tab with normalizaton page tool tip
#' # observeEvent(c(complete_normalization(), input$apply_filters),
#' #              {
#' #                cond <- all(sapply(names(datatypes_pos$dataholder), function(name) !is.null(omicsData_objects[[name]]))) &&
#' #                  complete_goals() && !is.null(input$apply_filters) && input$apply_filters > 0
#' #                
#' #                if ((is.null(complete_normalization()) || !complete_normalization()) && cond) {
#' #                  tooltip_span <- generate_warning_tooltip("norm-warning-tooltip")
#' #                  html(
#' #                    html = paste0("<div>%sNormalization <span class='caret'></span></div>", tooltip_span),
#' #                    selector = '.dropdown-toggle[data-value="Normalization"]'
#' #                  )
#' #                  addPrompter(session, "norm-warning-tooltip", "One or more required items are incomplete")
#' #                  
#' #                  js$disableTab("EDA")
#' #                  js$disableTab("Pepstats")
#' #                  js$disableTab("Statistics")
#' #                  
#' #                  ## triggers when remade with imd_anova filter
#' #                } else if (!is.null(input$apply_filters) && input$apply_filters > 0 && 
#' #                           (is.null(input$normalization_engage) || input$normalization_engage < 1)) {
#' #                  # tooltip_span <- "<span id = 'norm-warning-tooltip'', class='glyphicon glyphicon-exclamation-sign'', style='color:red;margin-right:3px'></span>"
#' #                  # html(
#' #                  #   html = paste0("<div>%sNormalization <span class='caret'></span></div>", tooltip_span),
#' #                  #   selector = '.dropdown-toggle[data-value="Normalization"]'
#' #                  # )
#' #                  # addPrompter(session, "norm-warning-tooltip", 'Confirm selections on "Progress" page')
#' #                  
#' #                  tooltip_span <- generate_warning_tooltip("norm-progress-warning-tooltip")
#' #                  html(
#' #                    html = paste0("<div>%sProgress</div>", tooltip_span),
#' #                    selector = 'a[data-value="Normalization_progress"]'
#' #                  )
#' #                  addPrompter(session, "norm-progress-warning-tooltip", "Confirm normalization selections to proceed")
#' #                }
#' #              },
#' #              ignoreNULL = FALSE
#' # )
#' 
#' 


### Norm output UI

## Used to extract Global parameters
text_parser <- function(text) {
  res <- str_split(text, ",")
  if (length(res) > 0) {
    return(suppressWarnings(as.numeric(str_trim(res[[1]]))))
  } else {
    return(NA)
  }
}

## Used to populate UI for spans testing options (used in line 361)
populate_global_options <- function(output_name, input_name, tab) {
  output[[paste0(tab, output_name)]] <- renderUI({
    condition <- tab %in% c("Prodata", "Pepdata", "Isobaricpepdata")
    
    # LOS
    if (input_name == "_spans_los") {
      condition <- condition &&
        "los" %in% input[[paste0(tab, "_spans_subset_fn")]]
      defaults <- c(0.05, 0.1, 0.2, 0.3)
      input_text <- "Proportions for Top L order statistics:"
      
      # PPP
    } else if (input_name == "_spans_ppp") {
      condition <- condition &&
        any(c("ppp", "ppp_rip") %in% input[[paste0(tab, "_spans_subset_fn")]])
      defaults <- c(0.1, 0.25, 0.50, 0.75)
      input_text <- "Percentage present proportions:"
      
      # RIP
    } else {
      condition <- condition &&
        any(c("rip", "ppp_rip") %in% input[[paste0(tab, "_spans_subset_fn")]])
      defaults <- c(0.1, 0.15, 0.2, 0.25)
      input_text <- "Rank invariant p-value thresholds:"
    }
    
    if (condition) {
      
      # Auto-fill
      clear <- input[[paste0(tab, "_clear_input_spans")]]
      default <- input[[paste0(tab, "_load_default_spans")]]
      
      if (!is.null(clear) && !is.null(default) && clear > default) {
        fill <- NULL
      } else {
        fill <- defaults
      }
      
      # Checkbox input and tet input for custom values
      return(
        tagList(
          br(),
          checkboxGroupInput(
            paste0(tab, input_name),
            input_text,
            choices = defaults,
            selected = fill,
            inline = TRUE
          ),
          
          textInput(paste0(tab, input_name, "_other"),
                    "Custom values (comma seperated):",
                    placeholder = "E.g. 0.025, 0.25, 0.66"
          )
        )
      )
    } else {
      return()
    }
  })
}

###
assign_norm_output <- function(tab) {
  list(
    
    #### Tab UI based on norm option picker ####
    
    ## Side panel ##
    output[[paste0(tab, "_normalize_sidepanel")]] <- renderUI({
      if (is.null(input[[paste0(tab, "_normalize_option")]])) {
        return()
      } else if (input[[paste0(tab, "_normalize_option")]] == "SPANS - Proteomics only") {
        return(
          div(
            collapseBoxGroup(
              id = paste0(tab, "_normalization_sidebar"),
              
              # SPANS panel, only available for proteomics
              collapseBox(
                "Evaluate Normalization Options",
                icon_id = paste0(tab, "_spans_norm_icon"),
                icon = icon("exclamation-sign", lib = "glyphicon"),
                icon_hidden = F,
                value = "use_spans",
                collapsed = F,
                uiOutput(paste0(tab, "_spans_settings"))
              ),
              
              # Normalization parameters
              collapseBox(
                "Select Normalization",
                icon_id = paste0(tab, "_apply_norm_icon"),
                icon = icon("exclamation-sign", lib = "glyphicon"),
                icon_hidden = F,
                value = "choose_params",
                
                pickerInput(
                  paste0(tab, "_norm_fn"),
                  "Normalization Function",
                  choices = c(
                    "Mean" = "mean",
                    "Median" = "median",
                    "Z-norm" = "zscore",
                    "Median Absolute Distance" = "mad"
                  ),
                  options = pickerOptions(maxOptions = 1),
                  multiple = TRUE
                ),
                
                
                pickerInput(
                  paste0(tab, "_subset_fn"),
                  "Subset Function",
                  choices = c(
                    "No subsetting" = "all",
                    "Top L order statistics (los)" = "los",
                    "Percentage present (ppp)" = "ppp",
                    "Complete" = "complete",
                    "Rank invariant (rip)" = "rip",
                    "Percentage present and rank invariant (ppp+rip)" = "ppp_rip"
                  ),
                  options = pickerOptions(maxOptions = 1),
                  multiple = TRUE
                ),
                
                uiOutput(paste0(tab, "_subset_params")),
                
                radioGroupButtons(
                  paste0(tab, "_backtransform"),
                  "Apply backtransformation?",
                  choices = c("Yes" = TRUE, "No" = FALSE),
                  selected = character(0)
                ),
                
                
                hr(),
                
                uiOutput(paste0(tab, "_norm_buttons"))
              )
            )
          )
        )
      } else if (input[[paste0(tab, "_normalize_option")]] == "Global Normalization") {
        return(
          div(
            collapseBoxGroup(
              id = paste0(tab, "_normalization_sidebar"),
              
              # Normalization parameters
              collapseBox(
                "Apply Normalization",
                icon_id = paste0(tab, "_apply_norm_icon"),
                icon = icon("exclamation-sign", lib = "glyphicon"),
                icon_hidden = F,
                value = "choose_params",
                collapsed = F,
                
                pickerInput(
                  paste0(tab, "_norm_fn"),
                  "Normalization Function",
                  choices = c(
                    "Mean" = "mean",
                    "Median" = "median",
                    "Z-norm" = "zscore",
                    "Median Absolute Distance" = "mad"
                  ),
                  options = pickerOptions(maxOptions = 1),
                  multiple = TRUE
                ),
                
                
                pickerInput(
                  paste0(tab, "_subset_fn"),
                  "Subset Function",
                  choices = c(
                    "No Subsetting" = "all",
                    "Top L order statistics (los)" = "los",
                    "Percentage present (ppp)" = "ppp",
                    "Complete" = "complete",
                    "Rank invariant (rip)" = "rip",
                    "Percentage present and rank invariant (ppp+rip)" = "ppp_rip"
                  ),
                  options = pickerOptions(maxOptions = 1),
                  multiple = TRUE
                ),
                
                uiOutput(paste0(tab, "_subset_params")),
                
                radioGroupButtons(
                  paste0(tab, "_backtransform"),
                  "Apply backtransformation?",
                  choices = c("Yes" = TRUE, "No" = FALSE),
                  selected = character(0)
                ),
                
                
                hr(),
                
                uiOutput(paste0(tab, "_norm_buttons"))
              )
            )
          )
        )
      }
      # } else if (input[[paste0(tab, "_normalize_option")]] == "Loess Normalization") {
      #   return(
      #     div(
      #       collapseBoxGroup(
      #         id = paste0(tab, "_normalization_sidebar"),
      #         open = "choose_params",
      #
      #         # Normalization parameters
      #         collapseBox(
      #
      #           subsection_header(
      #             "Apply Normalization",
      #             paste0(tab, "_apply_norm_icon"),
      #             "color:red;float:right",
      #             icon("exclamation-sign", lib = "glyphicon")
      #           ),
      #           value = "choose_params",
      #
      #           pickerInput(
      #             paste0(tab, "_loess_method"),
      #             "Normalization Function",
      #             choices = c(
      #               "Fast linear loess" = "fast",
      #               "Ordinary cyclic loess (pairwise)" = "pairs",
      #               "Affy cyclic loess (pairwise, column order invarient)" = "affy"
      #             ),
      #             options = pickerOptions(maxOptions = 1),
      #             multiple = TRUE
      #           ),
      #
      #           numericInput(
      #             paste0(tab, "_loess_span"),
      #             "Loess smoothing window span (0 to 1):",
      #             min = 0.000000000000000001,
      #             max = 0.999999999999999999,
      #             step = 0.05,
      #             value = 0.4
      #           ),
      #
      #           hr(),
      #
      #           uiOutput(paste0(tab, "_norm_buttons"))
      #         )
      #       )
      #     )
      #   )
      # }
    }),
    
    ## Spans UI in side panel ##
    output[[paste0(tab, "_spans_settings")]] <- renderUI({
      
      # Where proteomic, load spans options
      if (tab %in% c("Prodata", "Pepdata", "Isobaricpepdata")) {
        out <- list(
          actionButton(paste0(tab, "_clear_input_spans"), "Clear Inputs"),
          actionButton(paste0(tab, "_load_default_spans"), "Load Defaults"),
          hr(),
          
          pickerInput(
            paste0(tab, "_spans_norm_fn"),
            "Normalization Functions to Evaluate",
            choices = c(
              "Mean" = "mean",
              "Median" = "median",
              "Z-norm" = "zscore",
              "Median Absolute Distance" = "mad"
            ),
            multiple = TRUE,
            selected = c(
              "Mean" = "mean",
              "Median" = "median",
              "Z-norm" = "zscore",
              "Median Absolute Distance" = "mad"
            )
          ),
          
          # Subset function
          pickerInput(
            paste0(tab, "_spans_subset_fn"),
            "Subset Functions to Evaluate",
            choices = c(
              "No Subsetting" = "all",
              "Top L order statistics (los)" = "los",
              "Percentage present (ppp)" = "ppp",
              "Complete" = "complete",
              "Rank invariant (rip)" = "rip",
              "Percentage present and rank invariant (ppp+rip)" = "ppp_rip"
            ),
            multiple = TRUE,
            selected = c(
              "No Subsetting" = "all",
              "Top L order statistics (los)" = "los",
              "Percentage present (ppp)" = "ppp",
              "Complete" = "complete",
              "Rank invariant (rip)" = "rip",
              "Percentage present and rank invariant (ppp+rip)" = "ppp_rip"
            )
          ),
          
          uiOutput(paste0(tab, "_spans_subset_fn_param_box")),
          hr(),
          
          # Start button, disabled while inputs are incomplete
          uiOutput(paste0(tab, "_spans_start"))
        )
        
        return(do.call(tagList, out))
      } else {
        ### This should never happen given the req for loading this UI, but just in case
        return(
          strong("SPANS is only available for proteomic data types")
        )
      }
    }),
    
    ## Subset Parameter UI for Global normalization
    
    output[[paste0(tab, "_spans_subset_fn_param_box")]] <- renderUI({
      if (any(c("los", "rip", "ppp", "ppp_rip") %in% input[[paste0(tab, "_spans_subset_fn")]])) {
        return(
          tagList(
            # Conditional settings for subset parameters
            uiOutput(paste0(tab, "_los_param")),
            uiOutput(paste0(tab, "_ppp_param")),
            uiOutput(paste0(tab, "_rip_param"))
          )
        )
      } else {
        return()
      }
    }),
    
    ## Spans inputs to test
    map2(c("_los_param", "_ppp_param", "_rip_param"),
         c("_spans_los", "_spans_ppp", "_spans_rip"),
         populate_global_options,
         tab = tab
    ),
    
    ## Main Panel SPANS box ##
    output[[paste0(tab, "_normalize_mainpanel_spans")]] <- renderUI({
      if (is.null(input[[paste0(tab, "_normalize_option")]])) {
        return()
      } else if (input[[paste0(tab, "_normalize_option")]] == "SPANS - Proteomics only") {
        collapseBoxGroup(
          id = paste0(tab, "_spans_mainpanel"),
          
          collapseBox(
            "SPANS Results",
            value = "spans_mainpanel",
            collapsed = F,
            withSpinner(uiOutput(paste0(tab, "_spans_res_UI")))
          )
        )
      }
    }),
    
    
    output[[paste0(tab, "_spans_res_UI")]] <- renderUI({
      if (tab %in% c("Prodata", "Pepdata", "Isobaricpepdata") && !is.null(SPANS_res[[tab]])) {
        return(
          tabsetPanel(
            tabPanel(
              "Plot",
              br(),
              withSpinner(plotlyOutput(paste0(tab, "_spans_plot")))
            ),
            tabPanel(
              "Table",
              br(),
              withSpinner(DTOutput(paste0(tab, "_spans_table"))),
              br(),
              actionButton(paste0(tab, "_use_spans"), "Load Selected Result Parameters")
            )
          )
        )
      } else if (tab %in% c("Prodata", "Pepdata", "Isobaricpepdata")) {
        return(strong("Please run SPANS for results"))
      } else {
        return(strong("SPANS is only available for proteomic data types"))
      }
    }),
    
    
    #### Inputs ####
    
    ## Norm option picker ##
    
    output[[paste0(tab, "_normalize_option_UI")]] <- renderUI({
      
      req(!is.null(isolate(omicsData$objPP)))
      
      # input$complete_filters

      norm_choices <- c(
        "SPANS - Proteomics only",
        "Global Normalization",
        # "Loess Normalization",
        "No Normalization"
      )
      
      ### Pre-Normalized data w/ disabled UI
      if (!is.null(get_data_norm(isolate(omicsData$objPP))) &&
          length(get_data_norm(isolate(omicsData$objPP))) > 0 &&
          get_data_norm(isolate(omicsData$objPP))) {
        out <- list(
          strong("Data has already been normalized!"),
          br(),
          
          disabled(pickerInput(paste0(tab, "_normalize_option"),
                               label = "",
                               choices = norm_choices,
                               selected = "No Normalization",
                               multiple = TRUE,
                               options = pickerOptions(maxOptions = 1)
          ))
        )
        
        return(do.call(tagList, out))
      }
      
      
      ## Isobaric data
      if (tab == "Isobaric") {
        choicesOpt_norm <- NULL
        
        ## Proteomic data
      } else if (tab %in% c("Prodata", "Pepdata")) {
        choicesOpt_norm <- list(disabled = norm_choices == "No Normalization")
        
        ## NMR data
      } else if (tab == "Nmrdata") {
        choicesOpt_norm <- list(disabled = norm_choices == "SPANS - Proteomics only")
        
        # Everything else
      } else {
        choicesOpt_norm <- list(disabled = norm_choices %in% c("SPANS - Proteomics only", "No Normalization"))
      }
      
      return(
        pickerInput(paste0(tab, "_normalize_option"),
                    label = "Select a type of normalization:",
                    choices = norm_choices,
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1),
                    choicesOpt = choicesOpt_norm
        )
      )
    }),
    
    ## Buttons
    output[[paste0(tab, "_spans_start")]] <- renderUI({
      
      # out <- list()
      input[[paste0(tab, "_clear_input_spans")]]
      
      if (
        !is.null(input[[paste0(tab, "_spans_subset_fn")]]) &&
        !is.null(input[[paste0(tab, "_spans_norm_fn")]]) &&
        length(c(input[[paste0(tab, "_spans_subset_fn")]], input[[paste0(tab, "_spans_norm_fn")]])) > 2 &&
        (!("los" %in% input[[paste0(tab, "_spans_subset_fn")]]) ||
         (
           !is.null(input[[paste0(tab, "_spans_los")]]) ||
           !any(is.na(text_parser(input[[paste0(tab, "_spans_los_other")]])))
         )
        ) &&
        (!(any(c("rip", "ppp_rip") %in% input[[paste0(tab, "_spans_subset_fn")]])) ||
         (
           !is.null(input[[paste0(tab, "_spans_rip")]])) ||
         !any(is.na(text_parser(input[[paste0(tab, "_spans_rip_other")]])))
        ) &&
        (!(any(c("ppp", "ppp_rip") %in% input[[paste0(tab, "_spans_subset_fn")]])) ||
         (
           !is.null(input[[paste0(tab, "_spans_ppp")]]) ||
           !any(is.na(text_parser(input[[paste0(tab, "_spans_ppp_other")]])))
         )
        )
      ) {
        return(actionButton(paste0(tab, "_spans_start_button"), "Run SPANS"))
      } else {
        return(disabled(actionButton(paste0(tab, "_spans_start_button"), "Run SPANS")))
      }
    }),
    
    ## Gobal Normalization parameters  ##
    
    output[[paste0(tab, "_subset_params")]] <- renderUI({
      if (!is.null(input[[paste0(tab, "_subset_fn")]])) {
        if ("los" == input[[paste0(tab, "_subset_fn")]]) {
          return(
            numericInput(
              paste0(tab, "_los"),
              "Proportion of Top order statistics Percentage (los)",
              value = 0.05,
              min = 0,
              max = 1,
              step = 0.1
            )
          )
        } else if ("ppp" == input[[paste0(tab, "_subset_fn")]]) {
          return(
            numericInput(
              paste0(tab, "_ppp"),
              "Proportion of Percentage present (ppp)",
              value = 0.5,
              min = 0,
              max = 1,
              step = 0.1
            )
          )
        } else if ("rip" == input[[paste0(tab, "_subset_fn")]]) {
          return(
            numericInput(
              paste0(tab, "_rip"),
              "Rank invariance p-value (rip) p-value",
              value = 0.2,
              min = 0,
              max = 1,
              step = 0.1
            )
          )
        } else if ("ppp_rip" == input[[paste0(tab, "_subset_fn")]]) {
          return(
            div(
              id = "subset_params",
              
              numericInput(
                paste0(tab, "_ppp"),
                "Percentage present (ppp)",
                value = 0.5,
                min = 0,
                max = 1,
                step = 0.1
              ),
              
              numericInput(
                paste0(tab, "_rip"),
                "Rank invariance p-value (rip)",
                value = 0.2,
                min = 0,
                max = 1,
                step = 0.1
              )
            )
          )
        } else {
          return()
        }
      } else {
        return()
      }
    }),
    
    output[[paste0(tab, "_norm_buttons")]] <- renderUI({
      input[[paste0(tab, "_spans_start_button")]]
      
      req(!is.null(input[[paste0(tab, "_normalize_option")]]))
      
      lock <- prettySwitch(paste0(tab, "_lock_norm"), "Add/Remove", value = FALSE)
      # preview <- actionButton(paste0(tab, "_preview_normalization"), "Preview")
      bias <- actionButton(paste0(tab, "_inspect_norm"), "Evaluate Normalization Bias")
      tooltip <- div(
        id = paste0(tab, "_norm_buttons_icon"), style = "color:deepskyblue;display:inline-block",
        add_prompt(icon("question-sign", lib = "glyphicon"), message = ttext[["NORM_BUTTON_DISABLED"]])
      )
      
      ### If added; take out later if not ##################################################
      # cond_loess <- !is.null(input[[paste0(tab, "_normalize_option")]]) &&
      #   input[[paste0(tab, "_normalize_option")]] == "Loess Normalization" &&
      #   !is.null(input[[paste0(tab, "_loess_method")]]) &&
      #   !is.null(input[[paste0(tab, "_loess_span")]])
      
      cond_global <- !is.null(input[[paste0(tab, "_normalize_option")]]) &&
        input[[paste0(tab, "_normalize_option")]] %in%
        c("Global Normalization", "SPANS - Proteomics only") &&
        !is.null(input[[paste0(tab, "_subset_fn")]]) &&
        !is.null(input[[paste0(tab, "_norm_fn")]]) &&
        !is.null(input[[paste0(tab, "_backtransform")]])
      
      if ( # !cond_loess &&
        !cond_global) {
        lock <- disabled(lock)
        # preview <- disabled(preview)
        bias <- disabled(bias)
      } else {
        tooltip <- hidden(tooltip)
      }
      
      
      # if (input[[paste0(tab, "_normalize_option")]] == "Loess Normalization") {
      #   out <- list(
      #     preview,
      #     br(),
      #     br(),
      #     lock
      #   )
      # } else {
      out <- list(
        bias,
        # preview,
        tooltip,
        br(),
        br(),
        lock
      )
      # }
      
      return(tagList(out))
    }),
    
    
    ##### Outputs #####
    
    ## SPANS results ##
    
    #'@details SPANS plot
    output[[paste0(tab, "_spans_plot")]] <- renderPlotly({
      
      req(!is.null(SPANS_res[[tab]]))
      
      p <- plot(SPANS_res[[tab]], interactive = T)
      isolate(plots[[tab]][[paste0(tab, "_Normalization_spans_plot")]] <- p)
      
      p
    }),
    
    #'@details Table of SPANS scores
    output[[paste0(tab, "_spans_table")]] <- renderDT({
      datatable(SPANS_res[[tab]],
                options = list(scrollX = TRUE),
                selection = "single",
                colnames = c(
                  "Subset method" = "subset_method",
                  "Normalization method" = "normalization_method",
                  "SPANS score" = "SPANS_score",
                  "Parameters" = "parameters",
                  "Biomolecules used" = "mols_used_in_norm",
                  "Viable option" = "passed_selection"
                )
      )
    }),
    
    output[[paste0(tab, "_normalized_boxplots_pre")]] <- renderPlotly({ #### Is the group column name saved somewhere???
      
      req(input[["normalized"]] == "Yes" ||  get_data_norm(isolate(omicsData$objPP)) == F, cancelOutput = T)
      
      e_data <- isolate(omicsData$objPP)$e_data
      e_data_cname <- pmartR::get_edata_cname(isolate(omicsData$objPP))
      plot_data <- melt(e_data, id = e_data_cname, na.rm = TRUE)
      group_df <- get_group_DF(isolate(omicsData$objPP))
      
      if(!is.null(group_df)){
        
        plot_data <- left_join(plot_data, group_df, by = c("variable" = colnames(group_df)[1]))
        plot_data <- arrange(plot_data, !!rlang::sym(colnames(group_df)[2]))
        plot_data$variable <- factor(plot_data$variable, levels = unique(plot_data$variable))
        color <- plot_data[[colnames(group_df)[2]]]
        
      } else {
        color <- NULL
      }
      
      title <- paste0("Un-Normalized: ", tab, " Data")
      
      p <- plot_ly(
        data = plot_data,
        x = plot_data$variable,
        y = plot_data$value,
        color = color,
        type = "box"
      ) %>%
        layout(
          title = title,
          xaxis = list(title = "Samples"),
          yaxis = list(title = "Values")
        )
      
      isolate(plots[[tab]][[paste0(tab, "_Normalization_boxplots_pre")]] <- p)
      
      p
    }),
    
    # Post Norm
    output[[paste0(tab, "_normalized_boxplots_post_UI")]] <- renderUI({
      if (
        # if add or preview happened AND != "No norm"
        # (((!is.null(norm_settings[[tab]])) ||
        #  (!is.null(input[[paste0(tab, "_preview_normalization")]]) &&
        #   input[[paste0(tab, "_preview_normalization")]] > 0)) &&
        # (!is.null(norm_settings[[tab]]))) &&
        # !is.null(input[[paste0(tab, "_normalize_option")]]) &&
        # input[[paste0(tab, "_normalize_option")]] != "No Normalization"
        !is.null(omicsData$objNorm)
      ) {
        return(plotlyOutput(paste0(tab, "_normalized_boxplots_post")))
      } else {
        return(textOutput(paste0(tab, "_normalized_boxplots_post_null")))
      }
    }),
    
    output[[paste0(tab, "_normalized_boxplots_post_null")]] <- renderText({
      if (!is.null(input[[paste0(tab, "_normalize_option")]]) &&
          input[[paste0(tab, "_normalize_option")]] == "No Normalization") {
        return("No normalization applied")
      } else {
        return("Please select normalization options and 'Add' normalization to see normalization effect")
      }
    }),
    
    output[[paste0(tab, "_normalized_boxplots_post")]] <- renderPlotly({
      
      req(input[["normalized"]] == "Yes" ||  get_data_norm(isolate(omicsData$objPP)) == F, cancelOutput = T)

      # isolate({
      #   if (input[[paste0(tab, "_normalize_option")]] %in% c("Global Normalization", "SPANS - Proteomics only")) {
      #     params <- get_params(subset_fn = input[[paste0(tab, "_subset_fn")]])
      #     
      #     arg <- list(
      #       settings = input[[paste0(tab, "_normalize_option")]],
      #       subset_fn = input[[paste0(tab, "_subset_fn")]],
      #       norm_fn = input[[paste0(tab, "_norm_fn")]],
      #       params = params,
      #       backtransform = as.logical(input[[paste0(tab, "_backtransform")]])
      #     )
      #     
      #     req(!any(map_lgl(c(arg$subset_fn, arg$norm_fn, arg$backtransform), is.null)))
      #     
      #     norm_object <- check_norm_possible(
      #       omicsData = isolate(omicsData$objPP),
      #       subset_fn = arg$subset_fn,
      #       norm_fn = arg$norm_fn,
      #       params = arg$params,
      #       backtransform = arg$backtransform,
      #       apply_norm = TRUE
      #     )
      #     
      #     req(!is.null(norm_object), cancelOutput = T)
      #     # } else {
      #     #   arg <- list(
      #     #     settings = input[[paste0(tab, "_normalize_option")]],
      #     #     method = input[[paste0(tab, "_loess_method")]],
      #     #     span = input[[paste0(tab, "_loess_span")]]
      #     #   )
      #     #
      #     #   req(!any(map_lgl(c(arg$settings, arg$method, arg$span), is.null)))
      #     #
      #     #   norm_object <- normalize_loess(
      #     #     omicsData = isolate(omicsData$objPP),
      #     #     method = arg$method,
      #     #     span = arg$span
      #     #   )
      #   }
      # })
      
      norm_object <- omicsData$objNorm
      
      e_data <- norm_object$e_data
      e_data_cname <- pmartR::get_edata_cname(norm_object)
      plot_data <- melt(e_data, id = e_data_cname, na.rm = TRUE)
      group_df <- get_group_DF(norm_object)
      
      if(!is.null(group_df)){
        
        plot_data <- left_join(plot_data, group_df, by = c("variable" = colnames(group_df)[1]))
        plot_data <- arrange(plot_data, !!rlang::sym(colnames(group_df)[2]))
        plot_data$variable <- factor(plot_data$variable, levels = unique(plot_data$variable))
        color <- plot_data[[colnames(group_df)[2]]]
        
      } else {
        color <- NULL
      }
      
      title <- paste0("Normalized: ", tab, " Data")
      
      p <- plot_ly(
        data = plot_data,
        x = plot_data$variable,
        y = plot_data$value,
        color = color,
        type = "box"
      ) %>%
        layout(
          title = title,
          xaxis = list(title = "Samples"),
          yaxis = list(title = "Values")
        )
      
      isolate(plots[[tab]][[paste0(tab, "_Normalization_boxplots_post")]] <- p)
      
      p
    })
  )
  
  ###
  
  outputOptions(output, paste0(tab, "_normalize_sidepanel"), suspendWhenHidden = FALSE)
  outputOptions(output, paste0(tab, "_normalize_option_UI"), suspendWhenHidden = FALSE)
}



