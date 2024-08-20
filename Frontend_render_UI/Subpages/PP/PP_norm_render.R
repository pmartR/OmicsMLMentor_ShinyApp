#' ## This needs to be modified for the overlord <- -- must skip this section

SPANS_res <- reactiveValues()

subset_conv <- function(slData) {
  
  na_trf_df = attributes(slData)$na_transform
  
  if (!is.null(na_trf_df)) {
    nonconverted_biomols <- na_trf_df %>% 
      dplyr::filter(Handling != "Convert") %>% 
      dplyr::filter(Handling != "Remove") %>% 
      dplyr::pull(get_edata_cname(slData))
    
    nonconverted_biomols <- nonconverted_biomols[
      (nonconverted_biomols %in% slData$e_data[[get_edata_cname(slData)]])]
    
    if (length(nonconverted_biomols) > 0 && 
        length(nonconverted_biomols) != nrow(slData$e_data)) {
      cfilt <- pmartR::custom_filter(slData, e_data_remove = nonconverted_biomols)
      slData <- pmartR::applyFilt(cfilt, slData)
    } else if (length(nonconverted_biomols) == nrow(slData$e_data)){
      warning("No molecules have been converted. Returning NULL.")
      return(NULL)
    }
  } else {
    warning("No na_transform attribute found. Returning original slData object.")
  }
  
  return(slData)
}

subset_noconv <- function(slData) {
  na_trf_df = attributes(slData)$na_transform

  if (!is.null(na_trf_df)) {
    converted_biomols <- na_trf_df %>% 
      dplyr::filter(Handling == "Convert") %>% 
      dplyr::pull(get_edata_cname(slData))
    
    converted_biomols <- converted_biomols[(
      converted_biomols %in% slData$e_data[[get_edata_cname(slData)]])]
    
    if (length(converted_biomols) > 0) {
      cfilt <- pmartR::custom_filter(slData, e_data_remove = converted_biomols)
      slData <- pmartR::applyFilt(cfilt, slData)
    }
  } else {
    warning("No na_transform attribute found. Returning original slData object.")
  }
  
  return(slData)
}

combine_omicsdata <- function(obj_1, obj_2){
  
  new_edata_cname <- get_edata_cname(obj_1)
  new_emeta_cname <- get_emeta_cname(obj_1)
  
  # bind the two data frames
  new_edata <- dplyr::bind_rows(
    obj_1$e_data,
    obj_2$e_data %>%
      dplyr::rename(setNames(
        get_edata_cname(obj_2),
        get_edata_cname(obj_1)
      ))
  )
  
  molnames <- new_edata[, get_edata_cname(obj_1)]
  
  if (length(molnames) != length(unique(molnames))) {
    warning("Duplicate molecule identifiers were found in your combined data.")
  }
  
  # Combined fdata will keep all columns from the first dataset in the case of
  # duplicates.
  obj_1_fdata_colnames <- obj_1$f_data %>%
    dplyr::select(-dplyr::one_of(get_fdata_cname(obj_1))) %>%
    colnames()
  
  if (is.null(omicsData$objNorm)) {
    updatePrettySwitch(session, paste0(tab, "_lock_norm"), value = F)
    return(NULL)
  }
  
  new_fdata <- obj_1$f_data %>%
    dplyr::left_join(
      dplyr::select(obj_2$f_data, -dplyr::one_of(obj_1_fdata_colnames)),
      by = setNames(get_fdata_cname(obj_2), get_fdata_cname(obj_1))
    )
  
  # Combine e_meta in the same way as e_data if it exists in both datasets.
  if (!is.null(obj_1$e_meta) & !is.null(obj_2$e_meta)) {
    new_emeta_cname = get_emeta_cname(obj_1)
    
    new_emeta <- dplyr::bind_rows(
      obj_1$e_meta,
      obj_2$e_meta %>%
        dplyr::rename(setNames(
          get_emeta_cname(obj_2),
          get_emeta_cname(obj_1)
        ))
    )
    
    # Check and warn about non-unique e_meta identifiers, this is pre-empting a
    # situation where this function can take objects with pepData-like e_meta.
    new_emeta_ids = new_emeta[, new_emeta_cname]
    emeta_ids_1 = obj_1$e_meta[, get_emeta_cname(obj_1)]
    emeta_ids_2 = obj_2$e_meta[, get_emeta_cname(obj_2)]
    
    if (length(unique(new_emeta_ids)) !=
        length(unique(emeta_ids_1)) + length(unique(emeta_ids_2))) {
      if (drop_duplicate_emeta) {
        warning(
          "There were non-unique molecule identifiers in e_meta, dropping these duplicates, some meta-data information may be lost."
        )
        new_emeta <-
          new_emeta %>% dplyr::distinct(!!dplyr::sym(new_edata_cname), .keep_all = TRUE)
      } else {
        warning(
          "There were non-unique molecule identifiers in e_meta, this may cause the object construction to fail if edata_cname and emeta_cname do not specify unique rows in the combined e_meta"
        )
      }
    }
  } else {
    new_emeta_cname = new_emeta = NULL
  }
  
  # Construct the new object using the appropriate type.
  obj_class <- class(obj_1)
  obj_class <- obj_class[obj_class != "slData"]
  if(length(obj_class) > 1 && "pepData" %in% obj_class){
    ## should be ref normed at this point
    obj_class <- "pepData"
  }
  
  constructor_fn <- get(sprintf("as.%s", obj_class))
  
  new_object <- constructor_fn(
    e_data = new_edata,
    edata_cname = new_edata_cname,
    f_data = new_fdata,
    fdata_cname = get_fdata_cname(obj_1),
    e_meta = new_emeta,
    emeta_cname = new_emeta_cname,
    data_scale = get_data_scale(obj_1),
    is_normalized = get_data_norm(obj_1)
  )
    
    attr(new_object, "filters") = attr(obj_1, "filters")
  
  # Set the group designation of the new objects
  if (!is.null(attr(obj_1, "group_DF"))) {
    
    # check that main effects are functionally the same
    n_orig_groups <- attr(obj_1, "group_DF") %>%
      dplyr::group_by(Group) %>%
      attributes() %>%
      `[[`("groups") %>%
      nrow()
    
    n_combined_groups <- attr(obj_1, "group_DF") %>%
      dplyr::left_join(
        attr(obj_2, "group_DF"),
        by = setNames(get_fdata_cname(obj_2), get_fdata_cname(obj_1))
      ) %>%
      dplyr::group_by(Group.x, Group.y) %>%
      attributes() %>%
      `[[`("groups") %>%
      nrow()
    
    if (n_orig_groups != n_combined_groups) {
      stop("The main effect structures of the two omicsData objects were not identical.")
    }
    
    ## check covariates ##
    # 1. Rename covariates in each fdata to a temp name
    # 2. Join the two f_datas into a dataframe with the rename covariates
    # 3. Group by the just the first objects covariates and then both the first and second,
    # the number of groups should be the same in both cases if the covariate structure
    # is the same.  If not, throw an error.
    # 4. Run group_designation on the combined object with the first object's main effects/covariates.
    covariates_1 <- attr(obj_1, "group_DF") %>%
      attributes() %>%
      `[[`("covariates") %>%
      {
        `[`(., -which(colnames(.) == get_fdata_cname(obj_1)))
      } %>%
      colnames()
    
    covariates_2 <- attr(obj_2, "group_DF") %>%
      attributes() %>%
      `[[`("covariates") %>%
      {
        `[`(., -which(colnames(.) == get_fdata_cname(obj_2)))
      } %>%
      colnames()
    
    if (all(!sapply(list(covariates_1, covariates_2), is.null))) {
      # renaming ...
      tmp_covar_names_1 <- paste0("_COVARS_1_", 1:length(covariates_1))
      tmp_covar_names_2 <- paste0("_COVARS_2_", 1:length(covariates_2))
      
      rename_map_1 <- setNames(covariates_1, tmp_covar_names_1)
      rename_map_2 <- setNames(covariates_2, tmp_covar_names_2)
      
      tmp_fdata1 <- obj_1$f_data %>%
        dplyr::rename(!!!rename_map_1)
      tmp_fdata2 <- obj_2$f_data %>%
        dplyr::rename(!!!rename_map_2)
      
      # ... to perform a join ...
      combined_fdatas <- tmp_fdata1 %>%
        dplyr::left_join(
          tmp_fdata2,
          by = setNames(get_fdata_cname(obj_2), get_fdata_cname(obj_1))
        )
      
      # ... and check that both objects have the same covariate structure.
      n_orig_covariate_levels_1 <- combined_fdatas %>%
        dplyr::group_by(
          dplyr::across(dplyr::one_of(tmp_covar_names_1))
        ) %>%
        attributes() %>%
        `[[`("groups") %>%
        nrow()
      
      n_orig_covariate_levels_2 <- combined_fdatas %>%
        dplyr::group_by(
          dplyr::across(dplyr::one_of(tmp_covar_names_2))
        ) %>%
        attributes() %>%
        `[[`("groups") %>%
        nrow()
      
      n_comb_covariate_levels <- combined_fdatas %>%
        dplyr::group_by(
          dplyr::across(dplyr::one_of(c(tmp_covar_names_1, tmp_covar_names_2)))
        ) %>%
        attributes() %>%
        `[[`("groups") %>%
        nrow()
      
      if (n_orig_covariate_levels_1 != n_comb_covariate_levels |
          n_orig_covariate_levels_2 != n_comb_covariate_levels) {
        stop("The covariate structure of both omicsData objects was not identical.")
      }
    }
    
    main_effects <- attr(obj_1, "group_DF") %>%
      attributes() %>%
      `[[`("main_effects")
    
    message(sprintf(
      "Grouping new object with main effects: %s.%s",
      paste(main_effects, collapse = ", "),
      if (is.null(covariates_1))
        ""
      else
        sprintf("  Covariates: %s", paste(covariates_1, collapse = ", "))
    ))
    
    new_object <- group_designation(
      new_object,
      main_effects = main_effects,
      covariates = covariates_1
    )
  }
  
  return(new_object)
}

observeEvent(omicsData$objPP, {
  req(!is.null(isolate(omicsData$objPP)))
  
  isolate(nm <- get_omicsData_type(omicsData$objPP))
  
  output$norm_tab <- renderUI({
    norm_tab(nm)
  })
  
  load_norm_observers(nm)
  assign_norm_output(nm)
}, once = T)


#' ##### Function for glabal normalization #####
#' # Grab params for global normalization
get_params <- function(subset_fn, tab) {

  req(!is.null(subset_fn))

  # Grab params
  params <- list()
  if ("los" == subset_fn) {
    params[["los"]] <- as.numeric(input[[paste0(tab, "_los")]])
  } else if ("ppp" == subset_fn) {
    params[["ppp"]] <- as.numeric(input[[paste0(tab, "_ppp")]])
  } else if ("rip" == subset_fn) {
    params[["rip"]] <- as.numeric(input[[paste0(tab, "_rip")]])
  } else if ("ppp_rip" == subset_fn) {
    rip <- as.numeric(input[[paste0(tab, "_rip")]])

    ppp <- as.numeric(input[[paste0(tab, "_ppp")]])

    params[["ppp_rip"]] <- list(rip = rip, ppp = ppp)
  } else {
    params <- NULL
  }
  return(params)
}

#' # Evaluate norm bias for global normalization
inspect_norm <- function(omicsData, subset_fn, norm_fn, params, backtransform) {
  group_df <- attr(omicsData, "group_DF")
  reorder <- match(
    colnames(omicsData$e_data)[
      -which(colnames(omicsData$e_data) == pmartR::get_edata_cname(omicsData))],
    as.character(group_df[, get_fdata_cname(omicsData)])
  )
  group <- group_df[reorder, ]$Group
  
  if(norm_fn == "Zero-to-one scaling"){
    norm_object <- pmartR:::zero_one_scale(omicsData$e_data,
                                           get_edata_cname(omicsData))
    params <- norm_object$norm_params
  } else {
    
    # create norm object and pull normalization parameters
    norm_object <- check_norm_possible(omicsData,
                                       subset_fn,
                                       norm_fn,
                                       params = params,
                                       apply_norm = FALSE,
                                       backtransform = backtransform
    )
    
    if (is.null(norm_object)) {
      return(NULL)
    }
    params <- norm_object$parameters$normalization
    
  }

  if (!is.null(params$location)) {
  # p value and dataframe of normalization factors for location
  p_location <- pmartR:::kw_rcpp(matrix(params$location, nrow = 1),
                                 group = as.character(group)
  )
  loc_params <- stack(params$location) %>%
    rename("VAL__" = values) # very possible someone has the column name 'values' in their data, so rename
  } else {
    p_location <- NULL
  }

  # p value and dataframe of normalization factors for scale, if there are scale parameters
  if (!is.null(params$scale)) {

    if(norm_fn == "Zero-to-one scaling"){
      p_scale <- pmartR:::kw_rcpp(matrix(params$scale[2,] - params$scale[1,], 
                                         nrow = 1),
                                  group = as.character(group)
      )
      scale_params <- data.frame(
        VAL__ = params$scale[2,] - params$scale[1,],
        ind = colnames(params$scale)
      )
      
    } else {
      p_scale <- pmartR:::kw_rcpp(matrix(params$scale, nrow = 1),
                                  group = as.character(group)
      )
      scale_params <- stack(params$scale) %>%
        rename("VAL__" = values)
    }
    
  } else {
    p_scale <- NULL
  }

  #### Create Plots ####

  # plot by group if the object has group assignments
  if (!is.null(attributes(omicsData)$group_DF)) {
    
    if (!is.null(params$location)) {
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
    } else loc_boxplot <- NULL

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
    if (!is.null(params$location)) {
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
    }

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


### Reactive val for triggering extra warning
warn_user_bias <- reactiveVal()

#' 
#' #'@details Function which assigns the observers for unassigned data types
#' #'for the normalization tab
load_norm_observers <- function(tab) {

    if(tab %in% c("Prodata","Pepdata",  "Isobaricpepdata")){
      ## SPANS
      ### Run spans ###
      observeEvent(input[[paste0(tab, "_spans_start_button")]], {
        
        req(input[[paste0(tab, "_spans_start_button")]] > 0 &&
              input$user_level_pick == "beginner" ||
              (!is.null(input[[paste0(tab, "_spans_norm_fn")]]) &&
              !is.null(input[[paste0(tab, "_spans_subset_fn")]])))

        shinyjs::show("SPANS_busy")

        UI_elements <- paste0(tab, c(
          "_preview_normalization",
          "_lock_norm",
          "_inspect_norm",
          "_spans_start_button",
          "_normalize_option"
        ))

        map(UI_elements, disable)

        hide(selector = ".navbar-nav a")

        on.exit({
          # enable(paste0(tab, "_spans_start_button"))
          shinyjs::show(selector = ".navbar-nav a")
          # removeNotification(paste0(tab, "_SPANS_note"))
          map(UI_elements, enable)
          updateBoxCollapse(session, id = "normalization_picker", 
                            close = "picker")
          updateBoxCollapse(session, paste0(tab, "_normalization_sidebar"), 
                            open = "choose_params",
                            close = "use_spans")
          updateBoxCollapse(session, id = "normalization_mainpanel", 
                            open = "spans_mainpanel",
                            close = "normdata_mainpanel")
          hide("SPANS_busy")

        })

        updateBoxCollapse(session, paste0(tab, "_normalization_sidebar"), close = "choose_params")
        updateBoxCollapse(session, paste0(tab, "_normalization_mainpanel"), close = "normdata_mainpanel")
        updateBoxCollapse(session, paste0(tab, "_spans_mainpanel"), open = "spans_mainpanel")

        noconv <- subset_noconv(as.slData(omicsData$objPP))
        
        if(input$user_level_pick == "beginner"){
          SPANS_res[[tab]] <- spans_procedure(noconv)
        } else {
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
            noconv,
            norm_fn = input[[paste0(tab, "_spans_norm_fn")]],
            subset_fn = input[[paste0(tab, "_spans_subset_fn")]],
            params = params
          )
        }
        
        isolate(table_table_current$table$PP__SPANS <- SPANS_res[[tab]])
        
      })

      ### SPANS defaults ###
      observeEvent(input[[paste0(tab, "_load_default_spans")]], {
        updatePickerInput(session, paste0(tab, "_spans_norm_fn"), selected = c(
          "Mean" = "mean",
          "Median" = "median",
          "Z-norm" = "zscore",
          "Median Absolute Deviation" = "mad"
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
        req(!is.null(selected) && !is.null(SPANS_res))

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
      
      ## Populate Global normalization options from clicking the graph
      observeEvent(event_data("plotly_click", source = "SPANS"), {
        
        
        data <- event_data("plotly_click", source = "SPANS")
        norm_method <- data$x
        subset_method <- str_trim(gsub(" | .+", "", data$y))
        parameters <- str_trim(gsub(".+ | ", "", data$y))
        
        # updateBoxCollapse(session,
        #                   id = paste0(tab, "_normalization_sidebar"),
        #                   open = "choose_params"
        # )
        
        updatePickerInput(
          session,
          paste0(tab, "_subset_fn"),
          selected = subset_method
        )
        
        updatePickerInput(
          session,
          paste0(tab, "_norm_fn"),
          selected = norm_method
        )
        
        if (subset_method %in% c("los", "ppp", "rip")) {
          updateNumericInput(
            session,
            paste0(tab, "_", subset_method),
            value = parameters
          )
        } else if (subset_method == "ppp_rip") {
          opts <- str_split(parameters, ";")[[1]]
          
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
      req(input[[paste0(tab, "_inspect_norm")]] > 0 && 
            !get_data_norm(omicsData$objPP), cancelOutput = T)
      
      temp_dat <- omicsData$objPP
      
      if(is.null(temp_dat$f_data) || is.null(get_group_DF(temp_dat))){
        
        if(!is.null(temp_dat$f_data)){
          temp_dat$f_data$Temp_col_all <- "All"
        } else {
          temp_dat$f_data <- data.frame(
            SampleID = colnames(temp_dat$e_data)[
              colnames(temp_dat$e_data) != pmartR::get_edata_cname(temp_dat)],
            Temp_col_all = "All"
          )
        }
        temp_dat <- group_designation(temp_dat, "Temp_col_all")
        
      }

      if(input[[paste0(tab, "_normalize_option")]] == "Zero-to-one scaling"){
        
        params <- NULL
        
        eval <- inspect_norm(
          omicsData = temp_dat,
          subset_fn = NULL,
          norm_fn = "Zero-to-one scaling",
          params = NULL,
          backtransform = FALSE
        )
        
      } else {
        params <- get_params(subset_fn = input[[paste0(tab, "_subset_fn")]], tab)
        
        eval <- inspect_norm(
          omicsData = temp_dat,
          subset_fn = input[[paste0(tab, "_subset_fn")]],
          norm_fn = input[[paste0(tab, "_norm_fn")]],
          params = params,
          backtransform = as.logical(input[[paste0(tab, "_backtransform")]])
        )
      }

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
        msg <- if (!is.na(eval$p_location) && eval$p_location < 0.05) {
          warn_user_bias(T)
          div(style = "color:red", 
              paste0("There is evidence to suggest the centering effect of ",
                     "this normalization differs across groups, ",
                     "consider choosing another normalization."))
        } else {
          warn_user_bias(F)
          div(paste0("Weak or no evidence to suggest a difference",
                     " in the centering effect of this normalization ",
                     "across groups."))
        }

        insertTab(
          inputId = paste0(tab, "_normalize_boxplot_tabset"),
          target = "Normalization Preview",
          position = "after",
          select = T,
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
          p <- eval$loc_boxplot
          isolate(plot_table_current$table$PP__bias__location <- p)
          p
        })

        updateTabsetPanel(
          session,
          paste0(tab, "_normalize_boxplot_tabset"),
          "Location Bias"
        )
      }

      if (!is.null(eval$p_scale)) {
        msg <- if (!is.na(eval$p_scale) && eval$p_scale < 0.05) {
          warn_user_bias(T)
          div(style = "color:red", 
              "There is evidence to suggest the scaling effect of this normalization differs across groups, consider choosing another normalization.")
        }
        else {
          warn_user_bias(F)
          div("Weak or no evidence to suggest a difference in the scaling effect of this normalization across groups.")
        }

        insertTab(
          inputId = paste0(tab, "_normalize_boxplot_tabset"),
          target = "Normalization Preview",
          position = "after",
          select = T,
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
          p <- eval$scale_boxplot
          isolate(plot_table_current$table$PP__bias__scale <- p)
          p
        })

        updateTabsetPanel(
          session,
          paste0(tab, "_normalize_boxplot_tabset"),
          "Scale Bias"
        )
      }
      
      
      updateBoxCollapse(session, "normalization_mainpanel", 
                        open = "normdata_mainpanel")
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

      req(!is.null(input[[paste0(tab, "_normalize_option")]]))
      req(input$top_page == "Pre-processing")

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
        
        updateBoxCollapse(session, id = "normalization_picker", close = "picker")
        
        if(!is.null(warn_user_bias()) && warn_user_bias() &&
           input$user_level_pick == "beginner"){
          
          shinyalert("Are you sure?", 
                     "Analysis suggests that this normalization has bias!",
                     showCancelButton = T,
                     showConfirmButton = T,
                     
                     callbackR = function(x){
                       if(x){
                         warn_user_bias(F)
                         updatePrettySwitch(
                           inputId = paste0(tab, "_lock_norm"), value = T)
                       }
                     }
                     
          )
          
          updatePrettySwitch(inputId = paste0(tab, "_lock_norm"), value = F)
          return()
        }

        if (input[[paste0(tab, "_normalize_option")]] %in% 
            c("Global Normalization", "SPANS - Proteomics only")) {

          params <- get_params(subset_fn = input[[paste0(tab, "_subset_fn")]], tab)

          
          noconv <- subset_noconv(as.slData(omicsData$objPP))
          conv <- subset_conv(as.slData(omicsData$objPP))
          
          omicsData$objNorm <- check_norm_possible(noconv,
                                             subset_fn = input[[paste0(tab, "_subset_fn")]],
                                             norm_fn = input[[paste0(tab, "_norm_fn")]],
                                             params = params,
                                             apply_norm = TRUE,
                                             backtransform = as.logical(input[[paste0(tab, "_backtransform")]])
          )
          
          if(!is.null(conv) && !is.null(noconv) && !identical(noconv, conv)){
            
            omicsData$objNorm <- combine_omicsdata(omicsData$objNorm, conv)
          }

          norm_settings[[tab]] <- list(
            settings = input[[paste0(tab, "_normalize_option")]],
            subset_fn = input[[paste0(tab, "_subset_fn")]],
            norm_fn = input[[paste0(tab, "_norm_fn")]],
            params = params,
            backtransform = as.logical(input[[paste0(tab, "_backtransform")]])
          )
          } else if (input[[paste0(tab, "_normalize_option")]] == "Zero-to-one scaling"){
            
            omicsData$objNorm <- normalize_zero_one_scaling(omicsData$objPP)
            norm_settings[[tab]] <- list(
              settings = input[[paste0(tab, "_normalize_option")]]
            )
          } else {
            omicsData$objNorm <- normalize_loess(
              omicsData$objPP,
              method = input[[paste0(tab, "_loess_method")]],
              span = input[[paste0(tab, "_loess_span")]]
              )
            
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
        if(!get_data_norm(omicsData$objPP)){
          omicsData$objNorm <- NULL
          norm_settings[[tab]] <- NULL
          map(UI_elements, enable)
        }
      }
      
      isolate(table_table_current$table$PP__normalization <- omicsData$objNorm$e_data)
    })

    ## Reactions to other inputs ##

    # Remove bias tabs when new normalization option selected
    observeEvent(input[[paste0(tab, "_normalize_option")]], {
      
      req(!get_data_norm(omicsData$objPP), cancelOutput = T)

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
                   updatePickerInput(session, paste0(tab, "_subset_fn"), choices = 
                     if (is.na(supervised()) || supervised()) {
                       c(
                         "No subsetting" = "all",
                         "Top L order statistics (los)" = "los",
                         "Percentage present (ppp)" = "ppp",
                         "Complete" = "complete",
                         "Rank invariant (rip)" = "rip",
                         "Percentage present and rank invariant (ppp+rip)" = "ppp_rip"
                       )
                     } else {
                       c(
                         "No subsetting" = "all",
                         "Top L order statistics (los)" = "los",
                         "Complete" = "complete"
                       )
                   })
                   
                   # req(tab %in% ALL_DATATYPE_NAMES)
                   show_add_tooltip(
                     session, paste0(tab, "_norm_picker_icon"),
                     is.null(input[[paste0(tab, "_normalize_option")]]),
                     "Specify normalization to apply"
                   )
                 },
                 ignoreNULL = FALSE,
                 ignoreInit = FALSE
    )
    
    observeEvent(input[[paste0(tab, "_normalize_option")]], {
      req(
        (!is.null(omicsData$objPP)) && 
            (is.null(attr(omicsData$objPP,"data_info")$norm_info$norm_fn)) &&
          !get_data_norm(omicsData$objPP)
        )
      if (input[[paste0(tab, "_normalize_option")]] %in% c(
        "Global Normalization","SPANS - Proteomics only","No Normalization")) {
        # Display an alert when the value is "triggerValue"
        shinyalert(
          "Note", 
          paste0("Using this normalization method, users will not be",
                 " able export the model as an RDS object to run on new data.",
                 " To do that, please use 'Zero-to-one Scaling'."), type = "info")
      }
    })

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
    req(input$user_level_pick != "beginner")
    
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
      
      text_input <- if(input$user_level_pick == "expert"){
        textInput(paste0(tab, input_name, "_other"),
                  "Custom values (comma seperated):",
                  placeholder = "E.g. 0.025, 0.25, 0.66"
        )
      } else NULL
      
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
          
          text_input
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
                # icon_id = paste0(tab, "_spans_norm_icon"),
                # icon = icon("exclamation-sign", lib = "glyphicon"),
                # icon_hidden = F,
                value = "use_spans",
                collapsed = F,
                uiOutput(paste0(tab, "_spans_settings"))
              ),
              
              # Normalization parameters
              collapseBox(
                "Select Normalization",
                # icon_id = paste0(tab, "_apply_norm_icon"),
                # icon = icon("exclamation-sign", lib = "glyphicon"),
                # icon_hidden = F,
                value = "choose_params",
                
                pickerInput(
                  paste0(tab, "_norm_fn"),
                  "Normalization Function",
                  choices = c(
                    "Mean" = "mean",
                    "Median" = "median",
                    "Z-norm" = "zscore",
                    "Median Absolute Deviation" = "mad"
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
                # icon_id = paste0(tab, "_apply_norm_icon"),
                # icon = icon("exclamation-sign", lib = "glyphicon"),
                # icon_hidden = F,
                value = "choose_params",
                collapsed = F,
                
                pickerInput(
                  paste0(tab, "_norm_fn"),
                  "Normalization Function",
                  choices = c(
                    "Mean" = "mean",
                    "Median" = "median",
                    "Z-norm" = "zscore",
                    "Median Absolute Deviation" = "mad"
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
        } else if (input[[paste0(tab, "_normalize_option")]] == "Zero-to-one scaling") {
          return(
            div(
              collapseBoxGroup(
                id = paste0(tab, "_normalization_sidebar"),
                open = "choose_params",

                # Normalization parameters
                collapseBox(

                  # subsection_header(
                    "Apply Normalization",
                  #   paste0(tab, "_apply_norm_icon"),
                  #   "color:red;float:right",
                  #   icon("exclamation-sign", lib = "glyphicon")
                  # ),
                  value = "choose_params",
                  collapsed = F,
                  
                  br(),
                  strong("Warning: All NA values will be converted to zero."),
                  br(),
                  br(),
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
      
      picker_norm_fn <- pickerInput(
        paste0(tab, "_spans_norm_fn"),
        "Normalization Functions to Evaluate",
        choices = c(
          "Mean" = "mean",
          "Median" = "median",
          "Z-norm" = "zscore",
          "Median Absolute Deviation" = "mad"
        ),
        multiple = TRUE,
        selected = c(
          "Mean" = "mean",
          "Median" = "median",
          "Z-norm" = "zscore",
          "Median Absolute Deviation" = "mad"
        )
      )
      
      picker_subset_fn <- pickerInput(
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
      )
      
      cl_inputs <- actionButton(paste0(tab, "_clear_input_spans"), "Clear Inputs")
      ld_dft <- actionButton(paste0(tab, "_load_default_spans"), "Load Defaults")
      
      if(input$user_level_pick == "beginner"){
        
        picker_subset_fn <- div(
          "SPANS will be run using default settings.",
          br(),
          br(),
          "Normalization methods considered: Mean, Median, Z-norm, Median Absolute Deviation",
          br(),
          br(),
          paste0("Subsets considered for assessment:", 
                 " No subsetting,",
                 " subset to features with top abundances per sample (los),",
                 " subset to features consistently observed across samples (ppp), ",
                 "subset to features observed without exception across all samples,",
                 " subset to features observed without exception across all samples that have notable differences by prediction group (rip),",
                 " subset to features consistently observed across samples that have notable differences by prediction group (ppp_rip)"
                 ),
          br(),
          br(),
          "By default, the thresholds for proportion of observations required for the ppp method are 0.05, 0.1, 0.2, 0.3.",
          br(),
          "By default, the p-value thresholds for the notable differences by prediction group required for the rip method are 0.1, 0.15, 0.2, 0.25.",
          br(),
          "When used together in the ppp_rip method, the default ppp thresholds are 0.1, 0.25, 0.5, and 0.75 and the default rip thresholds are 0.1, 0.15, 0.2, 0.25."
        )
        
        picker_norm_fn <- NULL
        cl_inputs <- NULL
        ld_dft <- NULL
      }
      
      # Where proteomic, load spans options
      if (tab %in% c("Prodata", "Pepdata", "Isobaricpepdata") &&
          !is.null(omicsData$objPP$f_data)) {
        
        out <- list(
          "Statistical Procedure for the Analyses of peptide abundance Normalization Strategies (SPANS)",
          br(), br(),
          cl_inputs,
          ld_dft,
          hr(),
          picker_norm_fn,
          picker_subset_fn,
          
          uiOutput(paste0(tab, "_spans_subset_fn_param_box")),
          hr(),
          
          hidden(div("Running SPANS - procedure may take several minutes, please wait...",
                     id = "SPANS_busy",
                     class = "fadein-out",
                     style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
          )),
          
          # Start button, disabled while inputs are incomplete
          uiOutput(paste0(tab, "_spans_start"))
        )
        
        return(do.call(tagList, out))
      } else {
        ### This should never happen given the req for loading this UI, but just in case
        return(
          strong("Statistical Procedure for the Analyses of peptide abundance Normalization Strategies (SPANS) is only available for proteomic data types and requires Sample Information file.")
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
      if (tab %in% c("Prodata", "Pepdata", "Isobaricpepdata") && 
          !is.null(SPANS_res) &&
          !is.null(SPANS_res[[tab]])) {
        return(
          div(
            
            br(),
            
            "Best normalization(s) notated as methods with black dot(s) in the graph and first row(s) in the table.",
            br(),
            br(),
            
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
              actionButton(paste0(tab, "_use_spans"), "Load selected result parameters")
            )
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
      
      if(isolate(!get_data_norm(omicsData$objPP))) {
        omicsData$objNorm <- NULL
        norm_settings[[tab]] <- NULL
        map(UI_elements, enable)
      }
      
      # input$complete_filters

      norm_choices <- c(
        "Global Normalization",
        # "Loess Normalization",
        "Zero-to-one scaling",
        "No Normalization"
      )
      
      if (!is.na(supervised()) && supervised()) {
        norm_choices <- c("SPANS - Proteomics only", norm_choices)
      }
      
      ### Pre-Normalized data w/ disabled UI
      if (isolate(inherits(omicsData$objPP, "seqData"))) {
        out <- list(
          strong("Not available for RNA seq data"),
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
      
      ### Pre-Normalized data w/ disabled UI
      if (!is.null(get_data_norm(isolate(omicsData$objPP))) &&
          length(get_data_norm(isolate(omicsData$objPP))) > 0 &&
          get_data_norm(isolate(omicsData$objPP)) &&
          (is.null(input[[paste0(tab, "_normalize_option")]]) ||
          input[[paste0(tab, "_normalize_option")]] == "No Normalization")) {
        browser()
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
      
      if(isolate(all(unlist(omicsData$objPP$e_data[-1]) %in% c(0, 1)))) {
        
        out <- list(
          strong("Normalization unavailable for Presence/absence data."),
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
      
      cond <- 
        ## Beginners don't need inputs
        input$user_level_pick == "beginner" ||
        
        (
        ## All the options are present
        !is.null(input[[paste0(tab, "_spans_subset_fn")]]) &&
        !is.null(input[[paste0(tab, "_spans_norm_fn")]]) &&
        length(c(input[[paste0(tab, "_spans_subset_fn")]], 
                 input[[paste0(tab, "_spans_norm_fn")]])) > 2 &&
        
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
        ))
      
      
      if (cond) {
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
      bias <- if(input$pick_model_EM %in% models_supervised){
        actionButton(paste0(tab, "_inspect_norm"), "Evaluate Normalization Bias")
      } else NULL
      
      
      tooltip <- div(
        id = paste0(tab, "_norm_buttons_icon"), style = "color:deepskyblue;display:inline-block",
        add_prompt(icon("question-sign", lib = "glyphicon"), message = ttext[["NORM_BUTTON_DISABLED"]])
      )
      
      ### If added; take out later if not ##################################################
      # cond_loess <- !is.null(input[[paste0(tab, "_normalize_option")]]) &&
      #   input[[paste0(tab, "_normalize_option")]] == "Loess Normalization" &&
      #   !is.null(input[[paste0(tab, "_loess_method")]]) &&
      #   !is.null(input[[paste0(tab, "_loess_span")]])
      
      cond_zero_one <- !is.null(input[[paste0(tab, "_normalize_option")]]) &&
        input[[paste0(tab, "_normalize_option")]] == "Zero-to-one scaling"
      
      cond_global <- !is.null(input[[paste0(tab, "_normalize_option")]]) &&
        input[[paste0(tab, "_normalize_option")]] %in%
        c("Global Normalization", "SPANS - Proteomics only") &&
        !is.null(input[[paste0(tab, "_subset_fn")]]) &&
        !is.null(input[[paste0(tab, "_norm_fn")]]) &&
        !is.null(input[[paste0(tab, "_backtransform")]])
      
      if ( # !cond_loess &&
        !cond_zero_one &&
        !cond_global) {
        lock <- disabled(lock)
        # preview <- disabled(preview)
        if(!is.null(bias))
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
      
      fluidRow(
        column(6,
               bias,
               # preview,
               tooltip,
               ),
        column(6, lock)
      )
      
      # out <- list(
      #   bias,
      #   # preview,
      #   tooltip,
      #   br(),
      #   br(),
      #   lock
      # )
      # }
      
      # return(tagList(out))
    }),
    
    
    ##### Outputs #####
    
    ## SPANS results ##
    
    #'@details SPANS plot
    output[[paste0(tab, "_spans_plot")]] <- renderPlotly({
      
      req(!is.null(SPANS_res) && !is.null(SPANS_res[[tab]]))
      
      p <- plot(SPANS_res[[tab]], interactive = T)
      p$x$source <- "SPANS"
      
      isolate(plot_table_current$table$PP__SPANS <- p)
      
      p
    }),
    
    #'@details Table of SPANS scores
    output[[paste0(tab, "_spans_table")]] <- renderDT({
      
      req(!is.null(SPANS_res))
      
      df <- SPANS_res[[tab]]
      
      df
      
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
    
    output[[paste0(tab, "_normalized_boxplots_pre_render")]] <- renderUI({
      if (isTruthy(input[[paste0(tab, "_normalized_boxplots_pre_load")]]) || 
          dim(omicsData$objPP$e_data)[1] < 20000) {
        withSpinner(plotlyOutput(paste0(tab, "_normalized_boxplots_pre")))
      } else {
        div(
          "This plot is large and may take a while to render.",
          actionButton(paste0(tab, "_normalized_boxplots_pre_load"), "Show plot")
        )
      }
      
    }),
    
    output[[paste0(tab, "_normalized_boxplots_pre")]] <- renderPlotly({ 
      
      req(!all(unlist(omicsData$objPP$e_data[-1]) %in% c(0, 1)))

      if(!(input[["normalized"]] == "Yes" ||  
            get_data_norm(isolate(omicsData$objPP)) == F)){
        return(isolate(plot_table_current$table$PP__normalization__pre))
      }
      
      if(all(unlist(omicsData$objPP$e_data[-1]) %in% c(0, 1))){
        df <- as.data.frame(table(melt(omicsData$objPP$e_data)[2:3]))
        p2 <- ggplot(data = df, aes(x = variable, fill = value, y = Freq)) + 
          geom_col() + theme_bw() + 
          labs(
            paste0("Un-Normalized: ", tab, " Data"),
            fill = "Conversion",
            y = "Number of biomolecules",
            x = "",
          ) + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
        
      } else if(inherits(omicsData$objPP, "seqData")){
        x <- omicsData$objPP
        p <- plot(x)
        
        yaxis <- switch(get_data_info(x)$data_scale_actual,
                        lcpm = "Log counts per million",
                        upper = "Upper-quantile transformed counts",
                        median = "Median counts")
        p <- p + labs(y = yaxis, 
                      title = paste0("Un-Normalized: ", tab, " Data"))
      
      } else if(!is.null(get_group_DF(omicsData$objPP))){
        
        p <- plot_noconv(as.slData(omicsData$objPP), 
                         color_by = "Group", order_by = "Group") + labs(
          title = paste0("Un-Normalized: ", tab, " Data")
        )
        
      } else {
        
        title_use <- ifelse(input[["normalized"]] == "Yes", 
                        paste0("Normalized: ", tab, " Data"),
                        paste0("Un-Normalized: ", tab, " Data"))
        
        p <- plot_noconv(as.slData(omicsData$objPP)) + labs(
          title = title_use
        )
      }
      
      isolate(plot_table_current$table$PP__normalization__pre <- p)
      
      p <- p %>% ggplotly()
      
      p
    }),
    
    # Post Norm
    output[[paste0(tab, "_normalized_boxplots_post_UI")]] <- renderUI({
      
      if (
        !is.null(omicsData$objNorm)
      ) {
        if (isTruthy(input[[paste0(tab, "_normalized_boxplots_post_load")]]) || 
            dim(omicsData$objPP$e_data)[1] < 20000) {
          return(plotlyOutput(paste0(tab, "_normalized_boxplots_post")))
        } else {
          div(
            "This plot is large and may take a while to render.",
            actionButton(paste0(tab, "_normalized_boxplots_post_load"), "Show plot")
          )
        }
        
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
      
      req((input[["normalized"]] == "Yes" ||  
            get_data_norm(isolate(omicsData$objPP)) == F ) &&
          !is.null(omicsData$objNorm),
          cancelOutput = T)

      if(!is.null(get_group_DF(omicsData$objNorm))){
        
        p <- plot_noconv(as.slData(omicsData$objNorm), 
                         color_by = "Group", order_by = "Group") + labs(
                           title = paste0("Normalized: ", tab, " Data")
                         )
        
      } else {
        p <- plot_noconv(as.slData(omicsData$objNorm)) + labs(
          title = paste0("Normalized: ", tab, " Data")
        )
      }
      
      isolate(plot_table_current$table$PP__normalization__post <- p)
      
      p <- p %>% ggplotly()
      
      p
    })
  )
  
  output[["complete_norm_UI"]] <- renderUI({
    
    req((!is.null(input[[paste0(tab, "_normalize_option")]]) &&
      input[[paste0(tab, "_normalize_option")]] == "No Normalization") || 
          (!is.null(omicsData$objNorm) &&
          pmartR:::get_data_norm(omicsData$objNorm)))
    
    actionButton("complete_norm", "Confirm Selections")
    
  })
  
  ###
  
  outputOptions(output, paste0(tab, "_normalize_sidepanel"), suspendWhenHidden = FALSE)
  outputOptions(output, paste0(tab, "_normalize_option_UI"), suspendWhenHidden = FALSE)
}
