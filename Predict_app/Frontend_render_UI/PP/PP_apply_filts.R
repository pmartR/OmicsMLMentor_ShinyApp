
## Needed for seq data transform post filters
edata_transform_seq <- function(OD, transformation){

  temp_data <- OD$e_data
  iCol <- which(names(OD$e_data) == get_edata_cname(OD))

  transform_data <- temp_data[, -iCol]
  if (transformation == "lcpm") {
    samp_sum <- apply(transform_data, 2, sum, na.rm = TRUE) +
      1
    div_sum <- sweep((transform_data + 0.5), 2, samp_sum,
                     `/`)
    temp_data[, -iCol] <- log2(div_sum * 10^6)
  } else if (transformation == "upper") {
    warning("Zeros will be regarded as NA for 'upper' transformation")
    transform_data[transform_data == 0] <- NA
    samp_upper <- apply(transform_data, 2, quantile,
                        na.rm = TRUE, probs = 0.75)
    g.q <- quantile(unlist(transform_data), probs = 0.75,
                    na.rm = TRUE)
    div_75 <- sweep(transform_data, 2, samp_upper, `/`)
    temp_data[, -iCol] <- div_75 * g.q
  } else if (transformation == "median") {
    warning("Zeros will be regarded as NA for 'median' transformation")
    transform_data[transform_data == 0] <- NA
    samp_med <- apply(transform_data, 2, median, na.rm = TRUE)
    div_med <- sweep(transform_data, 2, samp_med, `/`)
    g.q <- median(unlist(transform_data), na.rm = TRUE)
    temp_data[, -iCol] <- div_med * g.q
  }

  OD$e_data <- temp_data

  ## Plots get upset if not counts
  attr(OD, "data_info")$data_scale_actual <- transformation
  OD

}


## Needed for re-add of og molecules after imputation
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

observeEvent(input$apply_filters,{
  req(!is.null(input$apply_filters))
  if(input$apply_filters == "Yes"){
    showModal(
      modalDialog(
        title = "Alert!",
        "Any filters (molecule, cv, etc.) performed on the original dataset will also be performed on this new data. Any molecules used for predictions will remain regardless.",
        easyClose = TRUE,
        footer = modalButton("OK")
      )
    )
    #enable(id = "confirm_filters")
  }
})

observeEvent(input$confirm_filters,{
  req(omicsData$obj_scaled)
  req(input$apply_filters)

  # pipeline
  omics_processed <- omicsData$obj_scaled
  model <- omicsData$model$model
  norm_data <- omicsData$model$norm_omics
  pp_data <- omicsData$model$pp_omics

  tmp <- omics_processed

  if(input$apply_filters == "Yes"){


    # this will be used in all scenarios - will need to build this up
    # more with more filters in the future
    all_filter_functions <- c("custom_filter","molecule_filter","cv_filter","imputation_filter")
    names(all_filter_functions) <- c("customFilt","moleculeFilt","cvFilt","imputationFilt")


    # arguments needed for those functions
    all_filter_requirements <- list(
      "moleculeFilt" = list(
        "threshold" = NA,
        method = list("use_groups" = NA,"use_batch" = NA)
        ),
      "cvFilt" = list(
        "threshold" = NA,
        method = list("use_groups" = NA)
        ),
      "imputationFilt" = list(
        "thresholds" = list("keep" = NA,
                            "impute" = NA,
                            "convert" = NA,
                            "remove" = NA))
      )

    # get the different filters used in omics object from original model
    filter_info <- pmartR::get_filters(norm_data)
    filter_types <- unlist(sapply(filter_info,function(x) x['type']))
    filter_types <- filter_types[filter_types != "customFilt"]

    # subset to ones present in this specific analysis
    all_filter_requirements_specific <- all_filter_requirements[
      which(names(all_filter_requirements) %in% filter_types)]

    # determine what molecules will stay regardless, because they are
    # found in the final model
    # emeta of new dataset is emeta of pp omics dataset

    model_names_og <- attributes(omicsData$model$model)$feature_info$names_orig

    if("pepData" %in% class(tmp)){
      og_edata_cname = pmartR::get_emeta_cname(tmp)
      # peptide cname for new data
      new_edata_cname = pmartR::get_edata_cname(tmp)

      ## Important for removal, but with 0-1 norms I wouldn't expect any
      og_proteins = as.character(model_names_og)
      pep_cname = pmartR::get_edata_cname(omicsData$model$norm_omics)

      ## No peptides are converted or removed till after protein rollup, so these should essentially all be kept
      og_molecules = tmp$e_meta[[get_edata_cname(tmp)]][tmp$e_meta[[get_emeta_cname(tmp)]] %in% og_proteins]
    } else {
      og_edata_cname = pmartR::get_edata_cname(tmp)
      new_edata_cname = pmartR::get_edata_cname(tmp)
      og_molecules = as.character(model_names_og)
    }

    ### Filters ###

    ### Need to add seqData filters support ###

    # molecule filter (if applicable)
    if(("moleculeFilt" %in% names(all_filter_requirements_specific))){

      og_filter_id <- which(unlist(sapply(filter_info,function(x) x['type'])) == "moleculeFilt")

      use_groups <- any(map_lgl(og_filter_id, function(x)
        attr(norm_data,"filters")[[x]]$method$use_groups))
      threshold <- max(map_int(og_filter_id, function(x)
        attr(norm_data,"filters")[[x]]$threshold))
      use_batch <- any(map_lgl(og_filter_id, function(x)
        attr(norm_data,"filters")[[x]]$method$use_batch))

      # add in attributes
      all_filter_requirements_specific_id <- which(names(all_filter_requirements_specific) == "moleculeFilt")
      all_filter_requirements_specific[[all_filter_requirements_specific_id]]$threshold = threshold
      all_filter_requirements_specific[[all_filter_requirements_specific_id]]$method$use_groups = use_groups
      all_filter_requirements_specific[[all_filter_requirements_specific_id]]$method$use_batch = use_batch

      # Create filter object
      filtObj = pmartR::molecule_filter(
        tmp,
        ifelse(is.null(use_groups), FALSE, use_groups),
        ifelse(is.null(use_batch), FALSE, use_batch)
      )

      # Find molecules that would be filtered that are not in the model molecules
      filtObj_custom <- filtObj %>% data.frame() %>%
        dplyr::filter(!(!!as.symbol(new_edata_cname) %in% og_molecules)) %>%
        dplyr::filter(Num_Observations < threshold)

      # Use custom filter to drop those molecules
      if(nrow(filtObj_custom) > 0){
        molfilt_customFilt <- custom_filter(
          tmp, e_data_remove = as.character(filtObj_custom[[new_edata_cname]])
          )

        tmp <- pmartR::applyFilt(molfilt_customFilt, tmp)
      }
    }

    # cv filter
    if(("cvFilt" %in% names(all_filter_requirements_specific))){

      og_filter_id = which(unlist(sapply(filter_info,function(x) x['type'])) == "cvFilt")

      # add in attributes
      all_filter_requirements_specific_id <- which(names(all_filter_requirements_specific) == "cvFilt")

      threshold <- attr(norm_data,"filters")[[og_filter_id]]$threshold
      use_groups <- attr(norm_data,"filters")[[og_filter_id]]$method$use_groups

      all_filter_requirements_specific[[all_filter_requirements_specific_id]]$threshold <- threshold
      all_filter_requirements_specific[[all_filter_requirements_specific_id]]$method$use_groups <- use_groups

      # Create filter object
      filtObj = pmartR::cv_filter(omics_processed, use_groups)

      # Find molecules that would be filtered that are not in the model molecules
      filtObj_custom <- filtObj %>% data.frame() %>%
        dplyr::filter(!(!!as.symbol(new_edata_cname) %in% og_molecules)) %>%
        dplyr::filter(CV < threshold)

      # Use custom filter to drop those molecules
      if(nrow(filtObj_custom) > 0){

        cv_customFilt <- custom_filter(omics_processed,
                                       e_data_remove = as.character(filtObj_custom[[new_edata_cname]]))

        tmp <- pmartR::applyFilt(cv_customFilt, tmp)
      }
    }

    # need to filter out molecules that are never identified (which should not affect the process at all)
    molfilt_zero <- pmartR::molecule_filter(tmp)
    tmp <- pmartR::applyFilt(molfilt_zero, tmp, min_num = 1)
    omics_processed_sl <- as.slData(tmp)

    # separate step for imputation
    if(("imputationFilt" %in% names(all_filter_requirements_specific))){

      og_filter_id = which(unlist(sapply(filter_info,function(x) x['type'])) == "imputationFilt")
      thresholds <- attr(norm_data,"filters")[[og_filter_id]]$threshold

      imputed_dat <- imputation_function(omics_processed_sl, thresholds = thresholds)

      ## Where og molecules would be removed, restore them

      missing_mols <- !(og_molecules %in% imputed_dat$e_data[[new_edata_cname]])

      if(any(missing_mols)){

        missing_omics <- applyFilt(
          custom_filter(tmp, e_data_keep = og_molecules[missing_mols]), tmp
          )

        ## Missing molecules may just not be present
        if(nrow(missing_omics$e_data) > 0){
          tmp <- combine_omicsdata(missing_omics, imputed_dat)
        }

      } else {
        tmp <- imputed_dat
      }

      attr(tmp, "filters") <- c(attr(tmp, "filters"), list(list(type = "imputationFilt",
                                                                thresholds = thresholds)))

      class(tmp) <- class(omics_processed)
    }

  }

  if(inherits(norm_data, "seqData")){

    transformation <- attr(pp_data, "data_info")$data_scale_actual
    tmp <- edata_transform_seq(tmp, transformation)

  }

  # convert to normalization immediately
  if((attributes(omicsData$obj)$data_info$norm_info$is_normalized == FALSE)|
     (!is.null(attributes(omicsData$obj)$data_info$norm_info$norm_fn) &&
      attributes(omicsData$obj)$data_info$norm_info$norm_fn != norm_method)){
    omicsData$obj_norm <- pmartR::normalize_zero_one_scaling(tmp)
  } else {
    omicsData$obj_norm <- tmp
  }

  table_table_current$table$PP__normalization <-  omicsData$obj_norm$e_data

  # convert to sl object
  omics_sl <- as.slData(omicsData$obj_norm)
  omicsData$obj_sl <- omics_sl


  # rollup
  # if we have pepdata in normalized object but prodata in pp omics object
  # this means that we have undergone protein rollup
  # so we need to rollup as well

  if("proData" %in% class(pp_data) & "pepData" %in% class(norm_data)){


    rollup_method <- attr(pp_data,"pro_quant_info")$method
    omicsData$obj_sl_rollup <- slopeR::protein_rollup(omics_sl, method = rollup_method)

    if(input$apply_filters == "Yes" && "imputationFilt" %in% names(all_filter_requirements_specific)){

      og_filter_id = which(unlist(sapply(filter_info,function(x) x['type'])) == "imputationFilt")
      thresholds <- attr(norm_data,"filters")[[og_filter_id]]$threshold

      imputed_dat <- imputation_function(omicsData$obj_sl_rollup, thresholds = thresholds)


      ## Where og molecules would be removed, restore them
      missing_mols <- !(og_proteins %in% imputed_dat$e_data[[get_edata_cname(imputed_dat)]])

      if(any(missing_mols)){

        missing_omics <- applyFilt(
          custom_filter(omicsData$obj_sl_rollup, e_data_keep = og_proteins[missing_mols]),
          omicsData$obj_sl_rollup
        )

        ## Missing molecules may just not be present
        if(nrow(missing_omics$e_data) > 0){
          omicsData$obj_sl_rollup <- combine_omicsdata(missing_omics, imputed_dat)
        }

      }
    }

    table_table_current$table$PP__rollup <-   omicsData$obj_sl_rollup$e_data
  }

  if(!is.null(omicsData$obj_sl_rollup)){
    omicsData$obj_pp <- omicsData$obj_sl_rollup

    updateTabsetPanel(session, "plots_preprocess", selected = "Rollup")

  } else {
    omicsData$obj_pp <- omicsData$obj_sl

    updateTabsetPanel(session, "plots_preprocess", selected = "Normalization")
  }

})

