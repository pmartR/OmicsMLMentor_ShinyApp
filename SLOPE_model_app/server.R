options(shiny.maxRequestSize = 250 * 1024^2, 
        ch.dir = TRUE, 
        expressions = 5e5,
        DT.TOJSON_ARGS = list(na = "string"),
        shiny.fullstacktrace=TRUE)

shinyServer(function(session,input,output){
  
  source("RV_all.R")
  
  file_loads <- c(
    list.files("./Frontend_render_UI", recursive = T, full.names = T)
  )
  
  for (f in grep(".R$", file_loads, value = T)) source(f, local = TRUE)
  
  output$transformation_backfill_text <- renderText({
    
    req(!is.null(omicsData$model) && !is.null(omicsData$obj))
    
    # determine the number of 
    num_in_og <- length(attributes(omicsData$model$model)$feature_info$names_orig)
    
    if("pepData" %in% class(omicsData$obj)){
      # use emeta information
      emeta_cname = pmartR::get_emeta_cname(omicsData$obj)
      num_in_both <- sum(unique(omicsData$obj$e_meta[[emeta_cname]]) %in% attributes(omicsData$model$model)$feature_info$names_orig)
      num_in_just_new <- sum(!unique(omicsData$obj$e_meta[[emeta_cname]]) %in% attributes(omicsData$model$model)$feature_info$names_orig)
    } else {
      edata_cname = pmartR::get_edata_cname(omicsData$obj)
      num_in_both <- sum(unique(omicsData$obj$e_data[[edata_cname]]) %in% attributes(omicsData$model$model)$feature_info$names_orig)
      num_in_just_new <- sum(!unique(omicsData$obj$e_data[[edata_cname]]) %in% attributes(omicsData$model$model)$feature_info$names_orig)
    }
    num_in_og_not_new <- num_in_og - num_in_both
    
    paste0("The original model had ", num_in_og, " distinct molecules. Of those molecules, ", num_in_og_not_new,
           " were not identified in the new dataset and will need to be backfilled as 0s for the model to run.", 
           " Additionally, ", num_in_just_new, " molecules were identified in only the new dataset and therefore ",
           "will be removed prior to running the model.")
    
  })
  
  output$transformation_backfill_plot <- renderPlotly({
    
    req(!is.null(omicsData$model) && !is.null(omicsData$obj))
    # determine the number of 
    num_in_og <- length(attributes(omicsData$model$model)$feature_info$names_orig)
    
    if("pepData" %in% class(omicsData$obj)){
      # use emeta information
      emeta_cname = pmartR::get_emeta_cname(omicsData$obj)
      num_in_both <- sum(unique(omicsData$obj$e_meta[[emeta_cname]]) %in% attributes(omicsData$model$model)$feature_info$names_orig)
      num_in_just_new <- sum(!unique(omicsData$obj$e_meta[[emeta_cname]]) %in% attributes(omicsData$model$model)$feature_info$names_orig)
    } else {
      edata_cname = pmartR::get_edata_cname(omicsData$obj)
      num_in_both <- sum(unique(omicsData$obj$e_data[[edata_cname]]) %in% attributes(omicsData$model$model)$feature_info$names_orig)
      num_in_just_new <- sum(!unique(omicsData$obj$e_data[[edata_cname]]) %in% attributes(omicsData$model$model)$feature_info$names_orig)
    }
    num_in_og_not_new <- num_in_og - num_in_both
    backfill_plot <- data.frame(Dataset = factor(c("Only Original","Both", "Only New"),levels = c("Only Original","Both", "Only New")),
                                Value = c(num_in_og_not_new,num_in_both,num_in_just_new)) %>%
      ggplot(aes(x = Dataset, y = Value,fill = Dataset)) + 
      geom_col() +
      geom_text(aes(label = paste0("n = ",Value),y = Value + 200)) +
      theme_bw() + 
      labs(y = "Number of Distinct Proteins Identified",x = "") + 
      guides(fill = "none")
    ggplotly(backfill_plot)
    
  })
  
  observeEvent(omicsData$obj,{
    req(!is.null(omicsData$obj))
    
    # pipeline
    omics_processed <- omicsData$obj
    model <- omicsData$model$model
    norm_data <- omicsData$model$norm_omics
    pp_data <- omicsData$model$pp_omics
    
    # log2 or otherwise
    data_scale_info <- get_data_scale(norm_data)
    
    # this will be used in all scenarios - will need to build this up
    # more with more filters in the future
    all_filter_functions <- c("custom_filter","molecule_filter","cv_filter")
    names(all_filter_functions) <- c("customFilt","moleculeFilt","cvFilt")
    # imdanova filter()
    # rmd_filter()
    
    # arguments needed for those functions
    all_filter_requirements <- list(
      "moleculeFilt" = list("threshold" = NA,method = list("use_groups" = NA,"use_batch" = NA)),
      "cvFilt" = list("threshold" = NA,method = list("use_groups" = NA)))
    
    # get the different filters used in omics object from original model
    filter_info <- pmartR::get_filters(norm_data)
    filter_types <- unlist(sapply(filter_info,function(x) x['type']))
    filter_types <- filter_types[filter_types != "customFilt"]
    
    # subset to ones present in this specific analysis
    all_filter_requirements_specific <- all_filter_requirements[
      which(names(all_filter_requirements) %in% filter_types)]
    
    # iterate through the specific filters for this dataset
    for(i in 1:length(all_filter_requirements_specific)){
      
      # molecule filter
      if(names(all_filter_requirements_specific)[i] == "moleculeFilt"){
        # which filter in original matches this ?
        og_filter_id = which(unlist(sapply(filter_info,function(x) x['type'])) == "moleculeFilt")
        # add in attributes
        all_filter_requirements_specific[[i]]$threshold = attr(norm_data,"filters")[[og_filter_id]]$threshold
        all_filter_requirements_specific[[i]]$method$use_groups = attr(norm_data,"filters")[[og_filter_id]]$method$use_groups
        all_filter_requirements_specific[[i]]$method$use_batch = attr(norm_data,"filters")[[og_filter_id]]$method$use_batch
      }
      # cv filter
      if(names(all_filter_requirements_specific)[i] == "cvFilt"){
        # which filter in original matches this ?
        og_filter_id = which(unlist(sapply(filter_info,function(x) x['type'])) == "cvFilt")
        # add in attributes
        all_filter_requirements_specific[[i]]$threshold = attr(norm_data,"filters")[[og_filter_id]]$threshold
        all_filter_requirements_specific[[i]]$method$use_groups = attr(norm_data,"filters")[[og_filter_id]]$method$use_groups
      }
    }
    
    # normalization
    norm_method <- attr(norm_data,"data_info")$norm_info$norm_fn
    
    # rollup
    # if we have pepdata in normalized object but prodata in pp omics object
    # this means that we have undergone protein rollup
    # so we need to rollup as well
    rollup_method = "Not Applicable"
    if("proData" %in% class(pp_data) & "pepData" %in% class(norm_data)){
      rollup_method = attr(pp_data,"pro_quant_info")$method
    }
    
    omicsData$pp_info <- list("scale" = data_scale_info,
                              "normalization" = norm_method,
                              "filters" = all_filter_requirements_specific,
                              "rollup" = rollup_method)
    
    # add in group designation if needed for downstream filters
    if(!is.null(attr(norm_data, "group_DF")) && 
       !is.null(omics_processed$f_data) &&
       input$use_fdata == "Yes"){
      # need check that main effect column is in both original and new dataset
      omics_processed <- pmartR::group_designation(
        omics_processed,
        main_effects = attr(get_group_DF(norm_data), "main_effects"))
    } else {
      omics_processed$f_data <- data.frame(
        SampleID = colnames(omics_processed$e_data)[
          colnames(omics_processed$e_data) != get_edata_cname(omics_processed)],
        ` ` = "Unknown", check.names = F)
      
      omics_processed <- pmartR::group_designation(
        omics_processed,
        main_effects = " ")
    }
    
    # convert to log2 scale immediately
    if(data_scale_info != get_data_scale(omics_processed)){
      omicsData$obj_scaled = pmartR::edata_transform(omics_processed, data_scale_info)
    } else {
      omicsData$obj_scaled = omics_processed
    }
  })
  
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
    
    if(input$apply_filters == "Yes"){
      # this will be used in all scenarios - will need to build this up
      # more with more filters in the future
      all_filter_functions <- c("custom_filter","molecule_filter","cv_filter","imputation_filter")
      names(all_filter_functions) <- c("customFilt","moleculeFilt","cvFilt","imputationFilt")
      # imdanova filter()
      # rmd_filter()
      
      # arguments needed for those functions
      all_filter_requirements <- list(
        "moleculeFilt" = list("threshold" = NA,method = list("use_groups" = NA,"use_batch" = NA)),
        "cvFilt" = list("threshold" = NA,method = list("use_groups" = NA)),
        "imputationFilt" = list("thresholds" = list("keep" = NA,"impute" = NA,"convert" = NA, "remove" = NA)))
      
      # get the different filters used in omics object from original model
      filter_info <- pmartR::get_filters(norm_data)
      filter_types <- unlist(sapply(filter_info,function(x) x['type']))
      filter_types <- filter_types[filter_types != "customFilt"]
      
      # subset to ones present in this specific analysis
      all_filter_requirements_specific <- all_filter_requirements[
        which(names(all_filter_requirements) %in% filter_types)]
      
      # determine what molecules will stay regardless, because they are
      # found in the predictions dataset
      # edata of new dataset is emeta of pp omics dataset
      
      if("pepData" %in% class(omicsData$obj_scaled)){
        og_edata_cname = pmartR::get_emeta_cname(omicsData$obj_scaled)
        # peptide cname for new data
        new_edata_cname = pmartR::get_edata_cname(omicsData$obj_scaled)
        og_proteins = as.character(omicsData$model$pp_omics$e_data[[og_edata_cname]])
        pep_cname = pmartR::get_edata_cname(omicsData$model$norm_omics)
        og_molecules = as.character(omicsData$model$norm_omics$e_data[[pep_cname]])
      } else {
        og_edata_cname = pmartR::get_edata_cname(omicsData$obj_scaled)
        new_edata_cname = pmartR::get_edata_cname(omicsData$obj_scaled)
        og_molecules = as.character(omicsData$model$norm_omics$e_data[[og_edata_cname]])
      }
      
      
      # molecule filter (if applicable)
      if(("moleculeFilt" %in% names(all_filter_requirements_specific))){
        og_filter_id = which(unlist(sapply(filter_info,function(x) x['type'])) == "moleculeFilt")
        thresholds <- attr(norm_data,"filters")[[og_filter_id]]$threshold
        
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
        
        filtObj = pmartR::molecule_filter(
          omics_processed,
          ifelse(is.null(use_groups), FALSE, use_groups),
          ifelse(is.null(use_batch), FALSE, use_batch)
        )
        
        filtObj_custom <- filtObj %>% data.frame() %>%
          dplyr::filter(!(!!as.symbol(new_edata_cname) %in% og_molecules)) %>%
          dplyr::filter(Num_Observations < threshold)
        
        if(nrow(filtObj_custom) > 0){
          molfilt_customFilt <- custom_filter(omics_processed,e_data_remove = as.character(filtObj_custom[[new_edata_cname]]))
          omics_processed = pmartR::applyFilt(
            molfilt_customFilt,
            omics_processed)
        }
      }
      
      # cv filter
      if(("cvFilt" %in% names(all_filter_requirements_specific))){
        og_filter_id = which(unlist(sapply(filter_info,function(x) x['type'])) == "cvFilt")
        
        # add in attributes
        all_filter_requirements_specific_id <- which(names(all_filter_requirements_specific) == "cvFilt")
        all_filter_requirements_specific[[all_filter_requirements_specific_id]]$threshold = attr(norm_data,"filters")[[og_filter_id]]$threshold
        all_filter_requirements_specific[[all_filter_requirements_specific_id]]$method$use_groups = attr(norm_data,"filters")[[og_filter_id]]$method$use_groups
        
        threshold = all_filter_requirements_specific[[all_filter_requirements_specific_id]]$threshold
        use_groups = all_filter_requirements_specific[[all_filter_requirements_specific_id]]$method$use_groups
        
        filtObj = pmartR::cv_filter(omics_processed,
                                    use_groups)
        
        filtObj_custom <- filtObj %>% data.frame() %>%
          dplyr::filter(!(!!as.symbol(new_edata_cname) %in% og_molecules)) %>%
          dplyr::filter(CV < threshold)
        
        if(nrow(filtObj_custom) > 0){
          cv_customFilt <- custom_filter(omics_processed,e_data_remove = as.character(filtObj_custom[[new_edata_cname]]))
          omics_processed = pmartR::applyFilt(
            cv_customFilt,
            omics_processed)
        }
      }
      
      # need to filter out molecules that are never identified (which should not affect the process at all)
      molfilt_zero <- pmartR::molecule_filter(omics_processed)
      omics_processed <- pmartR::applyFilt(molfilt_zero,omics_processed, min_num = 1)
      omics_processed_sl <- as.slData(omics_processed)
      
      # separate step for imputation
      if(("imputationFilt" %in% names(all_filter_requirements_specific))){
        og_filter_id = which(unlist(sapply(filter_info,function(x) x['type'])) == "imputationFilt")
        thresholds <- attr(norm_data,"filters")[[og_filter_id]]$threshold
        if("pepData" %in% class(omicsData$model$norm_omics)){
          
          # Impute all of e_data
          imputed_data <- slopeR::imputation(omics_processed_sl)
          imputed_data_pmart <- slopeR::apply_imputation(imputed_data,omics_processed_sl)
          imputed_data <- cbind(E_DATA_CNAME = omics_processed_sl$e_data[[get_edata_cname(omics_processed_sl)]], imputed_data)
          names(imputed_data)[1] <- get_edata_cname(omics_processed_sl)
          
          # rollup for the intent of determining which get rolled up needing to be imputed
          rollup_method = attr(pp_data,"pro_quant_info")$method
          objQCPro <- slopeR::protein_rollup(imputed_data_pmart,method = rollup_method)
          # objQCPro <- protein_quant(edata_transform(omicsData$objQC, "log2"),
          #                           method = input$qc_which_rollup,
          #                           qrollup_thresh = input$qc_qrollup_thresh / 100,
          #                           single_pep = single_pep,
          #                           single_observation = single_observation,
          #                           combine_fn = input$qc_which_combine_fn,
          #                           parallel = TRUE)
          
          # get transformation data.frame
          transforms_df <- slopeR::get_transform_df(objQCPro,thresholds)
          
          # Get the proteins whose peptides will be imputed
          impute_proteins <- transforms_df[which(transforms_df$Handling == "Estimate"),][[pmartR::get_edata_cname(objQCPro)]]
          
          # Replace just those peptides with their imputed versions
          impute_pro_idx <- which(omics_processed_sl$e_meta[[get_emeta_cname(omics_processed_sl)]] %in% impute_proteins)
          impute_peps <- omics_processed_sl$e_meta[[get_edata_cname(omics_processed_sl)]][impute_pro_idx]
          impute_pep_idx <- which(omics_processed_sl$e_data[[get_edata_cname(omics_processed_sl)]] %in% impute_peps)
          omics_processed_sl$e_data[impute_pep_idx,] <- imputed_data[impute_pep_idx,]
          tmp <- omics_processed_sl
        } else {
          # all other data types
          tmp <- slopeR::edata_nathresh_transform(omics_processed_sl, thresholds)
        }
        attr(tmp, "filters") <- c(attr(tmp, "filters"), list(list(type = "imputationFilt")))
        omics_processed_sl <- tmp
      }
      # update for the other ones (0-1), and other
    }
    
    # for normalization have to convert back to not sl object
    class(tmp) <- class(omics_processed)
    
    # convert to normalization immediately
    if((attributes(omicsData$obj)$data_info$norm_info$is_normalized == FALSE)|
       (!is.null(attributes(omicsData$obj)$data_info$norm_info$norm_fn) && 
        attributes(omicsData$obj)$data_info$norm_info$norm_fn != norm_method)){
      omicsData$obj_norm <- pmartR::normalize_zero_one_scaling(tmp)
    } else {
      omicsData$obj_norm <- tmp
    }
    
    # convert to sl object
    omics_sl <- as.slData(omicsData$obj_norm)
    omicsData$obj_sl <- omics_sl

    
    # rollup
    # if we have pepdata in normalized object but prodata in pp omics object
    # this means that we have undergone protein rollup
    # so we need to rollup as well
    if("proData" %in% class(pp_data) & "pepData" %in% class(norm_data)){
      rollup_method = attr(pp_data,"pro_quant_info")$method
      omicsData$obj_sl_rollup <- slopeR::protein_rollup(omics_sl,method = rollup_method)
    }
    
    if(!is.null(omicsData$obj_sl_rollup)){
      omicsData$obj_pp <- omicsData$obj_sl_rollup
    } else {
      omicsData$obj_pp <- omicsData$obj_sl
    }
    
  })
  
  output$protein_rollup_pp_UI <- renderUI({
    req(omicsData$obj_sl)
    
    if(!is.null(omicsData$obj_sl_rollup)){
      collapseBox(
        div(
          "Rollup",
        ),
        value = "rollup",
        collapsed = F,
        plotlyOutput("transformation_rollup_plot")
      )
    }
  })

  
  
  observeEvent(input$done_molecule_filter,{
    req(!is.null(omicsData$obj))
    
    molfilt_1 <- molecule_filter(omicsData$obj)
    omicsData$obj_mol <- applyFilt(molfilt_1, omicsData = omicsData$obj,min_num = 2)
  })
  
  # run model
  observeEvent(input$run_model,{
    req(!is.null(omicsData$obj_scaled))
    req(!is.null(omicsData$obj_pp))
    req(!is.null(omicsData$model$model))
    
    # if the data is rolled up use that version, otherwise use OG slope version
    omicsNewdata = omicsData$obj_pp
    
    orig_names <- attr(omicsData$model$model,"feature_info")
    
    if("pepData" %in% class(omicsData$obj_scaled)){
      # remove columns not found in training data
      pro_edata <- omicsNewdata$e_data
      pro_edata <- pro_edata[unlist(pro_edata[pmartR::get_emeta_cname(omicsNewdata)]) %in% orig_names$names_orig,]
      # find which are found in the OG version but not in the new data
      # we will want
      in_og_not_new <- orig_names$names_orig[!orig_names$names_orig %in% unlist(pro_edata[pmartR::get_emeta_cname(omicsNewdata)])]
      
      in_og_not_new_df <- data.frame(emetname = in_og_not_new,
                                     matrix(0,ncol = nrow(omicsNewdata$f_data),nrow = length(in_og_not_new)))
      emetCol = which(colnames(in_og_not_new_df) == "emetname")
      colnames(in_og_not_new_df)[emetCol] <- pmartR::get_emeta_cname(omicsNewdata)
      colnames(in_og_not_new_df)[-emetCol] <- colnames(pro_edata)[-emetCol]
      # add into old dataset
      pro_edata <- dplyr::bind_rows(pro_edata,in_og_not_new_df)
      # arrange them to match the original features
      pro_edata <- pro_edata[match(orig_names$names_orig,unlist(pro_edata[pmartR::get_emeta_cname(omicsNewdata)])),]
    } else {
      pro_edata <- omicsNewdata$e_data
      pro_edata <- pro_edata[unlist(pro_edata[pmartR::get_edata_cname(omicsNewdata)]) %in% orig_names$names_orig,]
      # find which are found in the OG version but not in the new data
      # we will want
      in_og_not_new <- orig_names$names_orig[!orig_names$names_orig %in% unlist(pro_edata[pmartR::get_edata_cname(omicsNewdata)])]
      
      in_og_not_new_df <- data.frame(edatname = in_og_not_new,
                                     matrix(0,ncol = nrow(omicsNewdata$f_data),nrow = length(in_og_not_new)))
      edatCol = which(colnames(in_og_not_new_df) == "edatname")
      colnames(in_og_not_new_df)[edatCol] <- pmartR::get_edata_cname(omicsNewdata)
      colnames(in_og_not_new_df)[-edatCol] <- colnames(pro_edata)[-edatCol]
      # add into old dataset
      pro_edata <- dplyr::bind_rows(pro_edata,in_og_not_new_df)
      # arrange them to match the original features
      pro_edata <- pro_edata[match(orig_names$names_orig,unlist(pro_edata[pmartR::get_edata_cname(omicsNewdata)])),]
    }

    # now set up new data to be ran with predictions
    new_info <- pro_edata[,-which(colnames(pro_edata) == pmartR::get_edata_cname(omicsNewdata))] %>%
      t() %>% data.frame()
    names(new_info) <- paste0("feature_",seq(from = 1, to = ncol(new_info)))
    
    # unique levels in the model
    unique_outcomes <- unlist(unique(omicsData$model$model$pre$mold$outcomes))
    
    contains_exact_values <- function(column, values) {
      all(sort(column[!is.na(column)]) %in% (values))
    }
    
    # Apply the function to each column and find the matching column name
    matching_column <- colnames(omicsNewdata$f_data)[
      apply(omicsNewdata$f_data, 2, 
            contains_exact_values, values = unique_outcomes)][1]
    
    ### Note: this ought to be generalized for if fdata isn't uploaded
    ### RR put in a temp f_data, but it won't ever line up with matching_column so ya
    
    fdata_sub <- omicsNewdata$f_data %>%
      dplyr::select(!!as.symbol(pmartR::get_fdata_cname(omicsNewdata)),
                    !!as.symbol(matching_column))
    
    new_info <- new_info %>%
      tibble::rownames_to_column(var = pmartR::get_fdata_cname(omicsNewdata)) %>%
      dplyr::left_join(fdata_sub,by = pmartR::get_fdata_cname(omicsNewdata))
    
    new_info <- new_info %>%
      dplyr::rename(response = !!as.symbol(matching_column)) %>%
      tibble::column_to_rownames(var = pmartR::get_fdata_cname(omicsNewdata))
    new_info[is.na(new_info)] <- 0
    
    
    # run predictions
    prediction_test_df <- tibble(response = factor(unlist(omicsNewdata$f_data[matching_column]),levels = unique_outcomes),
                                 .pred_class = unlist(predict(omicsData$model$model$fit$fit,new_data = new_info))) %>%
      dplyr::bind_cols(data.frame(predict(omicsData$model$model$fit$fit,new_data = new_info,type = "prob"))) %>%
      dplyr::mutate(`__SAMPNAMES__` = unlist(omicsNewdata$f_data[pmartR::get_fdata_cname(omicsNewdata)]))
    # full model already an slRes object
    full_model_test <- omicsData$model$model
    full_model_train <- omicsData$model$model
    
    attributes(full_model_train)$prediction_test <- attributes(full_model_train)$prediction_test
    attributes(full_model_train)$prediction_test <- attributes(full_model_train)$prediction_train
    # update it to match OOB estimates
    #attributes(full_model_train)$prediction_test$.pred_Mock <- full_model_train$fit$fit$fit$votes[,1]
    #attributes(full_model_train)$prediction_test$.pred_RhCMV <- full_model_train$fit$fit$fit$votes[,2]
    #attributes(full_model_train)$prediction_test$.pred_class <- ifelse(attributes(full_model_train)$prediction_test$.pred_Mock > 0.5,"Mock","RhCMV")
    
    # update test dataset too
    attributes(full_model_test)$prediction_test <- prediction_test_df
  
    omicsData$obj_predictions <- full_model_test
  })
  
  # log 2 plot
  output$transformation_scaling_plot <- renderPlotly({
    req(!is.null(omicsData$obj_scaled))
    if(!is.null(attributes(omicsData$obj_scaled)$group_DF)){
      plot(omicsData$obj_scaled, use_VizSampNames = T,
           color_by = "Group", order_by = "Group",interactive = TRUE)
    } else {
      plot(omicsData$obj_scaled, use_VizSampNames = T, 
           color_by = "Group", order_by = "Group",interactive = TRUE)
    }
  })
  
  # norm plot
  output$transformation_norm_plot <- renderPlotly({
    req(!is.null(omicsData$obj_norm))
    if(!is.null(attributes(omicsData$obj_norm)$group_DF)){
      plot(omicsData$obj_norm, use_VizSampNames = T,color_by = "Group", order_by = "Group",interactive = TRUE)
    } else {
      plot(omicsData$obj_norm, use_VizSampNames = T,interactive = TRUE)
    }
  })
  
  # rollup plot
  output$transformation_rollup_plot <- renderPlotly({
    req(!is.null(omicsData$obj_sl_rollup))
    if(!is.null(attributes(omicsData$obj_sl_rollup)$group_DF)){
      plot(omicsData$obj_sl_rollup, use_VizSampNames = T,
           color_by = "Group", order_by = "Group",interactive = TRUE)
    } else {
      plot(omicsData$obj_sl_rollup, use_VizSampNames = T,interactive = TRUE)
    }
  })
  
  output$predict_plot_ROC <- renderPlotly({
    req(!is.null(omicsData$obj_predictions))
    ggplotly(plot(omicsData$obj_predictions, plotType = "roc_curve"))
  })
  output$predict_plot_predictBar <- renderPlotly({
    req(!is.null(omicsData$obj_predictions))
    ggplotly(plot(omicsData$obj_predictions, plotType = "prediction_bar"))
  })
  output$predict_plot_confusionHeatmap <- renderPlotly({
    req(!is.null(omicsData$obj_predictions))
    ggplotly(plot(omicsData$obj_predictions, plotType = "confusion_heatmap"))
  })
  # 
  # true_pos_predict_plot_confidenceScatter_UI <- renderUI({
  #   #req(omicsData$obj_predictions)
  #   # pickerInput("true_pos_predict_plot_confidenceScatter",
  #   #             "Designate true positive event",
  #   #             choices = c("dog","cat"))
  #   actionButton("dog","Dog")
  # })
  # 
  
  output$true_pos_picker_UI <- renderUI({
    req(omicsData$obj_predictions)
    df <- unique(omicsData$obj_predictions$pre$mold$outcomes$response)
    pickerInput(inputId = "true_pos_select",
                label = "Designate true positive event",
                choices = df,
                selected = df[1])
  })
  
  output$predict_plot_confidenceScatter <- renderPlotly({
    req(!is.null(omicsData$obj_predictions))
    ggplotly(plot(omicsData$obj_predictions, plotType = "confidence_scatter",pos_class = input$true_pos_select))
  })
  
  output$download_processed_data <- downloadHandler(
    filename = function() {
      paste0("SLOPE_output_", gsub("( |:|-)", "_", Sys.time()), ".zip")
    },
    content = function(fname) {
      file.copy(zipped_file$fs, fname)
    },
    contentType = "application/zip"
  )
  
  # download tab
  output$Download_button <- renderUI({
    button <- downloadButton(
      "download_processed_data",
      tags$b("Download Bundle")
    )
    
    div(
      id = "js_downloadbutton",
      style = "margin-left:4px;float:left",
      class = "grey_button",
      button
    )
  })
  # BROWSER
  observeEvent(input$Browser,{
    browser()
  })
})
