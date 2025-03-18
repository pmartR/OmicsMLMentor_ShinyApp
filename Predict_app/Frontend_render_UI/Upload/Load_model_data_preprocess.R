

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
  
  
  ### Figure out how this differs from the calls to all_filter_requirements_specific in PP_apply_filts
  if(length(all_filter_requirements_specific) > 0){
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
     input$use_fdata == "Yes"
     ){
    
    if(!attr(get_group_DF(norm_data), "main_effects") %in% colnames(omics_processed$f_data)){
      omics_processed$f_data[[attr(get_group_DF(norm_data), "main_effects")]] <- "Unknown"
    }
    
    # need check that main effect column is in both original and new dataset
    omics_processed <- pmartR::group_designation(
      omics_processed,
      main_effects = attr(get_group_DF(norm_data), "main_effects"))
  } else if (supervised()){
    omics_processed$f_data <- data.frame(
      SampleID = colnames(omics_processed$e_data)[
        colnames(omics_processed$e_data) != get_edata_cname(omics_processed)],
      Group = "Unknown", check.names = F)
    
    omics_processed <- pmartR::group_designation(
      omics_processed,
      main_effects = "Group")
  } else if (is.null(omics_processed$f_data)){
    
    omics_processed$f_data <- data.frame(
      SampleID = colnames(omics_processed$e_data)[
        colnames(omics_processed$e_data) != get_edata_cname(omics_processed)],
      Group = "Unknown", check.names = F)
    
  }
  
  # convert to log2 scale immediately
  if(!inherits(omics_processed, "seqData") && data_scale_info != get_data_scale(omics_processed)){
    omicsData$obj_scaled = pmartR::edata_transform(omics_processed, data_scale_info)
    table_table_current$table$PP__transform <- omicsData$obj_scaled$e_data
    
  } else {
    omicsData$obj_scaled = omics_processed
  }
  
  
})