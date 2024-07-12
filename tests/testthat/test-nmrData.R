library(shinytest2)

test_that("metabData: Regular, Rewind, All Models", {
  app <- AppDriver$new(name = "SLOPE-app", height = 1039, width = 1619, timeout = 60000, load_timeout = 60000)
  app$view()
  app$wait_for_idle() #
  app$run_js('$(".cancel").click()')
  app$wait_for_idle() #
  app$click("welcome_confirm")
  app$wait_for_idle() #
  app$set_inputs(data_type = "NMR")
  app$wait_for_idle() #
  app$set_inputs(have_emeta = TRUE)
  app$wait_for_idle() #
  app$set_inputs(use_example = TRUE)
  app$wait_for_idle() #
  app$click("data_type_done")
  app$wait_for_idle() #
  app$set_inputs(datascale = "abundance")
  app$wait_for_idle() #
  app$set_inputs(normalized = "No")
  app$wait_for_idle() #
  app$click("specify_edata_done")
  app$wait_for_idle() #
  app$click("specify_emeta_done")
  app$wait_for_idle() #
  app$click("upload_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(use_fdata = "f_data")
  app$wait_for_idle() #
  app$set_inputs(use_example_fdata = TRUE)
  app$wait_for_idle() #
  app$click("fdata_options_done")
  app$wait_for_idle() #
  app$click("check_group_cols")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("review_upload_done")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_reference_choice = "No")
  app$wait_for_idle() #
  app$click("refnorm_complete")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("LQ_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("outliers_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_sample_miss")
  app$wait_for_idle() #
  app$click("done_biom_miss")
  app$wait_for_idle() #
  app$click("done_md")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("qc_review_done")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Condition")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "default")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_TS_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle()
  app$set_inputs(vi_thresh_count = 15)
  app$wait_for_idle() #
  app$click("feature_select_posthoc")
  app$wait_for_idle() #
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  
  # RF
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_qc")
  app$set_inputs(Nmrdata_reference_choice = "No")
  app$wait_for_idle() #
  app$click("refnorm_complete")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("LQ_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("outliers_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_sample_miss")
  app$wait_for_idle() #
  app$click("done_biom_miss")
  app$wait_for_idle() #
  app$click("done_md")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("qc_review_done")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Condition")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_add_cvfilt = TRUE)
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "default")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_TS_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle() #
  app$click("feature_select_posthoc")
  app$wait_for_idle() #
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_rf.zip")
  app$wait_for_idle() #
  
  
  # PSVM
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_msu")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Condition")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "psvm")
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_add_cvfilt = FALSE)
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "default")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_TS_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle() #
  app$click("feature_select_posthoc")
  app$wait_for_idle() #
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_psvm.zip")
  app$wait_for_idle() #
  
  # LSVM
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_msu")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Condition")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "lsvm")
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "default")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_TS_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle() #
  app$click("feature_select_posthoc")
  app$wait_for_idle() #
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_lsvm.zip")
  app$wait_for_idle() #
  
  # RSVM
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_msu")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Condition")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "rsvm")
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "default")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_TS_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle() #
  app$click("feature_select_posthoc")
  app$wait_for_idle() #
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_rsvm.zip")
  app$wait_for_idle() #
  
  # GBTree
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_msu")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Condition")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "gbtree")
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "default")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_TS_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle()
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_gbtree.zip")
  app$wait_for_idle() #
  
  # PLS
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_msu")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Condition")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "pls")
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "default")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_TS_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle() #
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_pls.zip")
  app$wait_for_idle() #
  
  # Logistic
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_msu")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Condition")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "logistic")
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "default")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_TS_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle() #
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_logistic.zip")
  app$wait_for_idle() #
  
  # Loglasso
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_msu")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Condition")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "loglasso")
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "default")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_TS_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle() #
  app$click("feature_select_posthoc")
  app$wait_for_idle() #
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_loglasso.zip")
  app$wait_for_idle() #
  
  # Multi
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_msu")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Stage")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "multi")
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "los")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "default")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_TS_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle() #
  app$click("feature_select_posthoc")
  app$wait_for_idle() #
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_multi.zip")
  app$wait_for_idle() #
  
  # MultiLASSO
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_msu")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Stage")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "multilasso")
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "los")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "default")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_TS_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle() #
  app$click("feature_select_posthoc")
  app$wait_for_idle() #
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_multilasso.zip")
  app$wait_for_idle() #
  
  # HClust
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_msu")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_unsupervised = "clusters")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "hclust", wait_ = FALSE)
  app$run_js('$(".filter-option").click()')
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "los")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "panerm")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle()
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_multilasso.zip")
  app$wait_for_idle() #
  
  # KMeans
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_msu")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_unsupervised = "clusters")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "kmeans", wait_ = FALSE)
  app$run_js('$(".filter-option").click()')
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "los")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "tuned")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle()
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_multilasso.zip")
  app$wait_for_idle() #
  
  # PCA
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_msu")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_unsupervised = "clusters")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "pca", wait_ = FALSE)
  app$run_js('$(".filter-option").click()')
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "los")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "tuned")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle()
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_multilasso.zip")
  app$wait_for_idle() #
  
  # PPCA
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_msu")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_unsupervised = "clusters")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "ppca", wait_ = FALSE)
  app$run_js('$(".filter-option").click()')
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "los")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "tuned")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle()
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_multilasso.zip")
  app$wait_for_idle() #
  
  # UMAP
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_msu")
  app$wait_for_idle() #
  app$click("vscols_options_done")
  app$wait_for_idle() #
  app$click("vscols_cats_done")
  app$wait_for_idle() #
  app$click("done_VS")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_unsupervised = "clusters")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "umap", wait_ = FALSE)
  app$run_js('$(".filter-option").click()')
  app$wait_for_idle() #
  app$click("em_select")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("msu_review_done")
  app$wait_for_idle() #
  app$click("done_tr_box")
  app$wait_for_idle() #
  app$click("complete_transform")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_subset_fn = "los")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Nmrdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "tuned")
  app$wait_for_idle() #
  app$click("complete_RM_prompts")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_param_option")
  app$wait_for_idle() #
  app$click("complete_param")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("run_sl")
  app$wait_for_idle()
  app$click("complete_RM")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_results_review")
  app$wait_for_idle() #
  app$click("upload_dwn_done")
  app$wait_for_idle() #
  app$click("QC_dwn_done")
  app$wait_for_idle() #
  app$click("MSU_dwn_done")
  app$wait_for_idle() #
  app$click("PP_dwn_done")
  app$wait_for_idle() #
  app$click("RM_dwn_done")
  app$wait_for_idle() #
  app$click("makezipfile")
  app$wait_for_idle() #
  #app$get_download("download_processed_data", filename = "SLOPE-app_nmrData_multilasso.zip")
  app$wait_for_idle() #
  
  testthat::expect(TRUE, "logic has failed")
})