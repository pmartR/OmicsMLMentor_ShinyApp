library(shinytest2)
options(parallelly.makeNodePSOCK.setup_strategy = "sequential")

test_that("isobaricpepData: Regular and Rewind", {
  app <- AppDriver$new(name = "SLOPE-app", height = 1039, width = 1619, timeout = 60000, load_timeout = 60000)
  options(parallelly.makeNodePSOCK.setup_strategy = "sequential")
  
  tryCatch({
  app$view()
  app$wait_for_idle() #
  app$run_js('$(".cancel").click()')
  app$wait_for_idle() #
  app$set_inputs(user_level_pick = "expert")
  app$wait_for_idle() #
  app$click("welcome_confirm")
  app$wait_for_idle() #
  app$set_inputs(data_type = "Isobaric")
  app$wait_for_idle() #
  app$set_inputs(use_example = TRUE, wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("data_type_done", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000) #
  Sys.sleep(10)
  app$wait_for_idle(timeout = 15 * 60000) #
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
  app$click("specify_fdata_done")
  app$wait_for_idle() #
  app$click("check_group_cols", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000) #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("review_upload_done")
  app$wait_for_idle() #
  app$click("Isobaricpepdata_ref_done_idcols")
  app$wait_for_idle() #
  app$click("refnorm_complete")
  app$wait_for_idle(timeout = 15 * 60000) #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("LQ_done", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$run_js('$(".confirm").click()')
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("outliers_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_sample_miss")
  app$wait_for_idle() #
  app$set_inputs(qc_which_rollup = "rrollup")
  app$wait_for_idle() #
  app$set_inputs(qc_which_combine_fn = "median")
  app$wait_for_idle() #
  app$click("qc_apply_rollup", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$set_inputs(missing_options = c("impute"))
  app$wait_for_idle() #
  app$set_inputs(missing_options = c("impute", "convert"))
  app$wait_for_idle() #
  app$set_inputs(missing_options = c("impute", "convert", "remove"))
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
  app$set_inputs(f_data_response_picker = "Virus")
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
  app$set_inputs(Pepdata_add_imputefilt = TRUE, wait_ = FALSE)
  browser()
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("apply_filters", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("dismiss_modal")
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Pepdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')  # Trigger on "Note"
  app$wait_for_idle() #
  app$set_inputs(Pepdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("Pepdata_apply_rollup", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("complete_rollup")
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
  app$wait_for_idle(timeout = 15 * 60000)
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
  app$click("makezipfile", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  #app$expect_download("download_processed_data")
  
  # # RF
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_qc", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("Isobaricpepdata_ref_done_idcols")
  app$wait_for_idle() #
  app$click("refnorm_complete", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000) #
  Sys.sleep(10)
  app$wait_for_idle(timeout = 15 * 60000) #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("LQ_done", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$run_js('$(".confirm").click()')
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("outliers_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("done_sample_miss")
  app$wait_for_idle() #
  app$set_inputs(qc_which_rollup = "rrollup")
  app$wait_for_idle() #
  app$set_inputs(qc_which_combine_fn = "median")
  app$wait_for_idle() #
  app$click("qc_apply_rollup", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$set_inputs(missing_options = c("impute"))
  app$wait_for_idle() #
  app$set_inputs(missing_options = c("impute", "convert"))
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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Virus", wait_ = FALSE)
  app$run_js('$(".filter-option").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "variable importance")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "rf", wait_ = FALSE)
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
  app$set_inputs(Pepdata_add_imputefilt = TRUE, wait_ = FALSE)
  browser()
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("apply_filters", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("dismiss_modal")
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Pepdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')  # Trigger on "Note"
  app$wait_for_idle() #
  app$set_inputs(Pepdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("Pepdata_apply_rollup", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("complete_rollup")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "train")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "tuned")
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
  app$wait_for_idle(timeout = 15 * 60000)
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
  app$click("makezipfile", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)

  # PSVM
  # app$click("new_model")
  # app$wait_for_idle() #
  # app$click("rewind_msu")
  # app$wait_for_idle() #
  # app$click("vscols_options_done")
  # app$wait_for_idle() #
  # app$click("vscols_cats_done")
  # app$wait_for_idle() #
  # app$click("done_VS")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts = "unsupervised")
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts = "supervised")
  # app$wait_for_idle() #
  # app$set_inputs(f_data_response_picker = "Virus", wait_ = FALSE)
  # app$run_js('$(".filter-option").click()')
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts_supervised = "variable importance")
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts_supervised = "accuracy")
  # app$wait_for_idle() #
  # app$click("ag_done")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$set_inputs(pick_model_EM = "psvm", wait_ = FALSE)
  # app$run_js('$(".filter-option").click()')
  # app$wait_for_idle() #
  # app$click("em_select")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("msu_review_done")
  # app$wait_for_idle() #
  # app$click("done_tr_box")
  # app$wait_for_idle() #
  # app$click("complete_transform")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_add_imputefilt = TRUE, wait_ = FALSE)
  # app$wait_for_idle(timeout = 15 * 60000)
  # app$click("apply_filters", wait_ = FALSE)
  # app$wait_for_idle(timeout = 15 * 60000)
  # app$click("dismiss_modal")
  # app$wait_for_idle(timeout = 15 * 60000) #
  # app$click("complete_filters")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_normalize_option = "Global Normalization")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')  # Trigger on "Note"
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_norm_fn = "mean")
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_subset_fn = "ppp_rip")
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_backtransform = "FALSE")
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_lock_norm = TRUE)
  # app$wait_for_idle() #
  # app$click("complete_norm")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("Pepdata_apply_rollup", wait_ = FALSE)
  # app$wait_for_idle(timeout = 15 * 60000)
  # app$click("complete_rollup")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("complete_ppreview")
  # app$wait_for_idle() #
  # app$set_inputs(rm_prompts_train = "train")
  # app$wait_for_idle() #
  # app$set_inputs(rm_prompts_train = "notrain")
  # app$wait_for_idle() #
  # app$set_inputs(rm_prompts_hp = "tuned")
  # app$wait_for_idle() #
  # app$set_inputs(rm_prompts_hp = "default")
  # app$wait_for_idle() #
  # app$click("complete_RM_prompts")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("complete_TS_RM")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("done_param_option")
  # app$wait_for_idle() #
  # app$click("complete_param")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("run_sl")
  # app$wait_for_idle(timeout = 15 * 60000)
  # app$set_inputs(vi_thresh_count = 15)
  # app$wait_for_idle() #
  # app$click("feature_select_posthoc")
  # app$wait_for_idle() #
  # app$click("complete_RM")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("complete_results_review")
  # app$wait_for_idle() #
  # app$click("upload_dwn_done")
  # app$wait_for_idle() #
  # app$click("QC_dwn_done")
  # app$wait_for_idle() #
  # app$click("MSU_dwn_done")
  # app$wait_for_idle() #
  # app$click("PP_dwn_done")
  # app$wait_for_idle() #
  # app$click("RM_dwn_done")
  # app$wait_for_idle() #
  # app$click("makezipfile", wait_ = FALSE)
  # app$wait_for_idle(timeout = 15 * 60000)

  # RSVM
  # app$click("new_model")
  # app$wait_for_idle() #
  # app$click("rewind_msu")
  # app$wait_for_idle() #
  # app$click("vscols_options_done")
  # app$wait_for_idle() #
  # app$click("vscols_cats_done")
  # app$wait_for_idle() #
  # app$click("done_VS")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts = "unsupervised")
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts = "supervised")
  # app$wait_for_idle() #
  # app$set_inputs(f_data_response_picker = "Virus", wait_ = FALSE)
  # app$run_js('$(".filter-option").click()')
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts_supervised = "variable importance")
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts_supervised = "accuracy")
  # app$wait_for_idle() #
  # app$click("ag_done")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$set_inputs(pick_model_EM = "rsvm", wait_ = FALSE)
  # app$run_js('$(".filter-option").click()')
  # app$wait_for_idle() #
  # app$click("em_select")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("msu_review_done")
  # app$wait_for_idle() #
  # app$click("done_tr_box")
  # app$wait_for_idle() #
  # app$click("complete_transform")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_add_imputefilt = TRUE, wait_ = FALSE)
  # app$wait_for_idle(timeout = 15 * 60000)
  # app$click("apply_filters", wait_ = FALSE)
  # app$wait_for_idle(timeout = 15 * 60000)
  # app$click("dismiss_modal")
  # app$wait_for_idle(timeout = 15 * 60000) #
  # app$click("complete_filters")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_normalize_option = "Global Normalization")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')  # Trigger on "Note"
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_norm_fn = "mean")
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_subset_fn = "ppp_rip")
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_backtransform = "FALSE")
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_lock_norm = TRUE)
  # app$wait_for_idle() #
  # app$click("complete_norm")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("Pepdata_apply_rollup", wait_ = FALSE)
  # app$wait_for_idle(timeout = 15 * 60000)
  # app$click("complete_rollup")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("complete_ppreview")
  # app$wait_for_idle() #
  # app$set_inputs(rm_prompts_train = "train")
  # app$wait_for_idle() #
  # app$set_inputs(rm_prompts_train = "notrain")
  # app$wait_for_idle() #
  # app$set_inputs(rm_prompts_hp = "tuned")
  # app$wait_for_idle() #
  # app$set_inputs(rm_prompts_hp = "default")
  # app$wait_for_idle() #
  # app$click("complete_RM_prompts")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("complete_TS_RM")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("done_param_option")
  # app$wait_for_idle() #
  # app$click("complete_param")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("run_sl")
  # app$wait_for_idle(timeout = 15 * 60000)
  # app$set_inputs(vi_thresh_count = 15)
  # app$wait_for_idle() #
  # app$click("feature_select_posthoc")
  # app$wait_for_idle() #
  # app$click("complete_RM")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("complete_results_review")
  # app$wait_for_idle() #
  # app$click("upload_dwn_done")
  # app$wait_for_idle() #
  # app$click("QC_dwn_done")
  # app$wait_for_idle() #
  # app$click("MSU_dwn_done")
  # app$wait_for_idle() #
  # app$click("PP_dwn_done")
  # app$wait_for_idle() #
  # app$click("RM_dwn_done")
  # app$wait_for_idle() #
  # app$click("makezipfile", wait_ = FALSE)
  # app$wait_for_idle(timeout = 15 * 60000)

  # LSVM
  # app$click("new_model")
  # app$wait_for_idle() #
  # app$click("rewind_msu")
  # app$wait_for_idle() #
  # app$click("vscols_options_done")
  # app$wait_for_idle() #
  # app$click("vscols_cats_done")
  # app$wait_for_idle() #
  # app$click("done_VS")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts = "unsupervised")
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts = "supervised")
  # app$wait_for_idle() #
  # app$set_inputs(f_data_response_picker = "Virus", wait_ = FALSE)
  # app$run_js('$(".filter-option").click()')
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts_supervised = "variable importance")
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts_supervised = "accuracy")
  # app$wait_for_idle() #
  # app$click("ag_done")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$set_inputs(pick_model_EM = "lsvm", wait_ = FALSE)
  # app$run_js('$(".filter-option").click()')
  # app$wait_for_idle() #
  # app$click("em_select")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("msu_review_done")
  # app$wait_for_idle() #
  # app$click("done_tr_box")
  # app$wait_for_idle() #
  # app$click("complete_transform")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_add_imputefilt = TRUE, wait_ = FALSE)
  # app$wait_for_idle(timeout = 15 * 60000)
  # app$click("apply_filters", wait_ = FALSE)
  # app$wait_for_idle(timeout = 15 * 60000)
  # app$click("dismiss_modal")
  # app$wait_for_idle(timeout = 15 * 60000) #
  # app$click("complete_filters")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_normalize_option = "Global Normalization")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')  # Trigger on "Note"
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_norm_fn = "mean")
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_subset_fn = "ppp_rip")
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_backtransform = "FALSE")
  # app$wait_for_idle() #
  # app$set_inputs(Pepdata_lock_norm = TRUE)
  # app$wait_for_idle() #
  # app$click("complete_norm")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("Pepdata_apply_rollup", wait_ = FALSE)
  # app$wait_for_idle(timeout = 15 * 60000)
  # app$click("complete_rollup")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("complete_ppreview")
  # app$wait_for_idle() #
  # app$set_inputs(rm_prompts_train = "train")
  # app$wait_for_idle() #
  # app$set_inputs(rm_prompts_train = "notrain")
  # app$wait_for_idle() #
  # app$set_inputs(rm_prompts_hp = "tuned")
  # app$wait_for_idle() #
  # app$set_inputs(rm_prompts_hp = "default")
  # app$wait_for_idle() #
  # app$click("complete_RM_prompts")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("complete_TS_RM")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("done_param_option")
  # app$wait_for_idle() #
  # app$click("complete_param")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("run_sl")
  # app$wait_for_idle(timeout = 15 * 60000)
  # app$set_inputs(vi_thresh_count = 15)
  # app$wait_for_idle() #
  # app$click("feature_select_posthoc")
  # app$wait_for_idle() #
  # app$click("complete_RM")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("complete_results_review")
  # app$wait_for_idle() #
  # app$click("upload_dwn_done")
  # app$wait_for_idle() #
  # app$click("QC_dwn_done")
  # app$wait_for_idle() #
  # app$click("MSU_dwn_done")
  # app$wait_for_idle() #
  # app$click("PP_dwn_done")
  # app$wait_for_idle() #
  # app$click("RM_dwn_done")
  # app$wait_for_idle() #
  # app$click("makezipfile", wait_ = FALSE)
  # app$wait_for_idle(timeout = 15 * 60000)

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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Virus", wait_ = FALSE)
  app$run_js('$(".filter-option").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "variable importance")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "gbtree", wait_ = FALSE)
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
  app$set_inputs(Pepdata_add_imputefilt = TRUE, wait_ = FALSE)
  browser()
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("apply_filters", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("dismiss_modal")
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Pepdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')  # Trigger on "Note"
  app$wait_for_idle() #
  app$set_inputs(Pepdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("Pepdata_apply_rollup", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("complete_rollup")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "train")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "tuned")
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
  app$wait_for_idle(timeout = 15 * 60000)
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
  app$click("makezipfile", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$run_js('$(".cancel").click()')

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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Virus", wait_ = FALSE)
  app$run_js('$(".filter-option").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "variable importance")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "pls", wait_ = FALSE)
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
  app$set_inputs(Pepdata_add_imputefilt = TRUE, wait_ = FALSE)
  browser()
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("apply_filters", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("dismiss_modal")
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Pepdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')  # Trigger on "Note"
  app$wait_for_idle() #
  app$set_inputs(Pepdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("Pepdata_apply_rollup", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("complete_rollup")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "train")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "tuned")
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
  app$wait_for_idle(timeout = 15 * 60000)
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
  app$click("makezipfile", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$run_js('$(".cancel").click()')

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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Virus", wait_ = FALSE)
  app$run_js('$(".filter-option").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "variable importance")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "multi", wait_ = FALSE)
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
  app$set_inputs(Pepdata_add_imputefilt = TRUE, wait_ = FALSE)
  browser()
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("apply_filters", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("dismiss_modal")
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Pepdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')  # Trigger on "Note"
  app$wait_for_idle() #
  app$set_inputs(Pepdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("Pepdata_apply_rollup", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("complete_rollup")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "train")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "tuned")
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
  app$wait_for_idle(timeout = 15 * 60000)
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
  app$click("makezipfile", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$run_js('$(".cancel").click()')

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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Virus", wait_ = FALSE)
  app$run_js('$(".filter-option").click()')
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "variable importance")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_supervised = "accuracy")
  app$wait_for_idle() #
  app$click("ag_done")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(pick_model_EM = "multilasso", wait_ = FALSE)
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
  app$set_inputs(Pepdata_add_imputefilt = TRUE, wait_ = FALSE)
  browser()
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("apply_filters", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("dismiss_modal")
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Pepdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')  # Trigger on "Note"
  app$wait_for_idle() #
  app$set_inputs(Pepdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("Pepdata_apply_rollup", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("complete_rollup")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "train")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_train = "notrain")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "tuned")
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
  app$wait_for_idle(timeout = 15 * 60000)
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
  app$click("makezipfile", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$run_js('$(".cancel").click()')

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
  app$set_inputs(Pepdata_add_imputefilt = TRUE, wait_ = FALSE)
  browser()
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("apply_filters", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("dismiss_modal")
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Pepdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')  # Trigger on "Note"
  app$wait_for_idle() #
  app$set_inputs(Pepdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_subset_fn = "los")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("Pepdata_apply_rollup", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("complete_rollup")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "custom")
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
  app$wait_for_idle(timeout = 15 * 60000)
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
  app$click("makezipfile", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$run_js('$(".cancel").click()')

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
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_unsupervised = "variation source")
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
  app$set_inputs(Pepdata_add_imputefilt = TRUE, wait_ = FALSE)
  browser()
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("apply_filters", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("dismiss_modal")
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Pepdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')  # Trigger on "Note"
  app$wait_for_idle() #
  app$set_inputs(Pepdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_subset_fn = "los")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("Pepdata_apply_rollup", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("complete_rollup")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "custom")
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
  app$wait_for_idle(timeout = 15 * 60000)
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
  app$click("makezipfile", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$run_js('$(".cancel").click()')

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
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_unsupervised = "variation source")
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
  app$set_inputs(Pepdata_add_imputefilt = TRUE, wait_ = FALSE)
  browser()
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("apply_filters", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("dismiss_modal")
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Pepdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')  # Trigger on "Note"
  app$wait_for_idle() #
  app$set_inputs(Pepdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_subset_fn = "los")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("Pepdata_apply_rollup", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("complete_rollup")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "custom")
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
  app$wait_for_idle(timeout = 15 * 60000)
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
  app$click("makezipfile", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$run_js('$(".cancel").click()')
  
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
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_unsupervised = "variation source")
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
  app$set_inputs(Pepdata_add_imputefilt = TRUE, wait_ = FALSE)
  browser()
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("apply_filters", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("dismiss_modal")
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Pepdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')  # Trigger on "Note"
  app$wait_for_idle() #
  app$set_inputs(Pepdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_subset_fn = "los")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("Pepdata_apply_rollup", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("complete_rollup")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "custom")
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
  app$wait_for_idle(timeout = 15 * 60000)
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
  app$click("makezipfile", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$run_js('$(".cancel").click()')
  
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
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts_unsupervised = "variation source")
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
  app$set_inputs(Pepdata_add_imputefilt = TRUE, wait_ = FALSE)
  browser()
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("apply_filters", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("dismiss_modal")
  app$wait_for_idle(timeout = 15 * 60000) #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Pepdata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')  # Trigger on "Note"
  app$wait_for_idle() #
  app$set_inputs(Pepdata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_subset_fn = "los")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Pepdata_lock_norm = TRUE)
  app$wait_for_idle() #
  app$click("complete_norm")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("Pepdata_apply_rollup", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$click("complete_rollup")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_ppreview")
  app$wait_for_idle() #
  app$set_inputs(rm_prompts_hp = "custom")
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
  app$wait_for_idle(timeout = 15 * 60000)
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
  app$click("makezipfile", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 60000)
  app$run_js('$(".cancel").click()')

  }, error = function(e){
    log_temp <<- app$get_logs()
    print(log_temp)
    print(e$message)
    browser()
    testthat::expect(FALSE, "logic has failed")
  })
  
  testthat::expect(TRUE, "logic has failed")
})
