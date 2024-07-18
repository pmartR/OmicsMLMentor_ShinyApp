library(shinytest2)

test_that("proData: Regular, Rewind, All Models", {
  app <- AppDriver$new(name = "SLOPE-app", height = 1039, width = 1619, timeout = 60000, load_timeout = 60000)
  app$view()
  app$wait_for_idle() #
  app$run_js('$(".cancel").click()')
  app$wait_for_idle() #
  app$click("welcome_confirm")
  app$wait_for_idle() #
  app$set_inputs(data_type = "Protein")
  app$wait_for_idle() #
  #app$set_inputs(have_emeta = TRUE)
  #app$wait_for_idle() #
  app$set_inputs(use_example = TRUE)
  app$wait_for_idle() #
  app$click("data_type_done")
  app$wait_for_idle() #
  app$set_inputs(datascale = "log2")
  app$wait_for_idle() #
  app$set_inputs(normalized = "Yes")
  app$wait_for_idle() #
  app$click("specify_edata_done")
  app$wait_for_idle() #
  #app$click("specify_emeta_done")
  #app$wait_for_idle() #
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
  app$set_inputs(f_data_response_picker = "Phenotype")
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
  app$set_inputs(Prodata_add_imputefilt = TRUE)
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
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
  app$run_js('$(".cancel").click()')

  # RF
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_qc")
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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Phenotype", wait_ = FALSE)
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
  app$set_inputs(pick_model_EM = "rf")
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
  app$set_inputs(Prodata_add_imputefilt = FALSE)
  app$wait_for_idle() #
  app$set_inputs(Prodata_add_imputefilt = TRUE)
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_norm")
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
  app$run_js('$(".cancel").click()')

  # PSVM
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_qc")
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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Phenotype", wait_ = FALSE)
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
  app$set_inputs(Prodata_add_imputefilt = FALSE)
  app$wait_for_idle() #
  app$set_inputs(Prodata_add_imputefilt = TRUE)
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_norm")
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
  app$run_js('$(".cancel").click()')
  
  # LSVM
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_qc")
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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Phenotype", wait_ = FALSE)
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
  app$set_inputs(Prodata_add_imputefilt = FALSE)
  app$wait_for_idle() #
  app$set_inputs(Prodata_add_imputefilt = TRUE)
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_norm")
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
  app$run_js('$(".cancel").click()')
  
  # RSVM
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_qc")
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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Phenotype", wait_ = FALSE)
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
  app$set_inputs(Prodata_add_imputefilt = FALSE)
  app$wait_for_idle() #
  app$set_inputs(Prodata_add_imputefilt = TRUE)
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_norm")
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
  app$run_js('$(".cancel").click()')
  
  # GBTree
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_qc")
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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Phenotype", wait_ = FALSE)
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
  app$set_inputs(Prodata_add_imputefilt = FALSE)
  app$wait_for_idle() #
  app$set_inputs(Prodata_add_imputefilt = TRUE)
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_norm")
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
  app$run_js('$(".cancel").click()')
  
  # PLS
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_qc")
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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Phenotype", wait_ = FALSE)
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
  app$set_inputs(Prodata_add_imputefilt = FALSE)
  app$wait_for_idle() #
  app$set_inputs(Prodata_add_imputefilt = TRUE)
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_norm")
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
  app$run_js('$(".cancel").click()')
  
  # Logistic
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_qc")
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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "SecondPhenotype", wait_ = FALSE)
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
  app$set_inputs(Prodata_add_imputefilt = FALSE)
  app$wait_for_idle() #
  app$set_inputs(Prodata_add_imputefilt = TRUE)
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_norm")
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
  app$run_js('$(".cancel").click()')
  
  # LogLASSO
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_qc")
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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "SecondPhenotype", wait_ = FALSE)
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
  app$set_inputs(Prodata_add_imputefilt = FALSE)
  app$wait_for_idle() #
  app$set_inputs(Prodata_add_imputefilt = TRUE)
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_norm")
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
  app$run_js('$(".cancel").click()')
  
  # Multi
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_qc")
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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Phenotype", wait_ = FALSE)
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
  app$set_inputs(Prodata_add_imputefilt = FALSE)
  app$wait_for_idle() #
  app$set_inputs(Prodata_add_imputefilt = TRUE)
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_norm")
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
  app$run_js('$(".cancel").click()')
  
  # MultiLASSO
  app$click("new_model")
  app$wait_for_idle() #
  app$click("rewind_qc")
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
  app$set_inputs(ag_prompts = "unsupervised")
  app$wait_for_idle() #
  app$set_inputs(ag_prompts = "supervised")
  app$wait_for_idle() #
  app$set_inputs(f_data_response_picker = "Phenotype", wait_ = FALSE)
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
  app$set_inputs(Prodata_add_imputefilt = FALSE)
  app$wait_for_idle() #
  app$set_inputs(Prodata_add_imputefilt = TRUE)
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_norm")
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
  app$click("complete_norm")
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
  app$click("makezipfile")
  app$wait_for_idle() #
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
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_norm")
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
  app$set_inputs(f_data_response_picker = "Phenotype", wait_ = FALSE)
  app$run_js('$(".filter-option").click()')
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
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_norm")
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
  app$set_inputs(f_data_response_picker = "Phenotype", wait_ = FALSE)
  app$run_js('$(".filter-option").click()')
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
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$click("complete_norm")
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
  app$run_js('$(".cancel").click()')
  
  # UMAP
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
  # app$set_inputs(ag_prompts = "supervised")
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts = "unsupervised")
  # app$wait_for_idle() #
  # app$set_inputs(f_data_response_picker = "Phenotype", wait_ = FALSE)
  # app$run_js('$(".filter-option").click()')
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts_unsupervised = "variation source")
  # app$wait_for_idle() #
  # app$set_inputs(ag_prompts_unsupervised = "clusters")
  # app$wait_for_idle() #
  # app$click("ag_done")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$set_inputs(pick_model_EM = "umap", wait_ = FALSE)
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
  # app$click("apply_filters")
  # app$wait_for_idle() #
  # app$click("dismiss_modal")
  # app$wait_for_idle() #
  # app$click("complete_filters")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("complete_norm")
  # app$wait_for_idle() #
  # app$run_js('$(".confirm").click()')
  # app$wait_for_idle() #
  # app$click("complete_ppreview")
  # app$wait_for_idle() #
  # app$set_inputs(rm_prompts_hp = "custom")
  # app$wait_for_idle() #
  # app$set_inputs(rm_prompts_hp = "tuned")
  # app$wait_for_idle() #
  # app$click("complete_RM_prompts")
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
  # app$wait_for_idle()
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
  # app$click("makezipfile")
  # app$wait_for_idle() #
  
  testthat::expect(TRUE, "logic has failed")
})

