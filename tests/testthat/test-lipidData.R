library(shinytest2)

test_that("lipidData: Regular and Rewind", {
  app <- AppDriver$new(name = "SLOPE-app", height = 1039, width = 1619, timeout = 60000, load_timeout = 60000)
  app$view()
  app$wait_for_idle() #
  app$run_js('$(".cancel").click()')
  app$wait_for_idle() #
  app$click("welcome_confirm")
  app$wait_for_idle() #
  app$set_inputs(data_type = "Positive")
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
  app$wait_for_idle()
  app$set_inputs(missing_options = c("impute"))
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
  app$set_inputs(Lipiddata_add_imputefilt = TRUE)
  app$wait_for_idle() #
  app$click("apply_filters")
  app$wait_for_idle() #
  app$click("dismiss_modal")
  app$wait_for_idle() #
  app$click("complete_filters")
  app$wait_for_idle() #
  app$run_js('$(".confirm").click()')
  app$wait_for_idle() #
  app$set_inputs(Lipiddata_normalize_option = "Global Normalization")
  app$wait_for_idle() #
  app$set_inputs(Lipiddata_norm_fn = "mean")
  app$wait_for_idle() #
  app$set_inputs(Lipiddata_subset_fn = "ppp_rip")
  app$wait_for_idle() #
  app$set_inputs(Lipiddata_backtransform = "FALSE")
  app$wait_for_idle() #
  app$set_inputs(Lipiddata_lock_norm = TRUE)
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
  app$wait_for_idle()
  
  testthat::expect(TRUE, "logic has failed")
})
