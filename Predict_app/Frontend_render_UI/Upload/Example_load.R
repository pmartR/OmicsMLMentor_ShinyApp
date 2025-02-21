
#### Loads

## Backend
observeEvent(input$load_example, {
  
  # fp <- "./Predict_app/example/data/unsup_models/Full_model_hclust_lipid_fdata.RDS"
  # fp <- "./Predict_app/example/data/unsup_models/Full_model_kmeans_fdata.RDS"
  # fp <- "./Predict_app/example/data/unsup_models/Full_model_ppca.RDS"
  # fp <- "./Predict_app/example/data/unsup_models/Full_model_SLOPE_kmeans.RDS"
  # fp <- "./Predict_app/example/data/unsup_models/Full_model_SLOPE_model_pca_fdata.RDS"
  fp <- "./Predict_app/example/data/unsup_models/umap.RDS"
  
  # fp <-"./Predict_app/example/data/Full_model_SLOPE_model_2025_01_15_16_17_29.638369.RDS"
  
  reactive_dataholder$model <- readRDS(fp)
  # reactive_dataholder$e_data$file <- pmartRdata::pep_edata
  # reactive_dataholder$e_meta$file <- pmartRdata::pep_emeta
  # reactive_dataholder$f_data$file <- pmartRdata::pep_fdata
  
  reactive_dataholder$e_data$file <- pmartRdata::lipid_neg_edata
  # reactive_dataholder$e_meta$file <- pmartRdata::pep_emeta
  reactive_dataholder$f_data$file <- pmartRdata::lipid_neg_fdata
  reactive_dataholder$f_data$file$SampleID <- paste0(reactive_dataholder$f_data$file$SampleID, "_new")
  colnames(reactive_dataholder$e_data$file) <- paste0(colnames(reactive_dataholder$e_data$file), "_new")
  
  
  reactive_dataholder$model$file <- attr(reactive_dataholder$model$model, "response_performance")
  
})


## Frontend
observeEvent(input$load_example, {
  
  map(c("e_data", "f_data", "e_meta", "model"), function(label) {
    
    tablabel <- switch(label,
                       e_data = "Abundance data .csv",
                       f_data = "Sample Information .csv",
                       e_meta = "Biomolecule information .csv",
                       model = "Model .RDS"
    )
    
    appendTab("preview_data", 
              select = T,
              tabPanel(tablabel,
                       br(),
                       DTOutput(paste0("DT_", label)),
                       br()
              ),
              session = session
    )
    
  })
  
  updateBoxCollapse(session, "", open = c("params_box", "data_props_fdata", "data_props"), 
                    close = c("model_upload", "data_upload_edata", "data_upload_fdata", "data_upload_emeta"))
  
})

