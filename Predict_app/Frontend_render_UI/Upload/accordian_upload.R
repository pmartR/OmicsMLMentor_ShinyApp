

## Uploads

observeEvent(input$upload_model_done, {
  updateBoxCollapse(session, id = "upload_collapse", close = "model_upload")
})

observeEvent(input$upload_edata_done, {
  updateBoxCollapse(session, id = "upload_collapse", close = "data_upload_edata")
})

observeEvent(input$upload_fdata_done, {
  updateBoxCollapse(session, id = "upload_collapse", close = "data_upload_fdata")
})

observeEvent(input$upload_emeta_done, {
  updateBoxCollapse(session, id = "upload_collapse", close = "data_upload_emeta")
})


### Specs

observeEvent(input$specify_edata_done, {
  updateBoxCollapse(session, id = "upload_collapse", close = "params_box")
})

observeEvent(input$specify_fdata_done, {
  updateBoxCollapse(session, id = "upload_collapse", close = "data_props_fdata")
})

observeEvent(input$specify_emeta_done, {
  updateBoxCollapse(session, id = "upload_collapse", close = "data_props")
})