

## When the done buttons should enable

#### Loads
observeEvent(reactive_dataholder$model, {
  
  if(is.null(reactive_dataholder$model)){
    disable(id = "upload_model_done")
  } else {
    enable(id = "upload_model_done")
  }
  
})

observeEvent(reactive_dataholder$e_data, {
  
  if(is.null(reactive_dataholder$e_data)){
    disable(id = "upload_edata_done")
  } else {
    enable(id = "upload_edata_done")
  }
  
})

observeEvent(c(input$have_emeta, reactive_dataholder$e_meta), {
  
  if(is.null(input$have_emeta) || 
     (input$have_emeta == "Yes" && is.null(reactive_dataholder$e_meta))){
    disable(id = "upload_emeta_done")
  } else {
    enable(id = "upload_emeta_done")
  }
})

observeEvent(c(input$use_fdata, reactive_dataholder$f_data), {
  
  if(is.null(input$use_fdata) || 
     (input$use_fdata == "Yes" && is.null(reactive_dataholder$f_data))){
    disable(id = "upload_fdata_done")
  } else {
    enable(id = "upload_fdata_done")
  }
})


#### Specs

observeEvent(
  c(
    input$e_data_id_col, 
    input$datascale,
    input$normalized,
    input$na_symbol,
    input$pick_dt
    ), {
    
  conds <- map_lgl(
    c("e_data_id_col", "datascale", "normalized", "na_symbol", "pick_dt"),
    function(str) is.null(input[[str]])
    )
  
  dt <- class(reactive_dataholder$model$model$norm_omics)
  
  if(!(dt == "proData")){
    conds <- conds[-length(conds)]
  }
      
  if(any(conds)){
    disable(id = "specify_edata_done")
  } else {
    enable(id = "specify_edata_done")
  }
})

observeEvent(
  c(
    input$use_fdata, 
    input$f_data_id_col,
    input$f_data_response_col
  ), {
    
    conds <- map_lgl(
      c("use_fdata", "f_data_id_col", "f_data_response_col"),
      function(str) is.null(input[[str]])
    )
    
    if(input$use_fdata == "No"){
      conds <- conds[1]
    }
    
    if(any(conds)){
      disable(id = "specify_fdata_done")
    } else {
      enable(id = "specify_fdata_done")
    }
  })


observeEvent(
  c(
    input$have_emeta, 
    input$e_meta_id_col
  ), {
    
    conds <- map_lgl(
      c("have_emeta", "e_meta_id_col"),
      function(str) is.null(input[[str]])
    )
    
    if(input$have_emeta == "No"){
      conds <- conds[1]
    }
    
    if(any(conds)){
      disable(id = "specify_emeta_done")
    } else {
      enable(id = "specify_emeta_done")
    }
  })


## When the confirm button should show
observeEvent(
  c(
    input$have_emeta, 
    input$specify_fdata_done,
    input$use_fdata,
    input$specify_emeta_done
  ), {

    
    conds <- is.null(input$have_emeta) || 
      (input$have_emeta == "Yes" && 
         (is.null(input$specify_emeta_done) || input$specify_emeta_done < 1)) ||
      (is.null(input$use_fdata) && input$specify_fdata_done < 1)
    
    if(conds){
      disable(id = "check_selections_upload")
    } else {
      enable(id = "check_selections_upload")
    }
  })


## Confirm triggers the make data
observeEvent(omicsData$obj, once = T, {
  
  shinyalert(title = "Success!", "Continue to next page or review results?",
             showCancelButton = T, closeOnEsc = F, 
             confirmButtonText = "Continue",
             cancelButtonText = "Review results",
             callbackR = function(value){
               if(value){
                 updateNavbarPage(session = session, "top_page", selected = "Pre-processing")
               }
             })
  
})
