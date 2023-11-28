

output$pick_model_UI <- renderUI({
  
  req(input$skip_ag)
  
  models <- models_long_name
  
  ## Add to rules ideally
  models_supervised <- map_lgl(models, function(x) algo_rules[[x]]$hard$supervised)
  
  models_supervised_text <-  map_chr(
    as.character(models_supervised), 
    function(x)
      switch(
        x,
        "TRUE" = "Requires sample data",
        "FALSE" = ""
        )
    )
  
  selected <- isolate(if(!is.null(input$pick_model)) input$pick_model else character(0))
  
  pickerInput("pick_model", label = "Select a model:",
              choices = names(models), 
              selected = selected,
              choicesOpt = list(
                disabled = if(is.null(omicsData$objMSU$f_data)) models_supervised else NULL,
                subtext = if(is.null(omicsData$objMSU$f_data)) models_supervised_text else NULL
                )
  )
  
})


output$ag_prompt_UI <- renderUI({
  
  out <- if(is.null(omicsData$objMSU$f_data)){
    disabled(radioButtons(
      "ag_prompts",
      label = "",
      choiceNames = choiceNames_ag[c(1, 4)],
      choiceValues = choiceValues_ag[c(1, 4)],
      inline = T,
      selected = choiceNames_ag[c(4)]
    ))
  } else {
    
    selected <- isolate(if(is.null(input$ag_prompts)) character(0) else {
      input$ag_prompts
    })
    
    radioButtons(
      "ag_prompts",
      label = "",
      choiceNames = choiceNames_ag[c(1, 4)],
      choiceValues = choiceValues_ag[c(1, 4)],
      inline = T,
      selected = selected
    )
  }
  
})

output$f_data_response_picker_UI <- renderUI({
  
  req(!is.null(omicsData$objMSU$f_data))
  
  selected <- isolate(if(is.null(input$f_data_response_picker)) logical(0) else {
    input$f_data_response_picker
  })
  
  pickerInput(
    "f_data_response_picker",
    "Which sample column(s) would you like to predict?",
    choices = colnames(omicsData$objMSU$f_data)[colnames(omicsData$objMSU$f_data) != input$f_data_id_col],
    multiple = T,
    selected = selected,
    options = list( `live-search` = TRUE, "max-options" = 2),
    width = "30%"
  )
})

output$pick_model_group_pick_UI <- renderUI({
  
  req(!is.null(omicsData$objMSU$f_data) && 
        !is.null(input$pick_model) && input$skip_ag &&
        
        input$pick_model %in% names(models_long_name)[
          models_long_name %in% names(algo_rules)[
            map_lgl(algo_rules, function(x) x$hard$supervised)
            ]
          ]
        )
  
  selected <- isolate(if(is.null(input$pick_model_group_pick)) logical(0) else {
    input$pick_model_group_pick
  })
  
  pickerInput(
    "pick_model_group_pick",
    "Which sample column(s) would you like to predict?",
    choices = colnames(omicsData$objMSU$f_data)[colnames(omicsData$objMSU$f_data) != input$f_data_id_col],
    multiple = T,
    selected = selected,
    options = list( `live-search` = TRUE, "max-options" = 2)
  )
})

##

observeEvent(input$ag_prompts, {
  if(input$ag_prompts == 'supervised' && is.null(omicsData$objMSU$f_data)){
    
    updateRadioButtons(session, "ag_prompts", selected = "unsupervised")
    shinyalert("Not enough data!", 
               "No sample data has been uploaded. Please upload or generate sample data to use this option.")
  }
})

observeEvent(input$ag_done, {
  
  if(isTruthy(input$skip_ag)){
    response <- input$pick_model_group_pick
  } else {
    response <- input$f_data_response_picker
  }
  
  if(!is.null(response)){
    omicsData$objMSU <- group_designation(omicsData$objMSU, 
                                          main_effects = response)
  }
  
  omicsData$objModel <- as.slData(omicsData$objMSU, 
                                response_cols = response)
  
})

observeEvent(input$ag_done, {
  updateBoxCollapse(session, "ag_collapse_center", close = "ag_choices")
})

observeEvent(input$done_md, {
  updateBoxCollapse(session, "ag_collapse_center", close = "md_handling")
  
})



