
### Picker for experts
output$pick_model_UI <- renderUI({
  
  req(input$skip_ag)
  
  models_supervised_text <-  map_chr(
    as.character(models_long_name %in% models_supervised), 
    function(x)
      switch(
        x,
        "TRUE" = "Requires Sample Information",
        "FALSE" = ""
        )
    )
  
  selected <- isolate(if(!is.null(input$pick_model)) 
    input$pick_model else character(0))
  
  pickerInput("pick_model", label = "Select a model:",
              choices = models_long_name, 
              selected = selected,
              choicesOpt = list(
                disabled = if(is.null(omicsData$objMSU$f_data)) 
                  models_long_name %in% models_supervised else NULL,
                subtext = if(is.null(omicsData$objMSU$f_data)) 
                  models_supervised_text else NULL
                )
  )
  
})


output$ag_prompt_UI <- renderUI({
  
  out <- if(is.null(omicsData$objMSU$f_data)){
    div(
      "Note: When Sample Information is uploaded, additional options are available.",
      radioButtons(
        "ag_prompts",
        label = "",
        choiceNames = choiceNames_ag[4],
        choiceValues = choiceValues_ag[4],
        inline = T,
        selected = choiceNames_ag[c(4)]
      )
    )
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
  div(
    strong("Which sample column(s) would you like to predict?"),
    br(), br(),
    pickerInput(
      "f_data_response_picker",
      "",
      choices = colnames(omicsData$objMSU$f_data)[colnames(omicsData$objMSU$f_data) != input$f_data_id_col],
      multiple = T,
      selected = selected,
      options = list( `live-search` = TRUE, "max-options" = 2),
      width = "60%"
    )
  )
})

output$ag_advanced_UI_select_model <- renderUI({
  # req(input$user_level_pick == "expert")
  checkboxInput("skip_ag", "I know what model I want to run.")
})

output$pick_model_group_pick_UI <- renderUI({
  
  req(!is.null(omicsData$objMSU$f_data) && 
        !is.null(input$pick_model) && input$skip_ag &&
        input$pick_model %in% models_supervised
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
    options = list( `live-search` = TRUE, "max-options" = 2),
    width = "60%"
  )
})

##

observeEvent(input$ag_prompts, {
  if(input$ag_prompts == 'supervised' && is.null(omicsData$objMSU$f_data)){
    
    updateRadioButtons(session, "ag_prompts", selected = "unsupervised")
    shinyalert("Not enough data!", 
               "No Sample Information has been uploaded. Please upload or generate Sample Information to use this option.")
  }
})

observeEvent(input$ag_done, {
  
  expert_cond <- !is.null(input$skip_ag) && 
    input$skip_ag && 
    input$pick_model %in% models_supervised
  
  other_cond <- !is.null(input$ag_prompts) && 
    input$ag_prompts == "supervised"
  
  if(expert_cond){
    
    response <- input$pick_model_group_pick
    
    omicsData$objMSU <- group_designation(omicsData$objMSU, 
                                          main_effects = response)
    
    omicsData$objModel <- as.slData(omicsData$objMSU, 
                                    response_cols = response)
    
  } else if(other_cond){
    
    response <- input$f_data_response_picker
    
    omicsData$objMSU <- group_designation(omicsData$objMSU, 
                                          main_effects = response)
    
    omicsData$objModel <- as.slData(omicsData$objMSU, 
                                    response_cols = response)
    
  } else {
    omicsData$objModel <- as.slData(omicsData$objMSU)
  }

})

observeEvent(input$ag_done, {
  updateBoxCollapse(session, "ag_collapse_center", close = "ag_choices")
})

observeEvent(input$done_md, {
  updateBoxCollapse(session, "ag_collapse_center", close = "md_handling")
  
})

observe({
  if (!is.null(input$ag_prompts) &&
      input$ag_prompts == 'supervised' && 
    !is.null(input$ag_prompts_supervised) &&
    input$ag_prompts_supervised == "accuracy"){
    
    updateCheckboxInput(session = session, "feature_selection", value = F)
    updateCheckboxInput(session = session, "explainability", value = F)
    updateCheckboxInput(session = session, "equation", value = F)
    
  } else {
    updateCheckboxInput(session = session, "feature_selection", value = F)
    updateCheckboxInput(session = session, "explainability", value = F)
    updateCheckboxInput(session = session, "equation", value = F)
  }
  })

# observe({
#   if (!is.null(input$ag_prompts) &&
#       input$ag_prompts == 'unsupervised' &&
#       !is.null(input$ag_prompts_supervised) &&
#       input$ag_prompts_supervised == "clusters"){
#     
#     updateCheckboxInput(session = session, "feature_selection", value = F)
#     
#   }
})

