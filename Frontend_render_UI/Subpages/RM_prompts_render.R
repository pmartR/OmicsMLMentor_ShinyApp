
## Examples
rm_text <- c(
  "Use user-specified settings",
  "Use default settings supported in literature",
  "Optimize settings for best performance",
  "Understand relationships within this dataset",
  "Apply model to new data"
)

files_rm <- list.files("./www/Example_rm")

p1_list <- files_rm[seq(to = length(files_rm), by = 2)]
p2_list <- files_rm[seq(2, to = length(files_rm), by = 2)]

choiceValues_rm <- list(
  "custom",
  "default",
  "tuned",
  "notrain",
  "train"
)

choiceNames_rm <- pmap(list(rm_text, files_rm), 
                       function(text, p1){
                         
                         div(id="pane",
                             wellPanel(
                               text,
                               br(),
                               br(),
                               # splitLayout(
                                 img(src = paste0("Example_rm/", p1), width = "95%", height = "95%")
                                 # ),
                                 # img(src = paste0("Example_ag/", p2), width = "95%", height = "95%"
                                 # )
                               # )
                             ), tags$style(type="text/css","#pane{font-size:14px;width:405px;}")
                         )
                       })

output$rm_prompt_train_UI <- renderUI({
  
  # out <- if(is.null(omicsData$objPP$f_data)){
  #   disabled(radioButtons(
  #     "rm_prompts_train",
  #     label = "",
  #     choiceNames = choiceNames_rm[4:5],
  #     choiceValues = choiceValues_rm[4:5],
  #     inline = T,
  #     selected = character(0)
  #   ))
  # } else {
    
    selected <- isolate(if(is.null(input$rm_prompts_train)) character(0) else {
      input$rm_prompts_train
    })
    
    radioButtons(
      "rm_prompts_train",
      label = "",
      choiceNames = choiceNames_rm[4:5],
      choiceValues = choiceValues_rm[4:5],
      inline = T,
      selected = selected
    )
  # }
  
})


output$rm_prompt_hp_UI <- renderUI({
  
  input$rm_prompts_train
  
  # out <- if(is.null(omicsData$objPP$f_data)){
  #   disabled(radioButtons(
  #     "rm_prompts_hp",
  #     label = "",
  #     choiceNames = choiceNames_rm[c(2,1,3)],
  #     choiceValues = choiceValues_rm[c(2,1,3)],
  #     inline = T,
  #     selected = character(0)
  #   ))
  # } else {
    
    selected <- isolate(if(is.null(input$rm_prompts_hp)) character(0) else {
      input$rm_prompts_hp
    })
    
    radioButtons(
      "rm_prompts_hp",
      label = "",
      choiceNames = choiceNames_rm[c(2,1,3)],
      choiceValues = choiceValues_rm[c(2,1,3)],
      inline = T,
      selected = selected
    )
  # }
  
})

observeEvent(input$rm_prompts_train, {
  
  req(input$rm_prompts_train == "train")
  
  
  ## Some eval function
  # if(is.null(omicsData$objPP$f_data)){
  #   samples <- min(get_group_table(omicsData$objPP))
  # } else {
    samples <- ncol(omicsData$objPP$e_data[-1])
  # }
  
  min_model_train <- 10
  min_train_holdout <- 4
  
  if((min_model_train + min_train_holdout) > samples){ ## at a minimal, loocv should work here
    disable(selector = "#rm_prompts_hp button:eq(2)")
  }
  
})


