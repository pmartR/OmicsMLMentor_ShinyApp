
## Examples
rm_text <- c(
  HTML("Advanced users only: use user-specified settings<br/><br/>This option should only be used if the user is familiar with the model or is trying to recreate the settings used in a previous SLOPE session."),
  HTML("Guided: Use default settings supported in literature<br/><br/>All settings will be pre-populated in the application for building the model."),
  HTML("Guided: Optimize settings for best performance<br/><br/>Optimal settings will be determined using internal functions with visability and guidance at each step."),
  HTML("Understand relationships within this dataset"),
  HTML("Apply model to new data")
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

holdout_valid <- reactive({
  
  ## Some eval function
  
  ## Check number of samples or number of samples in smallest group size
  if(!is.null(omicsData$objPP$f_data)){
    nsamp <- min(get_group_table(omicsData$objPP))
  } else {
    nsamp <- ncol(omicsData$objPP$e_data[-1])
  }
  
  ## Some minimum model-dependent holdout
  method <- input$pick_model_EM
  
  ## Some reccomend function
  reccomend_func <- function(nsamp, method){
    
    rec_minimum <- switch(
      method,
      lsvm = 5,
      psvm = 5,
      rsvm = 5,
      multi = 5,
      multilasso = 5,
      logistic = 5,
      loglasso = 5,
      rf = 5,
      kmeans = 5, ## For cluster optimization, might have to snag group stufff
      hclust = 0,
      pca = 0,
      umap = 0,
      gbtree = 5,
    )
    
    if(.3*nsamp < rec_minimum){
      return(FALSE)
    } else return (TRUE)
    
  }
  
  reccomend_func(nsamp, method)
  
})


choiceNames_rm <- pmap(list(rm_text, files_rm), 
                       function(text, p1){
                         
                         div(id="panerm",
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
                             ), tags$style(type="text/css","#panerm{font-size:14px;width:405px;}")
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
  
  out <- if(is.null(omicsData$objPP$f_data)){
    disabled(radioButtons(
      "rm_prompts_hp",
      label = "",
      choiceNames = choiceNames_rm[c(2,1,3)],
      choiceValues = choiceValues_rm[c(2,1,3)],
      inline = T,
      selected = character(0)
    ))
  } else {
    
    selected <- isolate(if(is.null(input$rm_prompts_hp)) character(0) else {
      input$rm_prompts_hp
    })
    
    if(!holdout_valid() && 
       !is.null(input$rm_prompts_train) && 
       input$rm_prompts_train == choiceValues_rm[5]){
      
      ## It'd be nice if this was able to disable rather than remove -- TO DO
      radioButtons(
        "rm_prompts_hp",
        label = "",
        choiceNames = choiceNames_rm[c(2,1)],
        choiceValues = choiceValues_rm[c(2,1)],
        inline = T,
        selected = selected
      )
      
    } else {
      radioButtons(
        "rm_prompts_hp",
        label = "",
        choiceNames = choiceNames_rm[c(2,1,3)],
        choiceValues = choiceValues_rm[c(2,1,3)],
        inline = T,
        selected = selected
      )
    }
    
  }
  
})

# output$warn_few_samps_train <- renderText({
#   req(!holdout_valid() && input$rm_prompts_train == "train")
# })

output$warn_few_samps_settings <- renderText({
  req(!holdout_valid() && input$rm_prompts_train == "train")
  paste0("Warning: The number of samples in your dataset is too small ",
         "to estimate robust measurements across new datasets and utilize ",
         "model optimizations. Model optimization option has been removed.")
})



## Disable attempt 1 :( no worky so far
# observeEvent(input$rm_prompts_train, {
#   
#   req(input$rm_prompts_train == "train")
#   
#   
#   ## Some eval function
#   # if(is.null(omicsData$objPP$f_data)){
#   #   samples <- min(get_group_table(omicsData$objPP))
#   # } else {
#     samples <- ncol(omicsData$objPP$e_data[-1])
#   # }
#   
#   min_model_train <- 10
#   min_train_holdout <- 4
#   
#   if((min_model_train + min_train_holdout) > samples){ ## at a minimal, loocv should work here
#     disable(selector = "#rm_prompts_hp button:eq(2)")
#   }
#   
# })


