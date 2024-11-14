
## Examples
rm_text <- c(
  HTML("Advanced: Use user-specified settings"),
  HTML("Guided: Use default settings supported in literature"),
  HTML("Guided: Optimize settings for best performance"),
  HTML("Being able to understand relationships within the data"),
  HTML("Making a model that would perform well on future data")
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
  if(!is.null(omicsData$objPP$f_data) && !is.null(get_group_table(omicsData$objPP))){
    nsamp <- min(get_group_table(omicsData$objPP))
  } else {
    nsamp <- ncol(omicsData$objPP$e_data[-1])
  }
  
  ## Some minimum model-dependent holdout
  method <- input$pick_model_EM
  
  ## Some reccomend function
  rec_func <- function(nsamp, method){
    
    rec_minimum <- switch(
      method,
      lsvm = 20,
      psvm = 20,
      rsvm = 20,
      multi = 20,
      multilasso = 20,
      logistic = 20,
      loglasso = 20,
      knn = 20,
      rf = 20,
      kmeans = 20, ## For cluster optimization, might have to snag group stufff
      lr = 20,
      lda = 20,
      qda = 20,
      hclust = 0,
      pls = 20,
      pca = 0,
      ppca = 0,
      umap = 0,
      gbtree = 20,
    )
    
    if(.3*nsamp < rec_minimum){
      return(FALSE)
    } else return (TRUE)
    
  }
  
  rec_func(nsamp, method)
  
})


choiceNames_rm <- pmap(list(rm_text, files_rm), 
                       function(text, p1){
                         
                         div(id="panerm",
                             wellPanel(
                               HTML(text),
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
  
  out <- if(!supervised()){
    
    selected <- isolate(if(is.null(input$rm_prompts_hp)) character(0) else {
      input$rm_prompts_hp
    })
    
    ## It'd be nice if this was able to disable rather than remove -- TO DO
    radioButtons(
      "rm_prompts_hp",
      label = "",
      choiceNames = choiceNames_rm[c(#3,
                                     1)],
      choiceValues = choiceValues_rm[c(#3,
                                       1)],
      inline = T,
      selected = selected
    )
    
    
  } else if(is.null(omicsData$objPP$f_data)){
    disabled(radioButtons(
      "rm_prompts_hp",
      label = "",
      choiceNames = choiceNames_rm[c(2:3,1)],
      choiceValues = choiceValues_rm[c(2:3,1)],
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
      
      slice <- if(input$user_level_pick == "beginner") 2 else c(2,1)
      
      ## It'd be nice if this was able to disable rather than remove -- TO DO
      radioButtons(
        "rm_prompts_hp",
        label = "",
        choiceNames = choiceNames_rm[slice],
        choiceValues = choiceValues_rm[slice],
        inline = T,
        selected = selected
      )
      
    } else {
      
      slice <- if(input$user_level_pick == "beginner") 2:3 else c(2:3,1)
      
      radioButtons(
        "rm_prompts_hp",
        label = "",
        choiceNames = choiceNames_rm[slice],
        choiceValues = choiceValues_rm[slice],
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
  
  req(!holdout_valid() && 
        input$rm_prompts_train == "train")
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


