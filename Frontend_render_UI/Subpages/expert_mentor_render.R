

tbl <- function(data, index, namecol)  {
  
  temp <- list()
  index <- deparse(substitute(index))
  namecol <- deparse(substitute(namecol))
  
  # Convert to Character
  col_names <- names(data)
  data[,col_names] <- lapply(data[,col_names] , as.character)
 
   # Loop over Rows
  for(i in 1:nrow(data)) {
    
    col0 = data[i,index]
    
    first2cols <-  list(
      
      tags$td(
        
        tags$div(
          style= "height: 75px;",
        class = "d-flex mt-2 border-right",
        tags$div(
          style= "height: 75px;",
          class = "box p-2 rounded",
          tags$span(
            class = "text-primary px-2 font-weight-bold",
            col0
          )
        )
      )
      )
    )
    
    # Loop over Columns (ignoring index, name and icon columns)
    temp.col <- list()
    data2 <- data[!(names(data) %in% c(index, namecol))]
    for(j in 1:ncol(data2)) {
      temp.col[[j]] <-
        tags$td(
          
          tags$div(
            style= "height: 75px;",
          class = "d-flex flex-column",
          
          tags$div(
            style= "height: 75px;",
            class = "text-muted",
            style= "height: 75px;",
            if(i == 1) colnames(data2)[j] else NULL
          ),
          
          if(!is.na(as.logical(data2[i,j]))){
            
            if(as.logical(data2[i,j])){
              tags$div(br(),
                       HTML('<i class="fa-solid fa-check fa-2xl" style="color: #4ebb30; height: 75px;"></i>')
                       )
              
            } else {
              tags$div(br(),
                       HTML('<i class="fa-solid fa-x fa-2xl" style="color: #c02216; height: 75px;"></i>'))
            }
          } else {
            tags$div(br(), tags$b(data2[i,j]))
          }
        ))
      
  }
    temp[[i]] <- tags$tr(first2cols, temp.col)
  
  }

  temp <- map(temp, as.character)
  
  for(i in colnames(data)){
    temp[[1]] <-  gsub("height: 75px",
                       "height: 120px",
                       temp[[1]]
    )
  }
  
  temp <- map(temp, HTML)
  
  tags$div(
    class = "bg-white table-responsive",
    tags$table(
      class = "table",
      tags$tbody(
        temp
      ))
  )
  
}

output$EM_dashboard <- renderUI({
  
  suggests <- expert_mentor(omicsData$objModel)
  df <- summary(suggests)
  
  missingness <- missingval_result(omicsData$objModel)$na.by.sample
  total <- sum(missingness$num_NA) + sum(missingness$num_non_NA)
  
  df <- df[df$supervised,] ## supervised/unsupervised
  df <- df[df$n_levels,] ## Correct number of levels for analysis
  df <- df[-2]
  df <- df[-6]
  
  colnames(df) <- c(
    "Method",
    paste0("Runs with ", ncol(omicsData$objModel$e_data) - 1, " samples?"),
    paste0("Runs with ", length(get_group_table(omicsData$objModel)), " classifications?"),
    paste0("Runs with ", nrow(omicsData$objModel$e_data), " predictors?"),
    paste0("Runs with a minimum group size of ", min(get_group_table(omicsData$objModel)), "?"),
    paste0("Runs with a sample/predictor ratio of ", 
           ncol(omicsData$objModel$e_data) - 1, ":", 
           nrow(omicsData$objModel$e_data), "?"),
    paste0("Runs with ", sum(missingness$num_non_NA), "/", total, 
           " (", signif(sum(missingness$num_non_NA)/total*100, 3), "%) of possible datapoints observed?" ),
    "Results explain which predictors contribute most?",
    "Method selects which predictors to use?",
    "Results contain an equation for the prediction?"
  )
  
  df$Score <- rowSums(df[-1])
  
  df_order <- c(
    "Score",
    "Method",
    paste0("Runs with ", ncol(omicsData$objModel$e_data) - 1, " samples?"), ## Automatic filter?
    paste0("Runs with ", length(get_group_table(omicsData$objModel)), " classifications?"),  ## Automatic filter?
    paste0("Runs with ", nrow(omicsData$objModel$e_data), " predictors?"),  ## Automatic filter?
    paste0("Runs with a minimum group size of ", min(get_group_table(omicsData$objModel)), "?"),  ## Automatic filter?
    paste0("Runs with a sample/predictor ratio of ", 
           ncol(omicsData$objModel$e_data) - 1, ":", 
           nrow(omicsData$objModel$e_data), "?"),  ## Automatic filter?
    paste0("Runs with ", sum(missingness$num_non_NA), "/", total, 
           " (", signif(sum(missingness$num_non_NA)/total*100, 3), "%) of possible datapoints observed?" ),
    "Method selects which predictors to use?",
    "Results explain which predictors contribute most?",
    "Results contain an equation for the prediction?"
  )
  
  df <- as.data.frame(df)[df_order]
  
  if(!input$equal_sort){
    
  order_input <- input$soft_sort
    
   order_cols <- df_order[3:length(df_order)]
   names(order_cols) <- c(
      "N samples",
      "N classifications",
      "N predictors",
      "Minimum group size",
      "Sample/predictor ratio",
      "Missingness handling",
      "Predictor selection",
      "Explainability",
      "Equation"
    )
   
   
   df <- arrange(df, across(starts_with(as.character(order_cols[order_input])), desc))
    
  } else {
    df <- arrange(df, desc(Score))
  }
  
  df$Method <- map_chr(df$Method, function(x) names(models_long_name)[which(models_long_name == x)])
  
    ## Filts
    filts <- c(
      "Feature selection" = "Method selects which predictors to use?",
      "Predictor weightings" = "Results explain which predictors contribute most?", 
      "Generates equation" = "Results contain an equation for the prediction?"
      )
    
    for(filt in names(filts)) if(input[[filt]]) df <- df[df[[filts[[filt]]]],]
    
    
    if(isTruthy(input$skip_ag)){
      df <- df[df$Method == input$pick_model, ]
    }
    
  if(nrow(df) < 5){
    df$Score <- as.character(df$Score)
    df[(nrow(df) + 1):5,] <- ""
  }
  
  fluidPage(
    
    br(),
    
    fluidRow(
      
      tags$div(
        class = "container mt-5",
        tags$div(
          class = "d-lg-flex align-items-lg-center py-4",
          tags$div(
            class = "h3 text-muted",
            "Recommended Models"
          )
        ),
        div(
          id = "top",
          tbl(as.data.frame(df), "Method", NULL),
          style = 'height:500px; overflow-y: scroll; overflow-x: scroll',
        )
      ))
  )
  
  
})

output$pick_EM_model_UI <- renderUI({
  
  
  ## until summary get fixed
  if(input$ag_prompts != "supervised"){
    choices <- models_long_name[models_long_name %in% c("kmeans", "pca", "umap", "hclust")]
  } else {
    choices <- models_long_name[!(models_long_name %in% c("kmeans", "pca", "umap", "hclust"))]
  }
  
  selected <- isolate(input$pick_model_EM)
  
  ## until summary get fixed
  pickerInput("pick_model_EM", label = "Select a model:",
              choices = choices, selected = selected
              )
  
  # suggests <- expert_mentor(omicsData$objModel, 
  #                           supervised = input$input$ag_prompts == "supervised")
  # df <- summary(suggests)
  # 
  # missingness <- missingval_result(omicsData$objModel)$na.by.sample
  # total <- sum(missingness$num_NA) + sum(missingness$num_non_NA)
  # 
  # if(input$input$ag_prompts == "supervised"){
  #   df <- df[df$supervised,] ## supervised/unsupervised
  # } else {
  #   df <- df[!df$supervised,] ## supervised/unsupervised
  # }
  # 
  # df <- df[-2]
  # df <- df[-6]
  # 
  # colnames(df) <- c(
  #   "Method",
  #   paste0("Runs with ", ncol(omicsData$objModel$e_data) - 1, " samples?"),
  #   paste0("Runs with ", length(get_group_table(omicsData$objModel)), " classifications?"),
  #   paste0("Runs with ", nrow(omicsData$objModel$e_data), " predictors?"),
  #   paste0("Runs with a minimum group size of ", min(get_group_table(omicsData$objModel)), "?"),
  #   paste0("Runs with a sample/predictor ratio of ", 
  #          ncol(omicsData$objModel$e_data) - 1, ":", 
  #          nrow(omicsData$objModel$e_data), "?"),
  #   paste0("Runs with ", sum(missingness$num_non_NA), "/", total, 
  #          " (", signif(sum(missingness$num_non_NA)/total*100, 3), "%) of possible datapoints observed?" ),
  #   "Results explain which predictors contribute most?",
  #   "Method selects which predictors to use?",
  #   "Results contain an equation for the prediction?"
  # )
  # 
  # df$Score <- rowSums(df[-1])
  # 
  # df_order <- c(
  #   "Score",
  #   "Method",
  #   paste0("Runs with ", ncol(omicsData$objModel$e_data) - 1, " samples?"),
  #   paste0("Runs with ", length(get_group_table(omicsData$objModel)), " classifications?"),
  #   paste0("Runs with ", nrow(omicsData$objModel$e_data), " predictors?"),
  #   paste0("Runs with a minimum group size of ", min(get_group_table(omicsData$objModel)), "?"),
  #   paste0("Runs with a sample/predictor ratio of ", 
  #          ncol(omicsData$objModel$e_data) - 1, ":", 
  #          nrow(omicsData$objModel$e_data), "?"),
  #   paste0("Runs with ", sum(missingness$num_non_NA), "/", total, 
  #          " (", signif(sum(missingness$num_non_NA)/total*100, 3), "%) of possible datapoints observed?" ),
  #   "Method selects which predictors to use?",
  #   "Results explain which predictors contribute most?",
  #   "Results contain an equation for the prediction?"
  # )
  # 
  # df <- as.data.frame(df)[df_order]
  # df <- arrange(df, desc(Score))
  # 
  # df$Method <- map_chr(df$Method, function(x) names(models_long_name)[models_long_name == x])
  # 
  # ## Filts
  # filts <- c(
  #   "Feature selection" = "Method selects which predictors to use?",
  #   "Predictor weightings" = "Results explain which predictors contribute most?", 
  #   "Generates equation" = "Results contain an equation for the prediction?"
  # )
  # 
  # for(filt in names(filts)) if(input[[filt]]) df <- df[df[[filts[[filt]]]],]
  # 
  # if(isTruthy(input$skip_ag)){
  #   df <- df[df$Method == input$pick_model, ]
  # }
  # 
  # selected <- isolate(if(!is.null(input$pick_model_EM)) input$pick_model_EM else character(0))
  # 
  # pickerInput("pick_model_EM", label = "Select a model:",
  #             choices = df$Method,
  #             
  #             )
  
  
})

