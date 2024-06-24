
## For some ranking automation
automated_model_text <- reactiveVal()
dashboard <- reactiveVal()

tbl <- function(data, index, namecol)  {
  
  temp <- list()
  index <- deparse(substitute(index))
  namecol <- deparse(substitute(namecol))
  
  # # Convert to Character
  # col_names <- names(data)
  # data[,col_names] <- lapply(data[,col_names] , as.character)
  # 
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

## Make dashboard -- lots of elements to consider, so we use observe here
observe({
  req(
    !any(map_lgl(
      list(
        input$skip_ag,
        # omicsData$objMSU,
        omicsData$objModel,
        input$feature_selection
      ),
      is.null
    )), 
    input$top_page == "Model Set-Up"
  )
  temp_omic <- omicsData$objModel
  
  if(get_data_scale(temp_omic) == "abundance" && 
     !inherits(temp_omic, "seqData")){
    temp_omic <- edata_transform(temp_omic, "log2")
  }
  
  supervised <- supervised()
  
  handles_regression <- if('continuous' %in% response_types_ag()) {
    TRUE
  } else {
    FALSE
  }
  
  if(is.null(omicsData$objQC$f_data) && supervised){
    temp_omic$f_data <- data.frame(
      SampleID = colnames(temp_omic$e_data)[
        colnames(temp_omic$e_data) != pmartR::get_edata_cname(temp_omic)],
      Temp_col_all = "All"
    )
    temp_omic <- group_designation(temp_omic, "Temp_col_all")
  } else if(supervised){
    temp_omic$f_data$Temp_col_all <- "All"
    temp_omic <- group_designation(temp_omic, "Temp_col_all")
  }
  
  id_col <- which(colnames(temp_omic$e_data) == 
                    get_edata_cname(temp_omic))
  
  samples_per_feature <- nrow(temp_omic$e_data)/
    min(get_group_table(temp_omic)) > 300
  
  correlation <- any(cor(t(temp_omic$e_data[-id_col])) > .90)
  
  ## Change based on algorithim for holdout
  overfit <- min(get_group_table(temp_omic)) < 5
  
  if (supervised) {
    rmd <- any(rmd_filter(temp_omic)$pvalue < 0.0001)
  } else if(inherits(temp_omic, "seqData")){
    rmd <- F
  } else {
    rmd <- T
  }
  
  if(input$user_level_pick == "beginner"){
    
    suggests <- expert_mentor(temp_omic,
                              supervised = supervised,
                              handles_regression = handles_regression,
                              feature_selection = input$feature_selection,
                              handles_missingness = input$handles_missingness,
                              explainability = input$explainability,
                              equation = input$equation,
                              
                              ## Autodetect
                              high_dimensional_data = input$high_dimensional_data,
                              samples_per_feature = samples_per_feature,
                              correlation = correlation,
                              prone_to_overfit = overfit,
                              handles_outliers = rmd
    )
    
  } else if (input$user_level_pick != "expert"){
    
    ## Auto detect for some of these
    suggests <- expert_mentor(temp_omic,
                              supervised = supervised,
                              feature_selection = input$feature_selection,
                              handles_missingness = input$handles_missingness,
                              explainability = input$explainability,
                              equation = input$equation,
                              handles_regression = handles_regression,
                              ## Autodetect
                              high_dimensional_data = input$high_dimensional_data,
                              samples_per_feature = samples_per_feature,
                              correlation = correlation,
                              prone_to_overfit = overfit,
                              handles_outliers = rmd
    )
    
    if(any(c(samples_per_feature, correlation, overfit, rmd))){
      
      conds <- c(
        "Based on your data, the rankings for each model also consider:",
        if(samples_per_feature) "Handling a large feature:sample ratio" else NULL,
        if(correlation) "Managing highly correlated features" else NULL,
        if(overfit) "Innately avoiding overfitting" else NULL,
        if(rmd) "Robustly managing possible outliers" else NULL)
      
      automated_model_text(HTML(paste0(conds, collapse = "\n")))
      
    } else automated_model_text("NULL")
    
  } else {
    
    suggests <- expert_mentor(temp_omic,
                              supervised = supervised,
                              feature_selection = input$feature_selection,
                              samples_per_feature = input$samples_per_feature,
                              explainability = input$explainability,
                              equation = input$equation,
                              correlation = input$correlation,
                              prone_to_overfit = input$prone_to_overfit,
                              handles_missingness = input$handles_missingness,
                              high_dimensional_data = input$high_dimensional_data,
                              handles_outliers = input$handles_outliers,
                              handles_regression = handles_regression
    )
    
  }
  
  df <- summary(suggests)
  
  decisions_soft <- attributes(suggests)$soft_rules_filter %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(method = names(slopeR::algo_rules)) %>%
    dplyr::select(method, dplyr::everything())
  
  decisions_hard <- attributes(suggests)$hard_rules_filter %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(method = names(slopeR::algo_rules)) %>%
    dplyr::select(method, dplyr::everything())
  
  df <- df[df$supervised,] ## supervised/unsupervised
  df <- df[df$n_levels,] ## Correct number of levels for analysis
  df$supervised <- unlist(suggests)
  df <- df %>% dplyr::select(-dplyr::one_of("any_is_na"))
  
  df$n_predictors_per_sample <- signif(df$n_predictors_per_sample, 3)
  df$prop_missing <- signif(df$prop_missing, 3)
  
  if (is.null(omicsData$objModel$f_data)) {
    missingness <- list(
      num_NA = sum(is.na(omicsData$objQC$e_data)),
      num_non_NA = sum(!is.na(omicsData$objQC$e_data))
    )
  } else {
    missingness <- missingval_result(omicsData$objModel)$na.by.sample
  }
  total <- sum(missingness$num_NA) + sum(missingness$num_non_NA)
  
  group_text <- ifelse(!is.null(get_group_table(omicsData$objModel)), 
                       min(get_group_table(omicsData$objModel)), 1)
  
  colnames(df) <- c(
    "Method",
    "Score",
    paste0("Runs with ", ncol(omicsData$objModel$e_data) - 1, " samples?"),
    paste0("Runs with ", length(get_group_table(omicsData$objModel)), " classifications?"),
    paste0("Runs with ", nrow(omicsData$objModel$e_data), " predictors?"),
    paste0("Runs with a minimum group size of ", group_text, "?"),
    "Can handle a continuous response?",
    paste0("Performance with a sample/predictor ratio of ", 
           ncol(omicsData$objModel$e_data) - 1, ":", 
           nrow(omicsData$objModel$e_data), "?"),
    paste0("Performance with ", sum(missingness$num_non_NA), "/", total, 
           " (", signif(sum(missingness$num_non_NA)/total*100, 3), "%) of possible datapoints observed?" ),
    "Relative interpretability of results?",
    "Method keeps relatively best predictors to use?",
    "Results contain an equation for the prediction?",
    "Model innately avoids overfitting?",
    "Model handles highly correlated features?",
    "High dimensional?",
    "Model handles outliers robustly?"
  )
  
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
  
  df$Method <- names(models_long_name)[match(df$Method, models_long_name)]
  df <- df[!is.na(df$Method),] ## knn is implemented in summary, but not in sup_models yet lol
  ## Filts
  # filts <- c(
  #   "Feature selection" = "Method selects which predictors to use?",
  #   "Predictor weightings" = "Results explain which predictors contribute most?", 
  #   "Generates equation" = "Results contain an equation for the prediction?"
  #   )
  
  # for(filt in names(filts)) if(input[[filt]]) df <- df[df[[filts[[filt]]]],]
  
  
  table_table_current$table$MSU__expert_mentor_summary <- df
  
  if(isTruthy(input$skip_ag)){
    picker <- names(models_long_name)[models_long_name == input$pick_model]
    df <- df[df$Method == picker, ]
  } else if(input$user_level_pick == "beginner"){
    df <- df[1:3,]
  } else if (input$user_level_pick == "familiar"){
    df <- df[1:min(c(nrow(df), 10)),]
  }
  
  # Convert to Character
  col_names <- names(df)
  df[,col_names] <- lapply(df[,col_names] , as.character)
  
  if(nrow(df) < 5){
    df[(nrow(df) + 1):5,] <- ""
  }

  isolate(dashboard(df))
  
})


output$EM_dashboard <- renderUI({
  
  text <- if(input$user_level_pick == "beginner"){
      " - Top 3 shown"
    } else if (input$user_level_pick == "familiar"){
      " - Top 10 shown"
    } else ""
  
  # fluidPage(
  #   
  #   br(),
    
    fluidRow(
      
      column(12,
      tags$div(
        class = "container mt-5",
        tags$div(
          class = "d-lg-flex align-items-lg-center py-4",
          tags$div(
            class = "h3 text-muted",
            paste("Recommended Models", text)
          )
        ),
        div(
          id = "top",
          tbl(as.data.frame(dashboard()), "Method", NULL),
          style = 'height:600px; overflow-y: scroll; overflow-x: scroll; width = 90%',
        )
      )))
  # )
  
  
})

output$pick_EM_model_UI <- renderUI({
  
  selected <- isolate(input$pick_model_EM)
  
  choices <- models_long_name[dashboard()$Method]

  pickerInput("pick_model_EM", label = "Select a model:",
              choices = choices[!is.na(choices)], 
              selected = selected
              )
  
})

