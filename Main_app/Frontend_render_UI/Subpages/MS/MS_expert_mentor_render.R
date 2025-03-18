
## For some ranking automation
automated_model_text <- reactiveVal()
dashboard <- reactiveVal()

correlation <- FALSE

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
            if(i == 1) HTML(colnames(data2)[j]) else NULL
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
    style = "height: 100%",
    class = "bg-white table-responsive",
    tags$table(
      class = "table",
      tags$tbody(
        temp
      ))
  )
  
}

## Make dashboard -- lots of elements to consider, so we use observe here
observeEvent(
  c(
    input$skip_ag,
    omicsData$objModel,
    input$feature_selection,
    input$handles_missingness,
    input$explainability,
    input$high_dimensional_data,
    input$equation,
    input$correlation,
    input$handles_outliers,
    input$samples_per_feature,
    input$prone_to_overfit
  ), ignoreInit = TRUE, {
  req(
    !any(map_lgl(
      list(
        input$skip_ag,
        # omicsData$objMSU,
        omicsData$objModel,
        input$feature_selection,
        input$handles_missingness,
        input$explainability,
        input$high_dimensional_data,
        input$equation,
        input$correlation,
        input$handles_outliers,
        input$samples_per_feature,
        input$prone_to_overfit
      ),
      is.null
    )), 
    input$top_page == "Model Set-Up"
  )
    
  shiny::showNotification(
    "Calculating optimal models for your data, please wait...",
    duration = NULL,
    id = "expert_mentor_note"
  )
  
  on.exit({
    shiny::removeNotification(id = "expert_mentor_note")
  })
    
  temp_omic <- omicsData$objModel
  
  if(get_data_scale(temp_omic) == "abundance" && 
     !inherits(temp_omic, "seqData")){
    temp_omic <- edata_transform(temp_omic, "log2")
  }
  
  supervised <- supervised()

  req(!is.null(supervised) && !is.na(supervised) &&
        ((!supervised && is.null(attr(temp_omic, "response_info"))) || 
           (supervised && !is.null(attr(temp_omic, "response_info"))))
        )
  
  handles_regression <- if('continuous' %in% response_types_ag()) {
    TRUE
  } else {
    FALSE
  }
  
  if(is.null(omicsData$objQC$f_data)){ ## Unsupervised
    temp_omic$f_data <- data.frame(
      SampleID = colnames(temp_omic$e_data)[
        colnames(temp_omic$e_data) != pmartR::get_edata_cname(temp_omic)],
      Temp_col_all = "All"
    )
    temp_omic <- group_designation(temp_omic, "Temp_col_all")
  } else if(supervised && response_types_ag() == "continuous"){ ## Continuous
    temp_omic$f_data$Temp_col_all <- "All"
    temp_omic <- group_designation(temp_omic, "Temp_col_all")
  }
  
  id_col <- which(colnames(temp_omic$e_data) == 
                    get_edata_cname(temp_omic))
  
  samples_per_feature <- nrow(temp_omic$e_data)/
    min(get_group_table(temp_omic)) > 300
  
  # Avoid out of memory crash with large data object
  if (dim(temp_omic$e_data)[1] > 20000) {
    correlation <<- TRUE
  } else {
    cor_use <- cor(t(temp_omic$e_data[-id_col]))
    diag(cor_use) <- NA
    correlation <<- any(cor_use > .90)
    if(is.na(correlation)) correlation <<- FALSE
  }
  
  ## Change based on algorithim for holdout
  overfit <- min(get_group_table(temp_omic)) < 5
  
  if (inherits(temp_omic, "seqData")) {
    rmd <- FALSE
  }  else if (min(get_group_table(temp_omic)) < 4) {
    
    ## Can't compute rmd with small sample sizes, 4 is cut-off for default rmd metrics as used in expert mentor
    rmd <- FALSE
    
  } else if (supervised) {
    rmd <- any(rmd_filter(temp_omic)$pvalue < 0.0001)
  } else {
    rmd <- TRUE
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
  
  df <- df[df$naive_bayes_cutoff,] ## nb cutoff check

  if('continuous' %in% response_types_ag()){
    df <- df[df$regression,]
    scores <- map(attr(suggests, "soft_rules"), function(x) sum(unlist(x)))
    df$supervised <- round(as.numeric(scores[df$method]), 2) ## This essentially assigns this to a score, which is manipulated later
  } else {
    df <- df[df$n_levels,] ## Correct number of levels for analysis
    df$supervised <- unlist(suggests) ## This essentially assigns this to a score, which is manipulated later
  }
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
    paste0("Runs with ", length(get_group_table(omicsData$objModel)), " classifications?"),
    #paste0("Runs with ", nrow(omicsData$objModel$e_data), " predictors?"),
    #paste0("Runs with a minimum group size of ", group_text, "?"),
    "Can handle a continuous response?",
    "Can handle few samples?",
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
    "Model handles outliers robustly?",
    "NB_CUTOFF"
  )
  
  df <- df[-which(colnames(df) == "NB_CUTOFF"),]
  
  #if(!input$equal_sort){
  #  
  #  order_input <- input$soft_sort
  #  
  #  order_cols <- df[3:length(df)]
  #  names(order_cols) <- c(
  #    "N samples",
  #    "N classifications",
  #    "Continuous response",
  #    "Sample/predictor ratio",
  #    "Missingness handling",
  #    "Explainability",
  #    "Predictor selection",
  #    "Equation",
  #    "Overfitting",
  #    "High correlation",
  #    "High dimensionality",
  #    "Outlier handling"
  #  )
  #  
  #  df <- arrange(df, across(starts_with(as.character(order_cols[order_input])), desc))
  #  
  #} else {
    df <- arrange(df, desc(Score))
  #}
  
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
  
  colnames(df) <- c(
    "Method",
    "Score",
    paste0(
      "Total Classifications ", 
      div(
        id = "em_info_total_classifications",
        class = "hint--bottom",
        icon(
          name = "circle-info",
          class = "em-col-info-btn",
          onmouseover = "Shiny.setInputValue(id = 'em_column_hover', value = 'total_classifications')",
          onclick = "Shiny.setInputValue(id = 'em_column_info', value = {'name': 'total_classifications', 'nonce': Math.random() })"
        )
      )
    ), # "Runs with ", length(get_group_table(omicsData$objModel)), " classifications?"
    # paste0(
    #   "Total Predictors ", 
    #   div(
    #     id = "em_info_total_predictors",
    #     class = "hint--bottom",
    #     icon(
    #       name = "circle-info",
    #       class = "em-col-info-btn",
    #       onmouseover = "Shiny.setInputValue(id = 'em_column_hover', value = 'total_predictors')",
    #       onclick = "Shiny.setInputValue(id = 'em_column_info', value = {'name': 'total_predictors', 'nonce': Math.random() })"
    #     )
    #   )
    # ), # "Runs with ", nrow(omicsData$objModel$e_data), " predictors?"
    # paste0(
    #   "Min Group Size ", 
    #   div(
    #     id = "em_info_min_group_size",
    #     class = "hint--bottom",
    #     icon(
    #       name = "circle-info",
    #       class = "em-col-info-btn",
    #       onmouseover = "Shiny.setInputValue(id = 'em_column_hover', value = 'min_group_size')",
    #       onclick = "Shiny.setInputValue(id = 'em_column_info', value = {'name': 'min_group_size', 'nonce': Math.random() })"
    #     )
    #   )
    # ), # "Runs with a minimum group size of ", group_text, "?"
    paste0(
      "Continuous Response ", 
      div(
        id = "em_info_continuous_response",
        class = "hint--bottom",
        icon(
          name = "circle-info",
          class = "em-col-info-btn",
          onmouseover = "Shiny.setInputValue(id = 'em_column_hover', value = 'continuous_response')",
          onclick = "Shiny.setInputValue(id = 'em_column_info', value = {'name': 'continuous_response', 'nonce': Math.random() })"
        )
      )
    ), # "Can handle a continuous response?"
    paste0(
      "Handles Few Samples ", 
      div(
        id = "em_info_few_samples",
        class = "hint--bottom",
        icon(
          name = "circle-info",
          class = "em-col-info-btn",
          onmouseover = "Shiny.setInputValue(id = 'em_column_hover', value = 'few_samples')",
          onclick = "Shiny.setInputValue(id = 'em_column_info', value = {'name': 'few_samples', 'nonce': Math.random() })"
        )
      )
    ), # "Runs with ", ncol(omicsData$objModel$e_data) - 1, " samples?"
    
    paste0(
      "Sample/Predictor Ratio Performance ", 
      div(
        id = "em_info_sample_predictor_ratio",
        class = "hint--bottom",
        icon(
          name = "circle-info",
          class = "em-col-info-btn",
          onmouseover = "Shiny.setInputValue(id = 'em_column_hover', value = 'sample_predictor_ratio')",
          onclick = "Shiny.setInputValue(id = 'em_column_info', value = {'name': 'sample_predictor_ratio', 'nonce': Math.random() })"
        )
      )
    ), # "Performance with a sample/predictor ratio of ", ncol(omicsData$objModel$e_data) - 1, ":", nrow(omicsData$objModel$e_data), "?"
    paste0(
      "Proportion Missing Performance ", 
      div(
        id = "em_info_prop_missing",
        class = "hint--bottom",
        icon(
          name = "circle-info",
          class = "em-col-info-btn",
          onmouseover = "Shiny.setInputValue(id = 'em_column_hover', value = 'prop_missing')",
          onclick = "Shiny.setInputValue(id = 'em_column_info', value = {'name': 'prop_missing', 'nonce': Math.random() })"
        )
      )
    ), # "Performance with ", sum(missingness$num_non_NA), "/", total, " (", signif(sum(missingness$num_non_NA)/total*100, 3), "%) of possible datapoints observed?"
    paste0(
      "Explainability ", 
      div(
        id = "em_info_explainability",
        class = "hint--bottom",
        icon(
          name = "circle-info",
          class = "em-col-info-btn",
          onmouseover = "Shiny.setInputValue(id = 'em_column_hover', value = 'explainability')",
          onclick = "Shiny.setInputValue(id = 'em_column_info', value = {'name': 'explainability', 'nonce': Math.random() })"
        )
      )
    ), # "Relative interpretability of results? "
    paste0(
      "Keeps Best Predictors ", 
      div(
        id = "em_info_best_predictors",
        class = "hint--bottom",
        icon(
          name = "circle-info",
          class = "em-col-info-btn",
          onmouseover = "Shiny.setInputValue(id = 'em_column_hover', value = 'best_predictors')",
          onclick = "Shiny.setInputValue(id = 'em_column_info', value = {'name': 'best_predictors', 'nonce': Math.random() })"
        )
      )
    ), # "Method keeps relatively best predictors to use? "
    paste0(
      "Generates Equation ", 
      div(
        id = "em_info_equation",
        class = "hint--bottom",
        icon(
          name = "circle-info",
          class = "em-col-info-btn",
          onmouseover = "Shiny.setInputValue(id = 'em_column_hover', value = 'equation')",
          onclick = "Shiny.setInputValue(id = 'em_column_info', value = {'name': 'equation', 'nonce': Math.random() })"
        )
      )
    ), # "Results contain an equation for the prediction? "
    paste0(
      "Avoids Overfitting", 
      div(
        id = "em_info_avoids_overfitting",
        class = "hint--bottom",
        icon(
          name = "circle-info",
          class = "em-col-info-btn",
          onmouseover = "Shiny.setInputValue(id = 'em_column_hover', value = 'avoids_overfitting')",
          onclick = "Shiny.setInputValue(id = 'em_column_info', value = {'name': 'avoids_overfitting', 'nonce': Math.random() })"
        )
      )
    ), # "Model innately avoids overfitting? "
    paste0(
      "Handles High Correlation ", 
      div(
        id = "em_info_high_correlation",
        class = "hint--bottom",
        icon(
          name = "circle-info",
          class = "em-col-info-btn",
          onmouseover = "Shiny.setInputValue(id = 'em_column_hover', value = 'high_correlation')",
          onclick = "Shiny.setInputValue(id = 'em_column_info', value = {'name': 'high_correlation', 'nonce': Math.random() })"
        )
      )
    ), # "Model handles highly correlated features?"
    paste0(
      "High Dimensional ", 
      div(
        id = "em_info_high_dimensional",
        class = "hint--bottom",
        icon(
          name = "circle-info",
          class = "em-col-info-btn",
          onmouseover = "Shiny.setInputValue(id = 'em_column_hover', value  = 'high_dimensional')",
          onclick = "Shiny.setInputValue(id = 'em_column_info', value = {'name': 'high_dimensional', 'nonce': Math.random() }')"
        )
      )
    ), # "High dimensional? "
    paste0(
      "Handles Outliers ", 
      div(
        id = "em_info_handles_outliers",
        class = "hint--bottom",
        icon(
          name = "circle-info",
          class = "em-col-info-btn",
          onmouseover = "Shiny.setInputValue(id = 'em_column_hover', value = 'handles_outliers')",
          onclick = "Shiny.setInputValue(id = 'em_column_info', value = {'name': 'handles_outliers', 'nonce': Math.random() })"
        )
      )
    ) # "Model handles outliers robustly? "
  )
  
  ## Add Rank column

  df$Rank <- 1:nrow(df)
  df <- df[c(ncol(df), 1:(ncol(df) - 1))]
  
  ## Remove classification and regression cols (auto-filtered)
  df <- df[-4]
  df <- df[-4]
  
  # Remove NB cutoff col
  df <- df[-14]
  
  # else if(input$user_level_pick == "beginner"){
  #  df <- df[1:4,]
  #} else if (input$user_level_pick == "familiar"){
  #  df <- df[1:min(c(nrow(df), 10)),]
  #}
  
  # Convert to Character
  col_names <- names(df)
  df[,col_names] <- lapply(df[,col_names] , as.character)
  
  if(nrow(df) < 5){
    df[(nrow(df) + 1):5,] <- ""
  }
  
  types_dashboard <- df$Method
  df <- df[types_dashboard %in% names(models_long_name),]

  isolate(dashboard(df))
  
})

# bibs <- RefManageR::ReadBib("www/references.bib")
# 
# get_citations_html <- function(level, column) {
#   # Get dataframe with all info
#   df <- as.data.frame(do.call(rbind, sapply(slopeR::algo_rules, \(x) x[[level]])))
#   
#   # Subset to specific level and column and extract citation portion of the list
#   df2 <- sapply(df[[column]][which(sapply(df[[column]], length) == 3)], \(x) x[[3]])
#   
#   # Get citations for models which have them
#   citations <- sapply(df2[which(sapply(df2, length) > 0)], paste)
#   
#   # Get full names
#   names(citations) <- sapply(names(citations), \(x) slopeR::algo_rules[[x]]$full_name)
#   
#   # Get full citations
#   citations <- sapply(citations, \(x) sapply(x, \(y) utils:::format.bibentry(bibs[[y]])))
#   
#   # Apply formatting
#   citations <- sapply(citations, \(x) gsub("\n", " ", x))
#   citations <- sapply(citations, \(x) gsub("<", "&lt;", x))
#   citations <- sapply(citations, \(x) gsub(">", "&gt;", x))
#   citations <- sapply(citations, \(x) gsub("\\textbar", "|", x))
#   citations <- sapply(citations, \(x) paste(unname(x), collapse = "<br><br>"))
#   
#   if (length(citations) == 0) {
#     return("No citations available.")
#   }
#   
#   # Get a single HTML source
#   html <- paste(sapply(1:length(citations), \(x) paste0("<br><b>", names(citations)[x], ":</b><br>", citations[[x]])), collapse = "<br>")
#   
#   return(HTML(html))
# }
#
# saveRDS(list(
#   "total_samples" = get_citations_html("hard", "n_samps"),
#   "total_classifications" = get_citations_html("hard", "n_levels"),
#   "continuous_response" = get_citations_html("soft", "continuous_response"),
#   "sample_predictor_ratio" = get_citations_html("soft", "n_predictors_per_sample"),
#   "prop_missing" = get_citations_html("soft", "prop_missing"),
#   "best_predictors" = get_citations_html("soft", "feature_selection"),
#   "explainability" = get_citations_html("soft", "explainability"),
#   "equation" = get_citations_html("soft", "equation"),
#   "avoids_overfitting" = get_citations_html("soft", "prone_to_overfit"),
#   "high_correlation" = get_citations_html("soft", "correlation"),
#   "high_dimensional" = get_citations_html("soft", "high_dimensional"),
#   "handles_outliers" = get_citations_html("soft", "outlier_sensitivity")
# ), "citations.RDS")

# uncomment above and run to regenerate citations

citations <- readRDS("./citations.RDS")

get_em_column_info <- function() {
  missingness <- missingval_result(omicsData$objModel)$na.by.sample
  total <- sum(missingness$num_NA) + sum(missingness$num_non_NA)
  
  group_text <- ifelse(!is.null(get_group_table(omicsData$objModel)), 
                       min(get_group_table(omicsData$objModel)), 1)
  
  temp_omic <- omicsData$objModel
  
  if(get_data_scale(temp_omic) == "abundance"){
    temp_omic <- edata_transform(temp_omic, "log2")
  }
  
  id_col <- which(colnames(temp_omic$e_data) == 
                    get_edata_cname(temp_omic))
  
  titles <- list(
    "total_samples" = paste0("Total Samples: ", ncol(omicsData$objModel$e_data) - 1),
    "few_samples" = paste0("Total Samples: ", ncol(omicsData$objModel$e_data) - 1),
    "total_classifications" = paste0("Total Classifications: ", length(get_group_table(omicsData$objModel))),
    "continuous_response" = "Continuous Response",
    "sample_predictor_ratio" = paste0("Sample/Predictor Ratio: ", ncol(omicsData$objModel$e_data) - 1, ":", 
           nrow(omicsData$objModel$e_data)),
    "prop_missing" = paste0("Proportion Missing: ", sum(missingness$num_non_NA), "/", total, " (",
           signif(sum(missingness$num_non_NA)/total*100, 3), "%) of possible datapoints\n observed)"),
    "explainability" = "Explainability",
    "best_predictors" = "Keeps Best Predictors",
    "equation" = "Generates Equation",
    "avoids_overfitting" = "Avoids Overfitting",
    "high_correlation" = paste("Handles High Correlation (Current data", ifelse(correlation, "is", "is not"), "highly correlated)"),
    "high_dimensional" = "High Dimensional",
    "handles_outliers" = "Handles Outliers"
  )
  
  summary <- list(
    "total_samples" = "This score is a pass/fail metric, where a pass indicates that the number of samples in the data meet or exceed the minimum number of samples required for the respective model.",
    "few_samples" = "This score indicates the performance of a given model with the few total samples in the data. A higher number indicates better performance.",
    "total_classifications" = "This score is a pass/fail metric, where a pass indicates that the number of classifications in the data meet or exceed the minimum number of classifications required for the respective model.",
    "continuous_response" = "This score indicates how well a given model handles continuous response variables.",
    "sample_predictor_ratio" = "This score indicates the performance of a given model with the ratio of samples to predictors in the data. A higher number indicates better performance.",
    "prop_missing" = "This score indicates the performance of a given model with the proportion of missing data to present data. A higher number indicates better performance.",
    "explainability" = "This value indicates how explainable a given model is.",
    "best_predictors" = "This value indicates whether or not a given model performs feature selection.",
    "equation" = "This value indicates whether or not a given model generates a closed-form equation after it is run.",
    "avoids_overfitting" = "This score indicates how robust a given model is to avoiding overfitting.",
    "high_correlation" = "This score indicates how robust a given model is to data with high correlation.",
    "high_dimensional" = "This score indicates how well a given model handles data with a large number of dimensions (i.e. predictors).",
    "handles_outliers" = "This score indicates how robust a given model is to data that contains outliers."
  )
  
  contents <- list(
    
  )
  
  return(list(titles = titles, summary = summary, contents = contents, citations = citations))
}

observeEvent(input$em_column_hover, {
  req(!is.null(input$em_column_hover))
  
  get_em <- get_em_column_info()
  
  addPrompter(
    session, 
    paste0("em_info_", input$em_column_hover),
    title = paste0(
      get_em[["titles"]][[input$em_column_hover]],
        "\n\n",
        paste(
          strwrap(get_em[["summary"]][[input$em_column_hover]], width = 64),
          collapse = "\n"
        ),
        "\n\n",
        "Click for more info..."
      ),
    size = "large"
  )
})

observeEvent(input$em_column_info, {
  req(!is.null(input$em_column_info))
  
  get_em <- get_em_column_info()
  
  showModal(
    modalDialog(
      title = get_em[["titles"]][[input$em_column_info$name]],
      get_em[["summary"]][[input$em_column_info$name]],
      br(),
      get_em[["contents"]][[input$em_column_info$name]],
      hr(),
      h4("Citations:"),
      get_em[["citations"]][[input$em_column_info$name]]
    )
  )
})

output$EM_dashboard <- renderUI({
  
  # fluidPage(
  #   
  #   br(),
  
  req(length(dashboard()) > 0)
  
  df <- as.data.frame(dashboard())
  
  if(isTruthy(input$skip_ag)){
    picker <- names(models_long_name)[models_long_name == input$pick_model]
    df <- df[df$Method == picker, ]
  } else {
    df <- df[1:min(
      nrow(df), 
      max(
        3, 
        ifelse(is.null(input$em_model_count), 
               0, input$em_model_count)
      )
    ),]
  }
    
    fluidRow(
      
      column(12,
      tags$div(
        class = "container mt-5",
        style = "width: 100%",
        tags$div(
          class = "d-lg-flex align-items-lg-center py-4",
          tags$div(
            class = "h3 text-muted",
            "Recommended Models"
          )
        ),
        div(
          id = "top",
          tbl(df, "Method", NULL),
          style = 'height:600px; overflow-y: scroll; overflow-x: scroll; width = 90%',
        )
      )))
  # )
  
  
})

output$pick_EM_model_UI <- renderUI({
  
  selected <- isolate(input$pick_model_EM)
  
  if(input$skip_ag && !is.null(input$pick_model)){
    choices <- models_long_name[models_long_name == input$pick_model]
  } else {
    
    types_dashboard <- dashboard()$Method
    types_supported <- types_dashboard[types_dashboard %in% names(models_long_name)]
    
    choices <- models_long_name[types_supported]

  }

  pickerInput("pick_model_EM", label = "Select a model:",
              choices = choices[!is.na(choices)], 
              selected = selected
              )
  
})

output$em_model_display_slider <- renderUI({
  
  req(!input$skip_ag)
  
  max <- if(supervised()){
    length(models_supervised)
  } else length(models_unsupervised)
  
  value <- switch(
    input$user_level_pick,
    beginner = 3,
    familiar = 5,
    expert = max
  )
  
  sliderInput(
    "em_model_count",
    "How many top models to show?",
    min = 3,
    max = max, 
    value = value,
    step = 1
  )
  
})
