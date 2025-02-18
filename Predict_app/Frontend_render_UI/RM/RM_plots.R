
observeEvent(omicsData$obj_predictions, {
  
  if(supervised()){
    if(input$use_fdata == "No" || 
       !attr(get_group_DF(omicsData$model$norm_omics), "main_effects") %in% colnames(omicsData$obj$f_data)){
      plot_table_current$table$RM__model_eval__full__roc_curve <- NULL
      plot_table_current$table$RM__model_eval__full__confusion_heatmap <- NULL
    } else {
      plot_table_current$table$RM__model_eval__full__roc_curve = plot(
        omicsData$obj_predictions, plotType = "roc_curve")
      
      plot_table_current$table$RM__model_eval__full__confusion_heatmap <- plot(
        omicsData$obj_predictions, plotType = "confusion_heatmap")
    }
    
    plot_table_current$table$RM__model_eval__full__prediction_bar = plot(
      omicsData$obj_predictions, plotType = "prediction_bar")
  } else {
    
    method <- attr(omicsData$model$model, "args_unsup")$slMethod

    slData_use <- as.slData(omicsData$model$pp_omics)
    
    if(is.null(slData_use$f_data)){
      slData_use$f_data <- data.frame(
        SampleID = colnames(slData_use$e_data)[!(colnames(slData_use$e_data) %in% attr(slData_use, "cnames")$edata_cname)]
          )
    }
    slData_use$f_data$Source <- "Model data"
    
    model_data_process <- unsup_plot_call(omicsData$model$model, 
                                          slData_use, 
                                          method = method, 
                                          color_by = "Source", 
                                          supervised = supervised(), 
                                          plot_type = "scatter"
                                          
                                          )
                                          
    new_data_process <- unsup_plot_call(omicsData$obj_predictions, 
                                        as.slData(omicsData$obj_predictions$combine_data), 
                                        method = method, 
                                        color_by = "Source", 
                                        supervised = supervised(), 
                                        plot_type = "scatter")
    
    plot_table_current$table$RM__model_unsup_newdata <- new_data_process$plot
    plot_table_current$table$RM__model_unsup_model <- model_data_process$plot
    
    table_table_current$table$RM__model_unsup_newdata <- new_data_process$table
    table_table_current$table$RM__model_unsup_model <- model_data_process$table
    
  }
  
})

observeEvent(input$redraw_plot, {
  
  method <- attr(omicsData$model$model, "args_unsup")$slMethod
  
  color_by <- if(isTruthy(input$color_by) && input$color_by != "Parameter clusters") 
    input$color_by else NULL
  plot_type <- input$plot_type
  
  slData_use <- as.slData(omicsData$model$pp_omics)
  
  if(is.null(slData_use$f_data)){
    slData_use$f_data <- data.frame(
      SampleID = colnames(slData_use$e_data)[!(colnames(slData_use$e_data) %in% attr(slData_use, "cnames")$edata_cname)]
    )
  }
  slData_use$f_data$Source <- "Model data"
  
  
  color_by_model <- NULL
  color_by_model <- if(
    !is.null(slData_use$f_data) && !is.null(color_by) &&
    color_by %in% colnames(slData_use$f_data)){
    color_by
  }
  
  model_data_process <- unsup_plot_call(omicsData$model$model, 
                                        slData_use, 
                                        method = method, 
                                        color_by = color_by_model, 
                                        supervised = supervised(), 
                                        plot_type = plot_type,
                                        label.vjust = input$dendro_vjust,
                                        label.hjust = input$dendro_hjust,
                                        label_size = input$dendro_label_size,
                                        component_x = input$unsup_embed_xaxis,
                                        component_y = input$unsup_embed_yaxis
                                        )
  
  new_data_process <- unsup_plot_call(omicsData$obj_predictions, 
                                      as.slData(omicsData$obj_predictions$combine_data), 
                                      method = method, 
                                      color_by = color_by, 
                                      supervised = supervised(), 
                                      plot_type = plot_type,
                                      label.vjust = input$dendro_vjust,
                                      label.hjust = input$dendro_hjust,
                                      label_size = input$dendro_label_size,
                                      component_x = input$unsup_embed_xaxis,
                                      component_y = input$unsup_embed_yaxis
                                      )
  
  plot_table_current$table$RM__model_unsup_newdata <- new_data_process$plot
  plot_table_current$table$RM__model_unsup_model <- model_data_process$plot
  
  table_table_current$table$RM__model_unsup_newdata <- new_data_process$table
  table_table_current$table$RM__model_unsup_model <- model_data_process$table
  
})

output$structure_plot <- renderPlot({
  
  p1 <- plot_table_current$table$RM__model_unsup_newdata 
  p2 <- plot_table_current$table$RM__model_unsup_model 
  
  shiny::validate(
    need(!is.null(p1) && !is.null(p2),
    "Click 'Run Model' to see results.")
  )
  
  
  p1 <- p1 + ggplot2::labs(title = "New data and model data")
  p2 <- p2 + ggplot2::labs(title = "Model data")
  
  req(!is.null(p1) && !is.null(p2))
  wrap_plots(p1, p2)
})

output$structure_plot_style_ui <- renderUI({
  
  method <- attr(omicsData$model$model, "args_use")$method
  
  comb_omics <- omicsData$obj_predictions$combine_data
  
  model <- omicsData$obj_predictions
  
  # always have a redraw plot button
  element_list <- list(
    actionButton("redraw_plot", "Redraw plot"), 
    br(), br())
  
  # if(attr(omicsData$model$model, "axis") == "samples"){
    colors <- colnames(comb_omics$f_data) 
  # } else {
  #   colors <- colnames(omicsData$objPP$e_meta)
  # }

  colors <- colors[!(colors %in% c(attr(comb_omics, "cnames")$fdata_cname,
                                   attr(comb_omics, "cnames")$edata_cname))]
  
  if(length(colors) > 0 && 
     (all(colors %in% c("group", "Source")) || all(colors %in% c("Col1", "Source")))  && 
     (all(unique(comb_omics$f_data[["group"]]) %in% c("Unknown", "Model data")) ||
     all(unique(comb_omics$f_data[["Col1"]]) %in% c(
       unique(comb_omics$f_data[[attr(comb_omics, "cnames")$fdata_cname]]), "Model data")))
     ){
    colors <- "Source"
  }
  
  choices <- if(inherits(model, "slRes.embed")){
    
    if(!(length(colors) > 0)){
      return(do.call(div, c(list(class = "inline-wrapper-1"), element_list)))
    }
    
    
    colors
    
  } else {
    c("Parameter clusters", colors)
  }
  
  # color by is probably not relevant for e.g. dendrogram plots
  if (isTruthy(input$plot_type %in% c("pca", "scatter"))) {
    color_by_picker <- pickerInput("color_by", "Color by:", choices = choices, selected = "Source")
    
    element_list[[length(element_list) + 1]] <- color_by_picker
  } else if (isTruthy(input$plot_type %in% c("dendro"))) {
    max_clusts = if (attr(model, "axis") == "samples") {
      length(attr(model, "sample_names"))
    } else if (attr(model, "axis") == "biomolecules") {
      nrow(attr(model, "feature_info"))
    }
    
    color_by_picker <- pickerInput("color_by", "Color by:", choices = choices, selected = "Source")
    vjust_input = numericInput("dendro_vjust", "Vertical adjustment", value = 0.5, min = 0, max = 1, step = 0.1)
    hjust_input = numericInput("dendro_hjust", "Horizontal adjustment", value = 1.5, min = 0, max = 1, step = 0.1)
    label_size_input = numericInput("dendro_label_size", "Label size", value = 5, min = 0.1, max = 20, step = 0.1)
    
    element_list[[length(element_list) + 1]] <- color_by_picker
    element_list[[length(element_list) + 1]] <- vjust_input
    element_list[[length(element_list) + 1]] <- hjust_input
    element_list[[length(element_list) + 1]] <- label_size_input
    
  }
  
  ### plot the pcs
  
  # This is currently only valid for embedding plots
  if (inherits(model, "slRes.embed")) {
    element_list[[length(element_list) + 1]] <- uiOutput("pc_xaxis_UI")
    element_list[[length(element_list) + 1]] <- uiOutput("pc_yaxis_UI")
  }
  
  return(do.call(div, c(list(class = "inline-wrapper-1"), element_list)))
  
})

# Picker for plot type, depending on the class of the output.  This will either be slRes.cluster or slRes.embed.
# currently pretty few plot options for these.
output$plot_type_UI <- renderUI({
  
  req(inherits(omicsData$model$model, c("slRes.cluster", "slRes.embed")))
  
  if (inherits(omicsData$model$model, "slRes.cluster")) {
    fit_obj <- workflows::extract_fit_engine(omicsData$model$model)
    # always has at least the pca scatter
    choices <- c("Scatterplot" = "pca")
    if (inherits(fit_obj, "hclust")) {
      choices <- c(choices, "Dendrogram" = "dendro")
    }
  } else {
    choices <- c("Scatterplot" = "scatter")
  }
  
  picker_out = pickerInput(
    "plot_type", "Plot type", choices = choices,
    selected = isolate(input$plot_type)
  )
  
  return(picker_out)
  
})

output$pc_xaxis_UI <- renderUI({
  req(inherits(omicsData$model$model, "slRes.embed"))
  num_comp <- omicsData$model$model$steps[[1]]$num_comp
  
  choices = setdiff(
    c(1:num_comp),
    input$unsup_embed_yaxis
  )
  
  picker_out <- pickerInput(
    "unsup_embed_xaxis", "Plot which component on x-axis?" , choices = choices,
    selected = input$unsup_embed_xaxis
  )
  
  return(picker_out)
})

output$pc_yaxis_UI <- renderUI({
  req(inherits(omicsData$model$model, "slRes.embed"))
  num_comp <- omicsData$model$model$steps[[1]]$num_comp
  
  choices = setdiff(
    c(1:num_comp),
    input$unsup_embed_xaxis
  )
  
  selected = if(!is.null(input$unsup_embed_yaxis)) input$unsup_embed_yaxis else choices[2] 
  
  picker_out <- pickerInput(
    "unsup_embed_yaxis", "Plot which component on y-axis?", choices = choices,
    selected = selected
  )
  
  return(picker_out)
})

output$true_pos_picker_UI <- renderUI({

  groups <- unique(attr(omicsData$model$model, "response_performance")$Group)
  
  req(!is.null(groups))
  pickerInput(inputId = "true_pos_select",
              label = "Designate true positive event",
              choices = groups
              )
})


observeEvent(c(omicsData$obj_predictions, input$true_pos_select), {
  
  req(!is.null(input$true_pos_select))
  
  if(input$use_fdata == "Yes" && 
     attr(get_group_DF(omicsData$model$norm_omics), "main_effects") %in% colnames(omicsData$obj$f_data)){
    
    plot_table_current$table$RM__model_eval__full__confidence_scatter = plot(
      omicsData$obj_predictions, plotType = "confidence_scatter", 
      pos_class = input$true_pos_select)
    
  } else {
    
    pos_class <- input$true_pos_select
    pred_df <- attr(omicsData$obj_predictions, "prediction_test")
    groups <- unique(attr(omicsData$model$model, "response_performance")$Group)
    
    xvar = paste0(".pred_", pos_class)

    # make the vline move for different numbers of classes
    gt_chance_prob <- 1 / length(groups)
    
    added_vline = ggplot2::geom_vline(xintercept = gt_chance_prob, linetype = "dashed")
    
    breaks = round(union(pretty(range(c(0,1))), gt_chance_prob), 2)
    
    pred_df$Sample <- pred_df$`__SAMPNAMES__`
    pred_df$Confidence <- pred_df[[xvar]]
    
    
    p <- ggplot2::ggplot(aes(y = Sample, x = Confidence), 
                         data = pred_df) 
    
    p <- p + suppressWarnings(ggplot2::geom_point(aes(color = response, text = Sample))) +
      added_vline +
      ggplot2::scale_x_continuous(limits = c(0, 1), breaks = breaks) +
      ggplot2::scale_color_discrete(name = "Class") +
      ggplot2::xlab(sprintf("Model confidence that sample is of class %s", pos_class)) +
      ggplot2::ggtitle("Model confidence of positive class for all samples") +
      ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
      )
    
    plot_table_current$table$RM__model_eval__full__confidence_scatter <- p
    
  }
})


output$predict_plot_ROC <- renderPlotly({
  validate(need(!is.null(plot_table_current$table$RM__model_eval__full__roc_curve),
           "Only available where sample data is provided with exact matches to model prediction classes."))
  ggplotly(plot_table_current$table$RM__model_eval__full__roc_curve)
})

output$predict_plot_predictBar <- renderPlotly({
  req(!is.null(plot_table_current$table$RM__model_eval__full__prediction_bar))
  ggplotly(plot_table_current$table$RM__model_eval__full__prediction_bar)
})

output$predict_plot_confusionHeatmap <- renderPlotly({

  validate(need(!is.null(plot_table_current$table$RM__model_eval__full__confusion_heatmap),
           "Only available where sample data is provided with exact matches to model prediction classes."))
  ggplotly(plot_table_current$table$RM__model_eval__full__confusion_heatmap)
})

output$predict_plot_confidenceScatter <- renderPlotly({
  req(!is.null(plot_table_current$table$RM__model_eval__full__confidence_scatter))
  ggplotly(plot_table_current$table$RM__model_eval__full__confidence_scatter, tooltip = c("text", "x"))
})
