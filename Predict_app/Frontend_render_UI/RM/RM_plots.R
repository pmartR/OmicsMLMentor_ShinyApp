
observeEvent(omicsData$obj_predictions, {
  
  if(input$use_fdata == "No"){
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
  
  if(input$use_fdata == "Yes"){
    
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
