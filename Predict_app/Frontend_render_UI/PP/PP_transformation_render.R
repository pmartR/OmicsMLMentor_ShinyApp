
observeEvent(omicsData$obj_scaled, {
  
  req(!is.null(omicsData$obj_scaled))
  if(!is.null(attributes(omicsData$obj_scaled)$group_DF)){
    p <- plot(omicsData$obj_scaled, use_VizSampNames = T,
         color_by = "Group", order_by = "Group")
  } else {
    p <- plot(omicsData$obj_scaled, use_VizSampNames = T)
  }
  
  plot_table_current$table$PP__transform <- p
  
})

# log 2 plot
output$transformation_scaling_plot <- renderPlotly({
    ggplotly(plot_table_current$table$PP__transform)
})



