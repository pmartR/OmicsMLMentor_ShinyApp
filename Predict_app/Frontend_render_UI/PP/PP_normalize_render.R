
observeEvent(omicsData$obj_norm, {
  
  req(!is.null(omicsData$obj_norm))
  
  if(!is.null(attributes(omicsData$obj_norm)$group_DF)){
    p <- plot(omicsData$obj_norm, use_VizSampNames = T,color_by = "Group", order_by = "Group")
  } else {
    p <- plot(omicsData$obj_norm, use_VizSampNames = T)
  }
  
  plot_table_current$table$PP__normalization__post <- p
  
})

# norm plot
output$transformation_norm_plot <- renderPlotly({
  
  validate(
    need(!is.null(plot_table_current$table$PP__normalization__post), 
         'Confirm filter pre-processing specifications to the left to generate this image.')
  )
  
  ggplotly(plot_table_current$table$PP__normalization__post)

})