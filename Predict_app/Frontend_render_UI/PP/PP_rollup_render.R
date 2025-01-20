observeEvent(omicsData$obj, {
  
  if(inherits(omicsData$obj, "pepData")){
    
    appendTab(
      "plots_preprocess",
      tabPanel(
        "Rollup",
        br(),
        withSpinner(plotlyOutput("transformation_rollup_plot"))
      )
    )
  }
  
})

observeEvent(omicsData$obj_sl_rollup, {
  if(!is.null(attributes(omicsData$obj_sl_rollup)$group_DF)){
    p <- plot(omicsData$obj_sl_rollup, use_VizSampNames = T,
         color_by = "Group", order_by = "Group")
  } else {
    p <- plot(omicsData$obj_sl_rollup, use_VizSampNames = T)
  }
  
  plot_table_current$table$PP__rollup__post <- p
})

# rollup plot
output$transformation_rollup_plot <- renderPlotly({
  
  validate(
    need(!is.null(plot_table_current$table$PP__rollup__post), 
         'Confirm filter pre-processing specifications to the left to generate this image.')
  )

  ggplotly(plot_table_current$table$PP__rollup__post)
  
})