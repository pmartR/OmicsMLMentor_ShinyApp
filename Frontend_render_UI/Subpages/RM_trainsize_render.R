

observeEvent(input$rec_split, {
  
  reset("nTest_count")
  reset("nTest_prop")
  
})

observeEvent(input$done_crossval_option, {
  updateBoxCollapse(session, id = "TS_side_collapse", close = "train_param_RM")
  shinyjs::show("complete_TS_RM")
})

output$TS_preview_plot <- renderPlot({
  
  data <- omicsData$objPP
  
  group_info <- get_group_DF(data)
  
  if(input$numb_test == "Proportion"){
    req(!is.na(input$nTest_prop))
    test_data <- group_info %>% group_by(Group) %>% slice_sample(prop = input$nTest_prop)
  } else {
    req(!is.na(input$nTest_count))
    prop_count <- input$nTest_count/nrow(group_info)
    test_data <- group_info %>% group_by(Group) %>% slice_sample(prop = prop_count)
  }
  
  group_info$Status <- "Training data"
  
  group_info$Status[group_info[[1]] %in% test_data[[1]]] <- "Testing data"
  
  group_info$Validation <- as.numeric(as.factor(group_info$Status))
  
  ggplot(group_info, aes(x = Status, fill = Group)) + 
    geom_bar() +
    theme_bw() + labs(x = "", fill = "", y = "Number of samples")
  
})
