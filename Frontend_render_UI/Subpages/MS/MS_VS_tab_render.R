## VS UI rendering

## Column tracking module
output$vs_cols_select_UI <- renderUI(KeepColsModule())

# ## Categorical/factor adjust -- depends on tracked cols
output$vs_cols_categorical_UI <- renderUI(FactorColsModule())

output$vs_cols_addint_UI <- renderUI(AddInteractionModule())


## Previews
output$preview_all_data_UI <- renderUI({
  
  req(!is.null(input$data_type))
  
  ## Get valid names
  gen_tab_names <- unlist(map(names(reactive_dataholder), function(x) 
    if(!is.null(reactive_dataholder[[x]]$file)) return(x) else NULL))
  
  gen_tab_names <- gen_tab_names[gen_tab_names != "e_data"]
  
  ## make tabs
  tabs <- map(sort(gen_tab_names), function(lab){
    
    tablabel <- switch(lab,
                       e_data = ifelse(input$data_type == "RNA-seq",
                                       "Expression data",
                                       "Abundance data"),
                       f_data = "Sample Information",
                       e_meta = "Biomolecule information"
    )
    
    tabPanel(tablabel,
             br(),
             strong("Note: Categorical columns are highlighted in yellow. Numeric columns are highlighted in blue."),
             br(),
             DTOutput(paste0("DT_VS_", lab))
    )
    
  })
  
  do.call(tabsetPanel, c(tabs, list(id = "vs_preview", selected = input$vs_preview)))
})

## DTs
map(c("e_data", "f_data", "e_meta"), function(lab) {
  output[[paste0("DT_VS_", lab)]] <- renderDT({
    
    use_file <- preview_keep_cols$result[[paste0(lab, "_file")]]
    req(use_file)
    class_cols <- purrr::map_chr(1:ncol(use_file), function(n) class(use_file[[n]]))
    
    dt <- datatable(use_file,
                    selection = 'none',
                    options = list(dom = 'tpi', 
                                   pageLength = 10, 
                                   scrollX = T)
    )
    
    dt <- formatStyle(dt, columns = which(class_cols %in% c("factor", "character")), 
                      backgroundColor = "#fcf4d9")
    
    dt <- formatStyle(dt, columns = which(!(class_cols %in% c("factor", "character"))), 
                      backgroundColor = "#DFE9F5")
    
    dt
    
  })
})

output$vs_tab_plots_UI <- renderUI({
  
  switch_text <- if(inherits(omicsData$objMSU, "seqData")) "Count data" else "Abundance data"
  
  names <- c(switch_text, "Sample Information", "Biomolecule data")
  choices <- names(omicsData$objMSU)
  if(!("f_data" %in% choices)) names <- names[-2]
  names(choices) <- names[1:length(choices)]
  
  div(
    fluidRow(
    column(6, pickerInput("VS_data_picker",
                "Select data to visualize",
                choices = choices
    )),
    column(6, 
    uiOutput("VS_column_examine")),
    br(),
    column(12, withSpinner(plotlyOutput("vs_tab_plots")))
  ))
  
})

output$VS_column_examine <- renderUI({
  
  req(input$VS_data_picker != "e_data")
  
  if(input$VS_data_picker == "f_data"){
    choices <- colnames(omicsData$objMSU$f_data)
    choices <- choices[!(choices %in% get_fdata_cname(omicsData$objMSU))]
  } else {
    choices <- colnames(omicsData$objMSU$e_meta)
    choices <- choices[!(choices %in% get_emeta_cname(omicsData$objMSU))]
  }
  
  pickerInput("VS_column_picker",
              "Select column to visualize",
              choices = choices
  )
  
})

output$vs_tab_plots <-  renderPlotly({
  
  if(input$VS_data_picker == "e_data"){
    plot(omicsData$objMSU)
  } else if(input$VS_data_picker == "f_data"){
    
    df <- omicsData$objMSU$f_data
    df <- df[colnames(df) != get_fdata_cname(omicsData$objMSU)]
    df <- gather(df)
    df <- df[!is.na(df$value),]
    
    df <- df[df$key %in% input$VS_column_picker,]
    
    if(all(is.na(as.numeric(as.character(df$value))))){
      return(
        ggplot(df, aes(x = value, fill = value)) + 
          geom_bar(color = "black", show.legend = F) + theme_bw() + 
          ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
          labs(x = "Level", y = "Samples per level", title = input$Gplot_picker)
      )
    } else {
      
      df$value <-  as.numeric(as.character(df$value))
      return(
        ggplot(df, aes(x = value, fill = key)) + geom_histogram(show.legend = F) + theme_bw() + 
          ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
          labs(x = "Value", y = "Frequency")
      )
    }
    
  } else if (input$VS_data_picker == "e_meta"){
    
    df <- omicsData$objMSU$e_meta
    df <- df[colnames(df) != get_emeta_cname(omicsData$objMSU)]
    df <- gather(df)
    df <- df[!is.na(df$value),]
    
    df <- df[df$key %in% input$VS_column_picker,]
    
    if(all(is.na(as.numeric(as.character(df$value))))){
      return(
        ggplot(df, aes(x = value, fill = value)) + 
          geom_bar(color = "black", show.legend = F) + theme_bw() + 
          ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
          labs(x = "Level", y = "Biomolecules per level", title = input$Gplot_picker)
      )
    } else {
      
      df$value <-  as.numeric(as.character(df$value))
      return(
        ggplot(df, aes(x = value, fill = key)) + geom_histogram(show.legend = F) + theme_bw() + 
          ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
          labs(x = "Value", y = "Frequency")
      )
    }
    
  }
  
})


#### Accordian behavior

output$detected_box_varsel <- renderUI({
  
  req(input$vscols_ints_done > 0)
  collapseBox("Data Properties",
              value = "detected_plots",
              uiOutput("vs_tab_plots_UI")
  )
})

observeEvent(input$vscols_options_done, {
  req(input$vscols_options_done > 0)
  
    updateBoxCollapse(session, "vs_collapse_left", close = "use_cols_vs", open = "factor_cols_vs")
    if(input$user_level_pick == 'beginner') shinyjs::show("done_VS")
    
})

observeEvent(input$vscols_cats_done, {
  req(input$vscols_cats_done > 0)
  updateBoxCollapse(session, "vs_collapse_left", close = "factor_cols_vs", open = "int_cols_vs")
})

observeEvent(input$vscols_ints_done, {
  req(input$vscols_ints_done > 0)
  updateBoxCollapse(session, "vs_collapse_left", close = "int_cols_vs")
  shinyjs::show("done_VS")
})

observeEvent(input$done_VS, {
  req(input$done_VS > 0)
  updateBoxCollapse(session, "vs_collapse_right", close = "data_preview_all", open = "detected_plots")
})

