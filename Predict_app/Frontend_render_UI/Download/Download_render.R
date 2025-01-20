

# download tab
output$Download_button <- renderUI({
  button <- downloadButton(
    "download_processed_data",
    tags$b("Download Bundle")
  )
  
  if(is.null(input$makezipfile)  || input$makezipfile == 0) button <- disabled(button)
  
  div(
    id = "js_downloadbutton",
    style = "margin-left:4px;float:left",
    class = "grey_button",
    button
  )
})

output$preview_selected_dwn_UI <- renderUI({
  
  req(!is.null(download_preview$current))
  if(download_preview$plot){
    
    div(
      uiOutput("preview_selected_dwn_plot"),
      br(),
      uiOutput("edit_plot")
    )
  } else {
    
    div(
      DTOutput("preview_selected_dwn_table"),
      br(),
      strong("Note: only first 500 rows will be loaded in preview."),
      br()
    )
  }
  
})


## Table fillings

output$download_plot_table <- renderDT(height = "650px",{
  
  session$sendCustomMessage("unbind-DT-RR", "download_plot_table")
  
  plots_made <- plot_table_current$table
  plots_made[sapply(plots_made, is.null)] <- NULL
  plot_names <- plot_table_current$names[names(plot_table_current$names) %in% names(plots_made)]
  
  data.frame(
    Visualization = unname(unlist(plot_names)),
    `Download?` = shinyInput(safeCheckboxInput, length(plot_names), value = T, "checkplot", width = "10%"),
    check.names = F
  )
  
}, options = list(
  preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
  drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "),
  dom = "t", 
  scrollY = "500px"
),
selection = "single",
escape = FALSE
)

output$download_table_table <- renderDT(height = "650px",{
  
  session$sendCustomMessage("unbind-DT-RR", "download_table_table")

  table_made <- table_table_current$table
  table_made[sapply(table_made, is.null)] <- NULL
  table_names <- table_table_current$names[names(table_table_current$names) %in% names(table_made)]
  
  data.frame(
    Table = unname(unlist(table_names)),
    `Download?` = shinyInput(checkboxInput, length(table_names), value = T, "checktable", width = "10%"),
    check.names = F
  )
}, 
options = list(
  preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
  drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "),
  dom = "t", 
  scrollY = "500px"
),
selection = "single",
escape = FALSE
)
  
map(c("plot", "table"), function(type){
  
  observeEvent(input[["dwn_select_all"]], {
    
    str <- paste0("check", type)
    values <- names(input)[str_detect(names(input), str)]
    
    map(values, function(val){
      updateCheckboxInput(session = session, val, value = T)
    })
    
  })
  
  observeEvent(input[["dwn_deselect_all"]], {
    
    str <- paste0("check", type)
    values <- names(input)[str_detect(names(input), str)]
    
    map(values, function(val){
      updateCheckboxInput(session = session, val, value = F)
    })
    
  })
  
})

## Accordian behavior
observeEvent(input$dwn_done, {
  
  updateBoxCollapse(session = session, 
                    id = "download_collapse_pages", 
                    close = "download_tabset"
  )
  
})

# Buttons
output$Bundle_button <- renderUI({
  
  plots <- getShinyInput(input, "checkplot")
  tables <- getShinyInput(input, "checktable")
  
  cond <- any(as.logical(unlist(c(plots, tables))))
  
  button <- appButton(inputId = "makezipfile",
                      label = tags$b("Bundle up all selected items"),
                      icon = icon("briefcase"),
                      lib = "glyphicon",
                      style = "margin-left:5px"
  )
  
  if(!cond) button <- disabled(button)
  
  div(
    id = "js_zipbutton",
    style = "float:left",
    class = "grey_button",
    button
  )
  
})

