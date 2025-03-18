

### Load up previews by table click ###.

map(c("table", "plot"), function(type){
  
  str <- paste0("download_", type, "_table_cell_clicked")
  
  observeEvent(input[[str]], {
    
    req(length(input[[str]]) > 0 && !is.null(input[[str]]$row))
    
    pull_info <- get(paste0(type, "_table_current"))
    req(!is.null(pull_info))
    index <- input[[str]]$row
    
    pull_info$names <- pull_info$names[names(pull_info$table)]
    
    pull_table <- tibble(
      rows = pull_info$names,
      list_el = pull_info$table 
    ) %>% filter(!map_lgl(list_el, is.null))
    
    download_preview$current <- pull_table$list_el[[index]]
    download_preview$name <- names(pull_table$list_el)[index]
    download_preview$plot <- type == "plot"
    
  })
})

### Set the correct UI for the selected preview ###

output$preview_selected_dwn_plot <- renderUI({
  req(download_preview$plot)
  
  plot_style <- plot_table_current$plot_options[[download_preview$name]]
  tagList(
    if (inherits(download_preview$current, "plotly") || 
        (!is.null(plot_style) && plot_style$meta[1] == "html")) {
      plotlyOutput("preview_selected_dwn_plot_plotly")
    } else {
      plotOutput("preview_selected_dwn_plot_ggplot")
    },
    if (inherits(download_preview$current, "plotly")) {
      tagList("Export format: ", tags$b("HTML"))
    } else if (!is.null(plot_style)) {
      tagList("Export format: ", tags$b(str_to_upper(plot_style$meta[1])))
    } else {
      tagList("Export format: ", tags$b("PNG"))
    }
  )
})


output$preview_selected_dwn_plot_ggplot <- renderPlot({
  req(download_preview$plot)
  
  plot_style <- plot_table_current$plot_options[[download_preview$name]]
  
  p <- apply_plot_style(download_preview$current, plot_style)
  
  p
})
output$preview_selected_dwn_plot_plotly <- renderPlotly({
  req(download_preview$plot)
  
  plot_style <- plot_table_current$plot_options[[download_preview$name]]
  
  if (inherits(download_preview$current, "plotly")) {
    warning("DEVELOPER: Please ensure this plot is stored as a ggplot object if possible! Plots can be made interactive with ggplotly() when being displayed, but should be stored as a ggplot.")
    return(apply_plot_style(download_preview$current, plot_style))
  }
  
  p <- apply_plot_style(download_preview$current, plot_style)
  
  ggplotly(p)
})

output$preview_selected_dwn_table <- renderDT(height = "450px",{
  req(!download_preview$plot)
  df <- download_preview$current
  
  if(nrow(df) > 500){
    df <- df[1:500,]
  }
  df
}, 
selection = "none",
options = list(dom = "tp", 
               scrollY = "300px"))