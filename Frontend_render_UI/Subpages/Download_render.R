
# File string holder
zipped_file <- reactiveValues(fs = NULL)

# Buttons
output$Bundle_button <- renderUI({
  
  plots <- getShinyInput(input, "checkplot_")
  tables <- getShinyInput(input, "checktable_")
  
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

output$New_model_button <- renderUI({
  
  cond <- is.null(zipped_file$fs)
  
  button <- appButton(inputId = "new_model",
                      label = tags$b("Try a different model"),
                      icon = icon("fast-backward"),
                      lib = "glyphicon",
                      style = "margin-left:5px"
  )
  
  if(cond) button <- disabled(button)
  
  div(
    id = "js_newmodelbutton",
    style = "float:left",
    class = "grey_button",
    button
  )
  
})

output$Download_button <- renderUI({
  
  plots <- getShinyInput(input, "checkplot_")
  tables <- getShinyInput(input, "checktable_")
  
  cond <- is.null(zipped_file$fs)
  
  button <- downloadButton(
    "download_processed_data",
    tags$b("Download bundle")
  )
  
  if(cond) button <- disabled(button)
  
  div(
    id = "js_downloadbutton",
    style = "margin-left:4px;float:left",
    class = "grey_button",
    button
  )
  
})


## Accordian behavior
observeEvent(input$upload_dwn_done, {
  
  updateBoxCollapse(session = session, 
                    id = "download_collapse_pages", 
                    close = "download_tabset_upload",
                    open = "download_tabset_QC"
  )
  
})

observeEvent(input$QC_dwn_done, {
  
  updateBoxCollapse(session = session, 
                    id = "download_collapse_pages", 
                    close = "download_tabset_QC",
                    open = "download_tabset_MSU"
  )
  
})

observeEvent(input$MSU_dwn_done, {
  
  updateBoxCollapse(session = session, 
                    id = "download_collapse_pages", 
                    close = "download_tabset_MSU",
                    open = "download_tabset_PP"
  )
  
})

observeEvent(input$PP_dwn_done, {
  
  updateBoxCollapse(session = session, 
                    id = "download_collapse_pages", 
                    close = "download_tabset_PP",
                    open = "download_tabset_RM"
                    )
  
})

observeEvent(input$RM_dwn_done, {
  
  updateBoxCollapse(session = session, 
                    id = "download_collapse_pages",
                    close = "download_tabset_RM"
  )
  
})

## Checkbox behavior

map(c("upload", "QC", "MSU", "PP", "RM"), function(pg){
  
  map(c("plot", "table"), function(type){
    
    observeEvent(input[[paste0(pg, "_dwn_select_all")]], {
      
      str <- paste0("check", type, "_", pg)
      values <- names(input)[str_detect(names(input), str)]
      
      map(values, function(val){
        updateCheckboxInput(session = session, val, value = T)
      })
      
    })
    
    observeEvent(input[[paste0(pg, "_dwn_deselect_all")]], {
      
      str <- paste0("check", type, "_", pg)
      values <- names(input)[str_detect(names(input), str)]
      
      map(values, function(val){
        updateCheckboxInput(session = session, val, value = F)
      })
      
    })
    
  })
  
})

