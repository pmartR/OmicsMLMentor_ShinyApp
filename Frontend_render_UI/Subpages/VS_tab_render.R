## VS UI rendering

## Column tracking module
output$vs_cols_select_UI <- renderUI(KeepColsModule("keepcols"))

# ## Categorical/factor adjust -- depends on tracked cols
output$vs_cols_categorical_UI <- renderUI(FactorColsModule("factor_cols"))

output$vs_cols_addint_UI <- renderUI(AddInteractionModule("add_int"))


## Previews
output$preview_all_data_UI <- renderUI({
  
  ## Get valid names
  gen_tab_names <- unlist(map(names(reactive_dataholder), function(x) 
    if(!is.null(reactive_dataholder[[x]]$file)) return(x) else NULL))
  
  ## make tabs
  tabs <- map(sort(gen_tab_names), function(lab){
    
    tablabel <- switch(lab,
                       e_data = ifelse(input$data_type == "RNA-seq",
                                       "Expression data",
                                       "Abundance data"),
                       f_data = "Sample data",
                       e_meta = "Biomolecule information"
    )
    
    tabPanel(tablabel,
             br(),
             strong("Note: Categorical columns are highlighted in yellow"),
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
    
    dt
    
  })
})


#### Accordian behavior

observeEvent(input$vscols_options_done, {
  req(input$vscols_options_done > 0)
  
    updateBoxCollapse(session, "vs_collapse_left", close = "use_cols_vs", open = "factor_cols_vs")
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
